%%% Simple tiny HTTP client, parses connection-close responses only.
-module(http_client).
-export([get/3]).

-export([init_parse_response/0, parse_response/2]).

-type context() :: term().

%% Simple GET - assumes Connection: close from the server.
get(Ip, Port, Path) ->
    erlang:display({connecting, Ip, Port}),
    Parent = self(),
    Pid = spawn(fun() -> do_get(Parent, Ip, Port, Path) end),
    MRef = erlang:monitor(process, Pid),
    receive
        {Pid, Res} ->
            Res;
        {'DOWN', MRef, _, _, _} ->
            {error, down}
    end.

do_get(Parent, Ip, Port, Path) ->
    case gen_tcp:connect(Ip, Port, [{timeout, 5000}, {active, true}, list]) of
        {ok, Sock} ->
            erlang:display({connected, Ip, Port}),
            try
                Packet = [<<"GET ">>, Path, <<" HTTP/1.0\r\n\r\n">>],
                case gen_tcp:send(Sock, Packet) of
                    ok ->
                        erlang:display(send_ok),
                        recv(Parent, Sock);
                    Error ->
                        erlang:display({send_error, Error}),
                        Error
                end
            after
                gen_tcp:close(Sock)
            end;
        Error ->
            erlang:display({connect_error, Ip, Port, Error}),
            Error
    end.

recv(Parent, Sock) ->
    Ctx = init_parse_response(),
    recv(Parent, Sock, Ctx).

recv(Parent, Sock, Ctx0) ->
    receive
        {tcp, Sock, Data} ->
            case parse_response(Ctx0, Data) of
                {need_more, Ctx1} ->
                    recv(Parent, Sock, Ctx1);
                {response, "200", _, RevBodyPart} ->
                    erlang:display({body, lists:reverse(RevBodyPart)}),
                    recv_rest(Parent, Sock, lists:reverse(RevBodyPart));
                Error ->
                    Error
            end;
        {tcp_closed, Sock} ->
            Parent ! {self(), {error, closed}};
        {tcp_error, Sock, Reason} ->
            Parent ! {self(), {error, Reason}}
    after
        2000 ->
            Parent ! {self(), {error, timeout}}
    end.

recv_rest(Parent, Sock, Acc) ->
    receive
        {tcp, Sock, Data} ->
            recv_rest(Parent, Sock, Acc ++ Data);
        {tcp_closed, Sock} ->
            Parent ! {self(), {ok, Acc}}
    after
        2000 ->
            Parent ! {self(), {error, timeout}}
    end.

-spec init_parse_response() ->
          context().
init_parse_response() ->
    {in_http_version, [], []}.

-spec parse_response(context(), Chars :: list()) ->
          {response,
           StatusCode :: list(),
           ReasonPhrase :: list(), % possibly ""
           RevBodyPart :: list() % possibly ""
          }
        | {need_more, context()}
        | {error, {Chars :: list(), context()}}.
%% The function returns when some part (may be empty) of the body has
%% been parsed.  This means that the caller should take the returned
%% `RevBodyPart` and append any other data received to it.
parse_response({in_http_version, _Acc, Data},
               [$\s | T]) ->
    %% ignore http version
    parse_response({in_status_code, [], Data}, T);
parse_response({in_http_version, Acc, Data},
               [_C | T]) ->
    %% ignore http version
    parse_response({in_http_version, Acc, Data}, T);

parse_response({in_status_code, Acc, Data},
               [$\s | T]) ->
    parse_response({in_reason_phrase, [],
                    [lists:reverse(Acc) | Data]}, T);
parse_response({in_status_code, Acc, Data}, [C | T]) ->
    parse_response({in_status_code, [C | Acc], Data}, T);

parse_response({in_reason_phrase, Acc, Data}, [$\r | T]) ->
    parse_response({{waiting_lf, wait_header}, [],
                    [lists:reverse(Acc) | Data]},
                   T);
parse_response({in_reason_phrase, Acc, Data},
               [C | T]) ->
    parse_response({in_reason_phrase, [C | Acc], Data}, T);

parse_response({wait_header, _Acc, Data},
               [$\r | T]) ->
    parse_response({{waiting_lf, wait_body}, [], Data}, T);
parse_response({wait_header, Acc, Data},
               [_C | T]) ->
    parse_response({in_header, Acc, Data}, T);

parse_response({in_header, _Acc, Data},
               [$\r | T]) ->
    parse_response({{waiting_lf, wait_header}, [], Data}, T);
parse_response({in_header, Acc, Data},
               [_C | T]) ->
    parse_response({in_header, Acc, Data}, T);

parse_response({wait_body, Acc, Data},
               [C | T]) ->
    parse_response({wait_body, [C | Acc], Data}, T);
parse_response({wait_body, Acc, Data},
               []) ->
    [StatusReasonPhrase, StatusCode] = Data,
    {response, StatusCode, StatusReasonPhrase, Acc};

parse_response({{waiting_lf, NextState}, [], Data},
               [$\n | T]) ->
    parse_response({NextState, [], Data}, T);

parse_response(StateAndData, []) ->
    {need_more, StateAndData};

parse_response(StateAndData, Chars) ->
    {error, {Chars, StateAndData}}.
