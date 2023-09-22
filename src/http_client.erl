%%% Simple tiny HTTP client, parses connection-close responses only.
-module(http_client).
-export([init_parse_response/0, parse_response/2]).


-type context() :: term().

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
