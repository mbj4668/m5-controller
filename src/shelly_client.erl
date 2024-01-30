-module(shelly_client).
-export([read/2, turn/3]).

%% Ret: {ok, true | false}
read(Ip, Port) ->
    get_relay(Ip, Port, "").

turn(Ip, Port, Turn) ->
    get_relay(Ip, Port, [<<"?turn=">>, Turn]).

get_relay(Ip, Port, Extra) ->
    case http_client:get(Ip, Port, ["/relay/0", Extra]) of
        {ok, Body} ->
            erlang:display({recv, Body}),
            case catch mjson_decode:decode(list_to_binary(Body)) of
                {ok, #{<<"ison">> := IsOn}} ->
                    erlang:display({ison, IsOn}),
                    {ok, IsOn};
                Error ->
                    erlang:display({json, Error}),
                    Error
            end;
        Error ->
            erlang:display({recv_error, Error}),
            Error
    end.
