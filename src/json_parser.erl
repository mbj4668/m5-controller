%%% Small and simple JSON parser, RFC8259 compatible
-module(json_parser).
-export([parse/1]).

-define(isint(C), ((C) >= $0 andalso (C) =< $9)).
-define(isexp(C), ((C) == $e orelse (C) == $E)).
-define(issign(C), ((C) == $+ orelse (C) == $-)).
-define(isws(C), ((C) == $\s orelse (C) == $\t
                  orelse (C) == $\r orelse (C) == $\n)).

-spec parse(list()) -> {ok, term()} | {error, term()}.
%% All errors end up as `{error, term()}`.
%% Strings are not un-escaped; this must be done by the client.
parse(Chars) ->
    try parse_value(skip_ws(Chars)) of
        {Value, _} ->
            {ok, Value}
    catch
        _:E ->
            {error, E}
    end.

parse_value([$" | T])               -> parse_str(T, []);
parse_value([$- | T])               -> parse_number(T, [$-]);
parse_value([C | T]) when ?isint(C) -> parse_number(T, [C]);
parse_value([${ | T])               -> parse_object(skip_ws(T));
parse_value([$[ | T])               -> parse_array(skip_ws(T));
parse_value("true" ++ T)            -> {true, T};
parse_value("false" ++ T)           -> {false, T};
parse_value("null" ++ T)            -> {null, T}.

parse_str([$" | T], Acc)      -> {lists:reverse(Acc), T};
parse_str([$\\, C | T], Acc) ->
    if C == $" orelse C == $\\ orelse C == $/ -> parse_str(T, [C | Acc]);
       C == $b ->  parse_str(T, [$\b | Acc]);
       C == $f ->  parse_str(T, [$\f | Acc]);
       C == $n ->  parse_str(T, [$\n | Acc]);
       C == $r ->  parse_str(T, [$\r | Acc]);
       C == $t ->  parse_str(T, [$\t | Acc]);
       C == $u ->  parse_escaped_unicode(T, Acc)
    end;
parse_str([C | T], Acc)       -> parse_str(T, [C | Acc]).

-ifndef(NO_LIST_TO_INTEGER_2). % not in AtomVM
parse_escaped_unicode([A,B,C,D | T1], Acc) ->
    case list_to_integer([A,B,C,D], 16) of
        High when 16#D800 =< High, High =< 16#DBFF ->
            [$\\,$u,E,F,G,H | T2] = T1,
            Low =  list_to_integer([E,F,G,H], 16), % no validation on low range
            <<Ch/utf16>> = <<High:16, Low:16>>,
            parse_str(T2, [Ch | Acc]);
        Ch ->
            parse_str(T1, [Ch | Acc])
    end.
-else.
parse_escaped_unicode(T, Acc) -> % in this case don't unescape
    parse_str(T, [$u | Acc]).
-endif.

parse_number([C | T], Acc) when ?isint(C) -> parse_number(T, [C | Acc]);
parse_number([$. | T], Acc)               -> parse_float(T, [$. | Acc]);
parse_number([C | T], Acc) when ?isexp(C) -> parse_float(T, [C, $0, $. | Acc]);
parse_number(T, Acc) -> {list_to_integer(lists:reverse(Acc)), T}.

parse_float([C | T], Acc) when ?isint(C) orelse ?isexp(C) orelse ?issign(C) ->
    parse_float(T, [C | Acc]);
parse_float(T, Acc) ->
    {list_to_float(lists:reverse(Acc)), T}.

parse_object([$} | T]) -> {#{}, T};
parse_object(T)        -> parse_object(T, #{}).

parse_object([$" | T], Acc0) ->
    {Field, T2} = parse_str(T, []),
    [$: | T3] = skip_ws(T2),
    {Val, T4} = parse_value(skip_ws(T3)),
    Acc1 = Acc0#{Field => Val},
    case skip_ws(T4) of
        [$, | T5] ->
            parse_object(skip_ws(T5), Acc1);
        [$} | T5] ->
            {Acc1, T5}
    end.

parse_array([$] | T]) -> {[], T};
parse_array(T)        -> parse_array(T, []).

parse_array(T, Acc0) ->
    {Value, T2} = parse_value(skip_ws(T)),
    Acc1 = [Value | Acc0],
    case skip_ws(T2) of
        [$] | T3] -> {lists:reverse(Acc1), T3};
        [$, | T3] -> parse_array(skip_ws(T3), Acc1)
    end.

skip_ws([C | T]) when ?isws(C) -> skip_ws(T);
skip_ws(T) -> T.
