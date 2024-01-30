-module(main).
-export([start/0]).

-define(SSID, <<"mbj162">>).
-define(PSK, <<"feedbabeff">>).

-define(SLEEP_AFTER_IDLE_SECONDS, 30).
-define(SLEEP_AFTER_ERROR_SECONDS, 60).

-define(TFT_WHITE, 16#FFFFFF).
-define(TFT_CYAN, 16#00FFFF).
-define(TFT_RED, 16#FF0000).
-define(TFT_YELLOW, 16#FFFF00).
-define(TFT_BLUE, 16#0000FF).
-define(TFT_GREEN, 16#00FF00).
-define(TFT_MAROON, 16#800000).

%-define(SOUND, 1).

-define(CH_å, 134).
-define(CH_Å, 143).
-define(CH_ä, 132).
-define(CH_Ä, 142).
-define(CH_ö, 148).
-define(CH_Ö, 153).

-record(state, {
          shelly_plugs,
          selected
       }).

-define(ON, <<"p", ?CH_å>>).
-define(OFF, <<"av">>).
-define(ERROR, <<"??">>).

shelly_plugs() ->
    [
     {<<"Stora huset">>,            {{192,168,2,10}, 80}}
    , {<<"Sj", ?CH_ö, "stugan">>,   {{192,168,2,11}, 80}}
%     {<<"Stora huset">>,            {{192,168,0,6}, 9010}}
%    , {<<"Sj", ?CH_ö, "stugan">>,   {{192,168,0,6}, 9011}}
%    , {<<"G",?CH_ä, "ststugan">>, {192,168,2,12}}
%    , {<<"Bastun">>,   {192,168,2,13}}
    ].

read_shelly_plugs() ->
    X=lists:map(
      fun({Name, {Ip, Port} = Addr}) ->
              case shelly_client:read(Ip, Port) of
                  {ok, true} ->
                      {Name, Addr, ?ON};
                  {ok, false} ->
                      {Name, Addr, ?OFF};
                  _ ->
                      {Name, Addr, ?ERROR}
              end
      end, shelly_plugs()),
    erlang:display({plugs, X}),
    X.

start() ->
    m5:begin_([{clear_display, true}]),

    case m5_speaker:is_enabled() of
        true ->
            %% set master volume (0~255)
            m5_speaker:set_volume(64),
            %% play beep sound 2000Hz 100msec (background task)
            m5_speaker_tone(2000, 100),
            %% wait until it is done
            wait_until_speaker_is_done(),
            %% play beep sound 1000Hz 100msec (background task)
            m5_speaker_tone(1000, 100),
            %% wait until it is done
            wait_until_speaker_is_done();
        false ->
            ok
    end,

%    m5_display:set_epd_mode(fastest),
    m5_display:set_brightness(128),

    m5_display:set_text_size(3),

    start_network(),

    scene_main(#state{shelly_plugs = read_shelly_plugs()}).

start_network() ->
    Config = [
              {ssid, ?SSID},
              {psk,  ?PSK}
             ],
    case network:wait_for_sta(Config, 15000) of
        {ok, {Address, _Netmask, _Gateway}} ->
            io:format("Acquired IP address: ~p~n", [Address]);
        {error, Reason} ->
            print_error(Reason),
            io:format("Network initialization failed: ~p~n", [Reason])
    end.

scene_main(S) ->
    print_scene(S),
    scene_main_loop(erlang:monotonic_time(second), S).

scene_main_loop(Start, S) ->
    timer:sleep(10),
    m5:update(),
    case {m5_btn_a:was_clicked(), m5_btn_c:was_clicked()} of
        {false, true} ->
            m5_speaker_tone(2000, 100),
            scene_main(S#state{selected = next_row(S#state.selected)});
        {true, false} when S#state.selected /= undefined ->
            m5_speaker_tone(3000, 100),
            scene_set_config(S);
        _ ->
            case erlang:monotonic_time(second) of
                Now when Now - Start > ?SLEEP_AFTER_IDLE_SECONDS ->
                    goto_sleep();
                _ ->
                    scene_main_loop(Start, S)
            end
    end.

scene_set_config(S) ->
    %% m5_display:clear(),
    %% m5_display:start_write(),
    %% print_header(),
    %% print_status(<<"Communicating...">>),
    %% m5_display:end_write(),

    {_, {Ip, Port}, Old} = lists:nth(S#state.selected, S#state.shelly_plugs),
    Turn = case toggle(Old) of
               ?ON -> "on";
               ?OFF -> "off"
           end,
    case shelly_client:turn(Ip, Port, Turn) of
        {ok, _} ->
            %print_ok(),
            %timer:sleep(1000),
            scene_main(S#state{shelly_plugs = read_shelly_plugs()});
        Error ->
            print_error(Error),
            wait_buttton_clicked(erlang:monotonic_time(second)),
            scene_main(S#state{shelly_plugs = read_shelly_plugs()})
    end.

print_error(_Error) ->
    m5_display:clear(),
    m5_display:start_write(),
    m5_display:set_cursor(0, 0),
    m5_display:println(<<"ERROR:">>),
    m5_display:set_text_size(1),
    m5_display:print(io_lib:format("~p", [_Error])),
    m5_display:end_write(),
    m5_display:set_text_size(3).

wait_buttton_clicked(Start) ->
    timer:sleep(10),
    m5:update(),
    case
        m5_btn_a:was_clicked() orelse
        m5_btn_b:was_clicked() orelse
        m5_btn_c:was_clicked()
    of
        true ->
            ok;
        false ->
            case erlang:monotonic_time(second) of
                Now when Now - Start > ?SLEEP_AFTER_ERROR_SECONDS ->
                    goto_sleep();
                _ ->
                    wait_buttton_clicked(Start)
            end
    end.

goto_sleep() ->
    m5_display:clear(),
    m5_display:start_write(),
    print_status(<<"Shutting down...">>),
    m5_display:end_write(),
    timer:sleep(2_000),
    %% wakeup on button A
    ok = esp:sleep_enable_ext0_wakeup(39, 0),
    m5_power:deep_sleep().

toggle(?OFF) -> ?ON;
toggle(_) -> ?OFF.

next_row(undefined) -> 1;
next_row(N) ->
    1 + (N rem length(shelly_plugs())).

print_scene(S) ->
    #state{shelly_plugs = ShellyPlugs, selected = Sel} = S,
    m5_display:clear(),
    m5_display:start_write(),
    print_header(),
    lists:foldl(
      fun({Name, _, Val}, {Row, Idx}) ->
              print_row(Row, Name, Val, Idx == Sel),
              {Row + 2, Idx + 1}
      end, {3, 1}, ShellyPlugs),

    %% button x-coord: 35, 130, 205
    Y = m5_display:height() - m5_display:font_height(),
    if Sel /= undefined ->
            m5_display:set_cursor(15, Y),
            m5_display:print(<<?CH_ä, "ndra">>);
       true ->
            ok
    end,

    m5_display:set_cursor(215, Y),
    m5_display:print(<<"n", ?CH_ä, "sta">>),

    m5_display:end_write().

print_header() ->
    m5_display:set_cursor(70, 0),
    m5_display:println(<<"VARMVATTEN">>).

print_status(Text) ->
    FH = m5_display:font_height(),
    m5_display:set_cursor(20, FH * 5),
    m5_display:println(Text).


print_row(Row, Text, Val, IsSelected) ->
    FH = m5_display:font_height(),
    FW = m5_display:font_width(),
    m5_display:set_cursor(0, FH * Row),
    m5_display:print([Text, $:]),
    if IsSelected ->
            m5_display:draw_round_rect(FW * 12, FH * Row - 8,
                                       FW * erlang:byte_size(Val) + 16, FH + 16,
                                       4, ?TFT_GREEN);
       true ->
            ok
    end,
    m5_display:set_cursor(FW * 12 + 8, FH * Row),
    m5_display:print(Val).

wait_until_speaker_is_done() ->
    case m5_speaker:is_playing() of
        true ->
            timer:sleep(1),
            wait_until_speaker_is_done();
        false ->
            ok
    end.


-ifdef(SOUND).
m5_speaker_tone(Freq, Time) ->
    m5:speaker_tone(Freq, Time).
-else.
m5_speaker_tone(_Freq, _Time) ->
    ok.
-endif.
