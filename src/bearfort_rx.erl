-module(bearfort_rx).

-export([open/0, close/1, do_test/1, test/1]).

open() ->
    {ok, FD} = serctl:open("/dev/cu.usbmodem1D11311"),
    Termios = lists:foldl(
        fun(Fun, Acc) -> Fun(Acc) end,
        serctl:mode(raw),
        [
            fun(N) -> serctl:flow(N, false) end,
            fun(N) -> serctl:ispeed(N, b9600) end,
            fun(N) -> serctl:ospeed(N, b9600) end
        ]
    ),
    ok = serctl:tcsetattr(FD, tcsanow, Termios),
    {ok, FD}.

close(FD) ->
  serctl:close(FD).
  
test(N) ->
    {ok, FD} = open(),
    test_1(FD, N),
    ok = close(FD).

%%% ((double)1.065) // 1.090/1023*1000
-define(TC1, 1.065).

adt7410_convert(V) ->
    float(V) / 128.0.

lm60_convert(V) ->
    ((float(V) * ?TC1) - 424.0) / 6.25.

test_1(_, 0) -> ok;
test_1(FD, N) ->
    {ok, Output} = do_test(FD),
    {DevId, ADT0, A0, A1, A2, A3} = Output,
    io:format("Device ID:~p~n", [DevId]),
    io:format("ADT7410 output:~p~n", [adt7410_convert(ADT0)]),
    io:format("LM60 values:~p~n", [[lm60_convert(V) || V <- [A0, A1, A2, A3]]]),
    timer:sleep(500),
    test_1(FD, N-1).

read_serial(FD) ->
    ok = serctl:write(FD, <<32>>),
    case serctl:readx(FD, 16, 500) of
        {error, _} -> read_serial(FD);
        {ok, Data} -> {ok, Data}
    end.

do_test(FD) ->
    {ok,
     <<2, 16#51, 16#82,
       DevId:2/little-unsigned-integer-unit:8,
       ADT0:2/little-signed-integer-unit:8,
       A0:2/little-unsigned-integer-unit:8,
       A1:2/little-unsigned-integer-unit:8,
       A2:2/little-unsigned-integer-unit:8,
       A3:2/little-unsigned-integer-unit:8,
       3>>} = read_serial(FD),
    {ok, {DevId, ADT0, A0, A1, A2, A3}}.
