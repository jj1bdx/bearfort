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

test_1(_, 0) -> ok;
test_1(FD, N) ->
    do_test(FD),
    test_1(FD, N-1).

do_test(FD) ->
    ok = serctl:write(FD, <<32>>),
    {ok,
     <<2, 16#51, 16#82,
       DevId:2/little-unsigned-integer-unit:8,
       ADT0:2/little-signed-integer-unit:8,
       A0:2/little-unsigned-integer-unit:8,
       A1:2/little-unsigned-integer-unit:8,
       A2:2/little-unsigned-integer-unit:8,
       A3:2/little-unsigned-integer-unit:8,
       3>>} = serctl:readx(FD, 16, 500),
    {DevId, ADT0, A0, A1, A2, A3}.
