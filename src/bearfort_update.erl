-module(bearfort_update).
-export([update/0]).

update() ->
    Hex = stk500:hex_file("./arduino-uno/bearfort-arduino.hex"),
    Bytes = stk500:chunk(Hex, 128),
    {ok,FD} = stk500:open("/dev/cu.usbmodem1D11311", [{speed, b115200}]),
    ok = stk500:load(FD, Bytes).
