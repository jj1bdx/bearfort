#!/bin/sh
yaws --conf ./yaws.conf \
    --pa ../deps/srly/ebin \
    --pa ../deps/stk500/ebin \
    --pa ../ebin
