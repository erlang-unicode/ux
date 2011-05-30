#!/bin/sh
cd `dirname $0`
make
exec erl -pa $PWD/ebin $PWD/deps/*/ebin \
    -sname ux \
    -boot start_sasl \
    -s reloader -s ux
