#!/bin/sh
cd `dirname $0`
make
# NOTE: mustache templates need \ because they are not awesome.
exec erl -pa $PWD/ebin edit $PWD/deps/*/ebin -boot start_sasl \
    -sname ux \
    -s ux \
#   -s reloader

