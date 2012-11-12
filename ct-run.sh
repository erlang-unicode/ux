#!/bin/sh
cd `dirname $0`
ct_run -spec ux_test.spec -pa $PWD/ebin edit $PWD/deps/*/ebin 
#   -s reloader

