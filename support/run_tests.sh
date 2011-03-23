#!/bin/bash
cd `dirname $0`
EBIN="./../ebin"
escript run_tests.escript          "$EBIN"
