#!/bin/bash
cd $(dirname $0)
SUPPORT_DIR="$PWD"
SRC_DIR="$SUPPORT_DIR/../src"
EBIN_DIR="$SUPPORT_DIR/../ebin"
cd "$SRC_DIR"
escript "$SUPPORT_DIR/run_tests.escript" "$EBIN_DIR" true
