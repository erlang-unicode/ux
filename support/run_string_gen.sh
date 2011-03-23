#!/bin/bash
cd `dirname $0`
EBIN="./../ebin"
IN="./../priv/UNIDATA/"
OUT="./../src/string/"
escript string_gen.escript          "$EBIN" "$IN" "$OUT"
escript comp_exclusions_gen.escript "$EBIN" "$IN" "$OUT"
escript norm_props_gen.escript      "$EBIN" "$IN" "$OUT"
