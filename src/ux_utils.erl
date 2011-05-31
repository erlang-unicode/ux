% vim: set filetype=erlang shiftwidth=4 tabstop=4 expandtab tw=80:
-module(ux_utils).
-export([is_always_true/1,
        is_always_false/1,
        nope/1]).

is_always_true(_)  -> true.

is_always_false(_) -> false.

nope(X) -> X.

