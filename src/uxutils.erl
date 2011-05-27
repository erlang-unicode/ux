-module(uxutils).
-export([is_always_true/1,
        is_always_false/1,
        nope/1]).

is_always_true(_)  -> true.

is_always_false(_) -> false.

nope(X) -> X.

