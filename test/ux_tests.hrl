-include("../src/ux.hrl").

-define(TO(X), {'timeout', 60, X}).
-define(_testTO(X), 
    ?TO(?_test(X))).
-define(_assertTO(X), 
    ?TO(?_assert(X))).
-define(_assertEqualTO(X, Y), 
    ?TO(?_assertEqual(X,Y))).
