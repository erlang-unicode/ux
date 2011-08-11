-module(ux_range).
-export([in_list/2,search/2]).

-spec in_list([{integer(), integer()} | integer()], integer()) -> 
    boolean().
in_list([{From, To}|T], V) when V >= From, V =< To ->
    true;
in_list([H|T], V) when V =:= H ->
    true;
in_list([H|T], V) ->
    in_list(T, V);
in_list([], _V) -> false.

-spec search([{{integer(), integer()} | integer(), term()}], integer()) -> 
    boolean().
search([{{From, To},P}|T], V) when V >= From, V =< To ->
    P;
search([{H,P}|T], V) when V =:= H ->
    P;
search([H|T], V) ->
    search(T, V);
search([], _V) -> false.
