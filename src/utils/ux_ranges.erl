%% @doc Functions for working with ranges in lists.
%%
%%      ETS is fast only as a key-value store.
%%      But some data files contains ranges: From..To.
%%      The fastest way is using lists for storing this values.
%%
%%      There is two types of these lists:
%%      * with booleans: `[{1,3}, 6, {8,9}]'. For example, `is_compat';
%%      * with values: `[{{1,3}, value1}, {{4,12}, value2}]'.
%%
%%      `in_list' function is for the first type.
%%      `search' function is for the second type.
%%
%% @end

-module(ux_ranges).
-export([in_list/2,search/2]).

-spec in_list([{integer(), integer()} | integer()], integer()) -> 
    boolean().
in_list([H|T], H) 
    when is_integer(H) ->
    true;
in_list([{From, To}|T], V) 
    when V >= From, V =< To ->
    true;
in_list([H|T], V) ->
    in_list(T, V);
in_list([], _V) -> false.

-spec search([{{integer(), integer()} | integer(), term()}], integer()) -> 
    boolean().
search([{H,P}|T], H) 
    when is_integer(H) ->
    P;
search([{{From, To},P}|T], V) 
    when V >= From, V =< To ->
    P;
search([_H|T], V) ->
    search(T, V);
search([], _V) -> false.


