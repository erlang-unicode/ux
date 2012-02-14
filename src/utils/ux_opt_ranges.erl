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

-module(ux_opt_ranges).
-export([in_list/1, search/2]).


in_list([H|_]=V) ->
    SortedV = in_list_sort(V),

    R = erlang:list_to_tuple(
        lists:map(fun(X) -> [] end, 
            lists:seq(1,651))),

    do_in_list(V, R).

search(Def, V) ->
    SortedV = search_sort(V),

    R = erlang:list_to_tuple(
        lists:map(fun(X) -> [] end, 
            lists:seq(1,651))),

    do_search(Def, V, R).


do_in_list([{H1,H2}=V|T], R) ->
    I1 = index(H1),
    I2 = index(H2),
    R2 = fill_elem(I1, I2, V, R),
    do_in_list(T, R2);

do_in_list([H1|T], R) ->
    R1 = set_elem(H1, H1, R),
    do_in_list(T, R1);

do_in_list([], R) ->
    L = erlang:tuple_to_list(R),
    ML = lists:map(fun lists:reverse/1, L),
    MR = erlang:list_to_tuple(ML),

    fun(X) ->
        I = index(X),
        MiniList = erlang:element(I, MR),
        ux_ranges:in_list(MiniList, X)
        end.
    
    

% skip
do_search(Def, [{_,Def}|T], R) ->
    do_search(Def, T, R);

do_search(Def, [{{H1,H2},_P}=V|T], R) ->
    I1 = index(H1),
    I2 = index(H2),
    R2 = fill_elem(I1, I2, V, R),
    do_search(Def, T, R2);

do_search(Def, [{H1,_P}=V|T], R) ->
    R1 = set_elem(H1, V, R),
    do_search(Def, T, R1);

do_search(Def, [], R) ->
    L = erlang:tuple_to_list(R),
    ML = lists:map(fun lists:reverse/1, L),
    MR = erlang:list_to_tuple(ML),

    fun(X) ->
        I = index(X),
        MiniList = erlang:element(I, MR),

        case ux_ranges:search(MiniList, X) of
        false -> Def;
        P -> P
        end
    end.

    
    


set_elem(H, V, R) 
    when is_tuple(R) ->
    I = index(H),
    E = erlang:element(I, R),
    erlang:setelement(I, R, [V|E]).

    
set_elem_i(I, V, R) 
    when is_tuple(R) ->
    E = erlang:element(I, R),
    erlang:setelement(I, R, [V|E]).


fill_elem(I, I, V, R) ->
    set_elem_i(I, V, R);
fill_elem(I1, I2, V, R) when I1<I2 ->
    NewR = set_elem_i(I1, V, R),
    NewI1 = I1 + 1,
    fill_elem(NewI1, I2, V, NewR).


index(N) when N > 65000 -> 
    651;
index(N) -> 
    (N div 100) + 1.


in_list_sort(V) ->
    MF = fun({From,To} = Key) -> 
                {From, Key};
            (From) ->
                {From, From}
        end,

    MF2 = fun({_,Key}) -> Key end,

    V1 = lists:map(MF, V),
    V2 = lists:sort(V1),
    lists:map(MF2, V2).
    


search_sort(V) ->
     MF = fun(Key) ->
            case erlang:element(1, Key) of
            ({From,To}) -> 
                {From, Key};
            (From) ->
                {From, From}
            end
        end,

    MF2 = fun({_,Key}) -> Key end,

    V1 = lists:map(MF, V),
    V2 = lists:sort(V1),
    lists:map(MF2, V2).







