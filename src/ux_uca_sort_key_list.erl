%%% @private
-module(ux_uca_sort_key_list).
-export([sort_key/2]).
-import(ux_uca, [sort_array/2]).

-include("ux.hrl").
-include("ux_uca.hrl").
-include("ux_uca_common.hrl").

sort_key(C=#uca_options{strength=MaxLvl}, S) ->
    W = sort_array(C, S),
    D = get_ducet(),
    A = ux_uca_alt:get_alternate_function(C, D),
    R = [], % Remains
    K = [], % Key
    lists:reverse(do_sort_key1(MaxLvl, W, D, A, R, K)).

%% @param S::integer()  Strength (Max level)
%% @param W::[binary()] Weights
%% @param D::fun()      Ducet
%% @param A::fun()      Altername
%% @param R::[[int()]]  Remain weights
%% @param K::[int()]    Result key
do_sort_key1(S, [WH|WT], D, A, R, K) ->
    {NewA, [L1|Rem]} = do_alt(A, WH),
    do_sort_key1(S, WT, D, NewA, [Rem|R], [L1|K]);
do_sort_key1(S, [], D, A, R, K) 
    when (S > 1) ->
    W = lists:reverse(R),
    L = 2, % Level
    WL = [],
    do_sort_key2(S, L, W, D, [0|K]);
do_sort_key1(S, [], D, A, R, K) ->
    K. % Return result

%% L::int() Level
%% WL::[int()] Weigth on level L
do_sort_key2(S, L, W, D, K) ->
    {LvlW, NewW} = split_levels(W),
    Fn = get_reassign_function(D, L),
    ReassignW = Fn(LvlW),
    NewK = lists:reverse(ReassignW, K),
    case NewW of
    []    -> NewK;
    _ when S=<L 
          -> NewK;
    [_|_] -> do_sort_key2(S, L+1, NewW, D, [0|NewK])
    end.

