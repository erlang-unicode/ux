%%% @private
-module(ux_uca_sort_key_uncompressed).
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
    lists:reverse(do_sort_key1(MaxLvl, W, A, R, K)).

%% @param S::integer()  Strength (Max level)
%% @param W::[binary()] Weights
%% @param D::fun()      Ducet
%% @param A::fun()      Altername
%% @param R::[[int()]]  Remain weights
%% @param K::[int()]    Result key
do_sort_key1(S, [WH|WT], A, R, K) ->
    {NewA, Ints} = do_alt(A, WH),
    case Ints of
    [0|Rem] ->
        do_sort_key1(S, WT, NewA, [Rem|R], K);
    [L1|Rem] ->
        do_sort_key1(S, WT, NewA, [Rem|R], [L1|K]);
    _ ->
        do_sort_key1(S, WT, NewA, R, K)
    end;
do_sort_key1(S, [], A, R, K) 
    when (S > 1) ->
    W = lists:reverse(R),
    L = 2, % Level
    WL = [],
    do_sort_key2(S, L, W, [0|K]);
do_sort_key1(S, [], A, R, K) ->
    K. % Return result

%% L::int() Level
%% WL::[int()] Weigth on level L
do_sort_key2(S, L, W, K) ->
    {LvlW, NewW} = split_levels(W),
    NewK = lists:reverse(LvlW, K),
    case NewW of
    []    -> NewK;
    _ when S=<L 
          -> NewK;
    [_|_] -> do_sort_key2(S, L+1, NewW, [0|NewK])
    end.

