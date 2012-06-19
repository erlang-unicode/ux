-module(ux_uca_sort_key_binary).
-export([sort_key/2]).
-import(ux_uca, [sort_array/2]).

-import(ux_uca_utils, [
    do_alt/2, 
    get_ducet/0, 
    get_options/0, 
    split_levels/3, 
    get_reassign_function/2]).

-include("ux.hrl").
-include("ux_uca.hrl").

sort_key(C=#uca_options{strength=MaxLvl, backwards=B}, S) ->
    W = sort_array(C, S),
    D = get_ducet(),
    A = ux_uca_alt:get_alternate_function(C, D),
    R = [], % Remains
    K = [], % Key
    do_sort_key1(MaxLvl, B, W, D, A, R, K).

%% @param S::integer()  Strength (Max level)
%% @param B
%% @param W::[binary()] Weights
%% @param D::fun()      Ducet
%% @param A::fun()      Altername
%% @param R::[[int()]]  Remain weights
%% @param K::[int()]    Result key
do_sort_key1(S, B, [WH|WT], D, A, R, K) ->
    {NewA, Ints} = do_alt(A, WH),
    case Ints of
    [0|Rem] ->
        do_sort_key1(S, B, WT, D, NewA, [Rem|R], K);
    [L1|Rem] ->
        do_sort_key1(S, B, WT, D, NewA, [Rem|R], [L1|K]);
    _ ->
        do_sort_key1(S, B, WT, D, NewA, R, K)
    end;
do_sort_key1(S, B, [], D, A, R, K) 
    when (S > 1) ->
    W = lists:reverse(R),
    L = 2, % Level
    WL = [],
    NewK = [0|K],
    RevK = lists:reverse(NewK),
    Fn = get_reassign_function(D, 1),
    BinK = Fn({to_binary, RevK}),
    do_sort_key2(S, B, L, W, D, BinK);
do_sort_key1(S, _B, [], D, _A, _R, K) ->
    Fn = get_reassign_function(D, 1),
    RevK = lists:reverse(K),
    Fn({to_binary, RevK}). % Return result

%% L::int() Level
%% WL::[int()] Weigth on level L
do_sort_key2(S, B, L, W, D, K) ->
    {LvlW, RemW} = split_levels(L, B, W),
    Fn = get_reassign_function(D, L),
    ReassignW = Fn(LvlW),
    case RemW of
    _ when RemW=:=[]; S=<L -> 
        BinW = Fn({to_binary, ReassignW}),
        <<K/binary, BinW/binary>>;

    [_|_] -> 
        % Add a delimeter.
        BinW  = Fn({to_binary, ReassignW}),
        Delim = Fn({to_binary, [0]}),
        NewK  = <<K/binary, BinW/binary, Delim/binary>>,

        do_sort_key2(S, B, L+1, RemW, D, NewK)
    end.

