-module(ux_uca).
-author('Uvarov Michael <freeakk@gmail.com>').

-export([
        compare/2,      % Str1, Str2
        compare/3,      % Opt, Str1, Str2
        sort_array/1,   % Str
        sort_array/2,   % Opt, Str
        sort_key/1,     % Str
        sort_key/2,     % Opt, Str
        sort/1,         % Str
        sort/2          % Opt, [Str]
%       get_options/0, 
%       get_options/1, 
%       get_options/2
        ]).

-include("ux.hrl").
-include("ux_uca.hrl").
-include("ux_uca_common.hrl").


compare(S1, S2) ->
    C = get_options(),
    compare(C, S1, S2).

-spec compare(#uca_options{}, string(), string()) -> boolean().
compare(C=#uca_options{}, S1, S2) ->
    G1 = generator(C, S1),
    G2 = generator(C, S2),
    do_compare(G1, G2).

-spec do_compare(fun(), fun()) -> lower | greater | equal.
do_compare(G1, G2) ->
    case {G1(), G2()} of
    {stop, stop} -> equal;
    {stop, ____} -> lower;
    {____, stop} -> upper;
    {{W1, NewG1}, {W2, NewG2}} 
        when W1 =:= W2 ->
        do_compare(NewG1, NewG2);
    {{W1, _NewG1}, {W2, _NewG2}} 
        when W1 < W2 -> lower;
    {{W1, _NewG1}, {W2, _NewG2}} 
        when W1 > W2 -> upper
    end.
    
sort_array(S) ->
    C = get_options(),
    sort_array(C, S).

sort_array(C, S) ->
    W = [],
    A = [],
    D = get_ducet(),
    do_sort_array(C, D, S, W, A).

do_sort_array(_C, _D, []=_S, []=_W, A) ->
    lists:reverse(A);
do_sort_array(C, D, S, []=_W, A) ->
    {NewW, NewS} = do_extract(C, S, D),
    do_sort_array(C, D, NewS, NewW, A);
do_sort_array(C, D, S, [WH|WT], A) ->
    do_sort_array(C, D, S, WT, [WH|A]).
    
sort_key(S) ->
    C = get_options(),
    sort_key(C, S).
    
%% @param C#sort_key_format{}
%% @param S::string()
sort_key(C=#uca_options{
        sort_key_format='binary', 
        case_sensitive=true}, S) ->
    ux_uca_sort_key_binary_cs:sort_key(C, S);
sort_key(C=#uca_options{sort_key_format=F}, S) ->
    case F of
    'binary' ->
        ux_uca_sort_key_binary:sort_key(C, S);
    'list' ->
        ux_uca_sort_key_list:sort_key(C, S);
    'uncompressed' ->
        ux_uca_sort_key_uncompressed:sort_key(C, S) 
    end.


%%
%% Generator
%% 

-spec generator(#uca_options{}, string()) -> fun().
%generator(#uca_options{}, []) -> stop;
generator(C=#uca_options{}, S) ->
    D = get_ducet(),
    A = ux_uca_alt:get_alternate_function(C, D),
    W = [],
    R = [],

    fun() -> do_generator(C, S, D, A, W, R) end.

%% @param C::uca_options{} Configuration
%% @param S::string()   String
%% @param D::fun()      Ducet_reversed function
%% @param A::fun()      Alternate function 
%% @param W::[int()]    ListOfWeights 
%% @param R::[[int()]]  Remain weights
do_generator(#uca_options{strength=S}, []=_S, _D, _A, []=_W, R) ->
    F = fun() -> do_generator2(S-1, lists:reverse(R), []) end,
    {0, F};
do_generator(C, S, D, A, []=_W, R) ->
    {NewW, NewS} = do_extract(C, S, D),
    do_generator(C, NewS, D, A, NewW, R);
do_generator(C, S, D, A, [WH|WT], R) ->
    {NewA, Ints} = do_alt(A, WH),
    case Ints of
    [0] -> % try extract next
        do_generator(C, S, D, NewA, WT, R);
    [L1] when is_integer(L1) -> 
        F = fun() -> 
                do_generator(C, S, D, NewA, WT, R)
            end,
        {L1, F};
    [0|IT] -> % try extract next
        do_generator(C, S, D, NewA, WT, [IT|R]);
    [L1|IT] when is_integer(L1) -> 
        F = fun() -> 
                do_generator(C, S, D, NewA, WT, [IT|R])
            end,
        {L1, F}
    end.

%% @param S::integer() Strength (not string).
do_generator2(0, _W, _R) ->
    stop;
do_generator2(_S, []=_W, []=_R) ->
    stop;
do_generator2(1, []=_W, _R) ->
    stop;
do_generator2(S, []=_W, R) ->
    F = fun() -> 
            do_generator2(S-1, lists:reverse(R), []) 
        end,
    {0, F};
do_generator2(S, [[0=_WH]|WT], R) ->
    do_generator2(S, WT, R);
do_generator2(S, [[0=_WH|WR]|WT], R) ->
    do_generator2(S, WT, [WR|R]);
do_generator2(S, [[WH]|WT], R) ->
    F = fun() -> do_generator2(S, WT, R) end,
    {WH, F};
do_generator2(S, [[WH|WR]|WT], R) ->
    F = fun() -> do_generator2(S, WT, [WR|R]) end,
    {WH, F}.
    

sort(Strings) ->
    C = get_options(),
    sort(C, Strings).

%% @doc Sort a string list.
sort(C, Strings) ->
    
    % Step 1: produce array of sort keys
    F = fun(S) -> 
            Key = sort_key(C, S),
            {Key, S} 
        end,
    Keys = lists:map(F, Strings),

    % Step 2: sort array
    SortedKeys = lists:keysort(1, Keys),

    % Step 3: Return result
    RetFn = fun({_Key, S}) -> S end,
    lists:map(RetFn, SortedKeys).


    


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
split_levels_test_() ->
    F = fun split_levels/1,
    [?_assertEqual(F([[1,2,3],[4,5,6],[7,8,9]]), {[1,4,7], [[2,3],[5,6],[8,9]]})
    ,?_assertEqual(F([[1,2,3],[4],[7,8]]), {[1,4,7], [[2,3],[8]]})
    ].
-endif.
