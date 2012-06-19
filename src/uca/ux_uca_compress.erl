-module(ux_uca_compress).
-export([reassign_fun/3]).
-include("ux.hrl").


%% Return a function for each level (1 - 4).
reassign_fun(_Lvl=1, _Min, Max) ->
    Len = get_bits_len(Max),
    fun({to_binary, W}) -> <<<<X:Len>> || X <- W>> end;
reassign_fun(Lvl, Min, OldMax) ->
    {Common, Max} = get_common_value(Lvl, OldMax),
    TopSize = Max - Common,
    BottomSize = Common - Min,
    NewMin = 1,
    NewMax = get_new_max(Max),
    MaxBottom = NewMax - TopSize,
    MinTop = NewMin + BottomSize,
    GapSize = MaxBottom - MinTop,
    true = (GapSize > 0),
   
    Bound = MaxBottom + (GapSize div 2),
    ?DBG(
        "~w:reassign_fun: Level ~w.~n"
        "   COMMON   is ~w. ~n" 
        "   MIN      is ~w. ~n" 
        "   MAX      is ~w. ~n" 
        "   BOUND    is ~w. ~n" 
        "   GAP_SIZE is ~w. ~n" 
        "   TOP_SIZE is ~w. ~n" 
        "   BOT_SIZE is ~w. ~n" 
        "   MAX_BOT  is ~w. ~n" 
        "   MIN_TOP  is ~w. ~n" 
        "   OLD_MAX  is ~w. ~n" 
        , 
        [?MODULE, Lvl,
         Common, Min, Max, Bound, 
         GapSize, TopSize, BottomSize,
         MaxBottom, MinTop, OldMax]),

%% Reassign the weights in the collation element table at level n to create
%% a gap of size GAP above COMMON. Typically for secondaries or tertiaries 
%% this is done after the values have been reduced to a byte range by the 
%% above methods. Here is a mapping that moves weights up or down to create 
%% a gap in a byte range.
%% w -> w + 01 - MIN, for MIN <= w < COMMON
%% w -> w + FF - MAX, for COMMON < w <= MAX
    RaFn = fun(W) when W < Common -> W + NewMin - Min;
              (W) when W > Common -> W + NewMax - Max
           end,

%% If a synthetic high weight would be less than BOUND, use a 
%% sequence of high weights of the form (BOUND)..(BOUND)(MAXBOTTOM - 
%% remainder).
    SynFn = fun(high, SynWeight, List) when SynWeight < Bound -> 
                {Remainder, NewList} = do_seq(Bound, SynWeight, List),
                [(MaxBottom - Remainder) | NewList];

%% If a synthetic low weight would not be less than BOUND, use a sequence 
%% of low weights of the form (BOUND-1)..(BOUND-1)(MINTOP + remainder) to 
%% express the length of the sequence.
               (low, SynWeight, List) -> 
                {Remainder, NewList} = do_seq(Bound - 1, SynWeight, List),
                [(MinTop + Remainder) | NewList]
            end,
        
%% When generating a sort key, look for maximal sequences of m COMMON values 
%% in a row. Let W be the weight right after the sequence.

    Capacity = Max - Min,
    Result = [],

    Len = get_bits_len(Max),
    fun({to_binary, W}) -> <<<<X:Len>> || X <- W>>;
       (get_common_value) 
            -> Common; % for ux_uca_alt
       (Weights) -> do_reassign(Common, SynFn, RaFn, Weights, Result)
    end.
        
do_seq(Val, Rem, List) when Rem >= Val ->
    do_seq(Val, Rem - Val, [Val|List]);
do_seq(Val, Rem, List) ->
    {Rem, List}.
 

get_bits_len(Max) ->
    if
    Max =< 16#FF     -> 8;
    Max =< 16#FFFF   -> 16;
    Max =< 16#FFFFFF -> 24 
    end.

%% @param W:[int()] List of weights on this lvl.
%% @param R:[int()] Reversed list of resulted weights.
%% @param Cnt:int() Count of repeated Commons.
%% The last step is a bit too simple, because the synthetic weights must 
%% not collide with other values having long strings of COMMON weights. 
%% This is done by using a sequence of synthetic weights, absorbing as 
%% much length into each one as possible. 
%% A value BOUND is defined between MINTOP and MAXBOTTOM. 
%% The exact value for BOUND can be chosen based on the expected 
%% frequency of synthetic low weights versus high weights for the 
%% particular collation element table.


%% When generating a sort key, look for maximal sequences of 
%% m (Cnt) COMMON values in a row. 
do_reassign(Common, SynFn, RaFn, [W|WT], R) 
    when (W =:= Common) -> 
    {Cnt, NewWT} = do_common(W, WT, 1),
    Type = syn_weight_type(NewWT, Common),
    NewR = SynFn(Type, Cnt, R),
    do_reassign(Common, SynFn, RaFn, NewWT, NewR) ;
do_reassign(Common, SynFn, RaFn, [W|WT], R) ->
    NewW = RaFn(W),
    do_reassign(Common, SynFn, RaFn, WT, [NewW|R]);
do_reassign(_Common, _SynFn, _RaFn, []=_W, R) ->
    lists:reverse(R).
    

%% The parameter is the tail of the string after the sequence.
%%
%% Let W be the weight right after the sequence. 
%% If W < COMMON (or there is no W), replace the sequence by a synthetic 
%%                low weight equal to (MINTOP + m).
%% If W > COMMON, replace the sequence by a synthetic high weight equal 
%%                to (MAXBOTTOM - m).
syn_weight_type([W|_], Common) when W > Common -> 
    high;
syn_weight_type(_Str, _Common) -> 
    low.


%% Count of repeated the common weights (W).
do_common(W, [WH|WT], Cnt) when WH=:=W ->
    do_common(W, WT, Cnt+1);
do_common(_W, WT, Cnt) ->
    {Cnt, WT}.

get_common_value(_L = 2, Max) -> 
    {32, Max};
get_common_value(_L = 3, Max) -> 
    {2, Max};
get_common_value(_L = 4, Max) -> 
    {Max+1, Max}.
%get_common_value(_L = 4) -> 16#FFFF.

get_new_max(X) when X<240 -> 16#FF;
get_new_max(X) when X<65530 -> 16#FFFF;
get_new_max(_X) -> 16#FFFFFF.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


do_seq_test_() ->
    F = fun do_seq/3,
    [?_assertEqual(F(10, 20, []), {0, [10,10]})
    ].

%% [0,0,0,4189], [0,0,0,4189,606]

cmp(X, Y) when X < Y -> '<';
cmp(X, X) -> '=';
cmp(_, _) -> '>'.

-define(_assertLower(X, Y), 
        {unicode:characters_to_list(io_lib:format("Is ~w < ~w?", [X, Y])), 
         ?_assertEqual([X, '<', Y], [X, cmp(X, Y), Y])}). 


binarize(Fn) ->
    fun(Val) ->
        Fn({to_binary, Fn(Val)})
        end.

lvl4_test_() ->
    Fn2 = binarize(reassign_fun(2, 0, 221)),
    Fn4 = binarize(reassign_fun(4, 0, 65501)),
    [?_assertLower(Fn2([97,124]),   Fn2([124])) % DATA1
    ,?_assertLower(Fn4([4189]),     Fn4([4189, 606]))
    ,?_assertLower(Fn4([65500,644,65500]),     Fn4([65500,65500]))
    ,?_assertLower(Fn4([65501,644,65501]),     Fn4([65501,65501]))
    ].


%% DATA1:
%% Error (key): [8427,820] greater [820,1425]
%%  Key1: <<0,0,158,131,0,224>>
%%  Key2: <<0,0,158,0,225>>   
%%  Arr1: [[non_variable,0,97,2,8427],[non_variable,0,124,2,820]]
%%  Arr2: [[non_variable,0,124,2,820],[non_variable,0,0,0,1425]]
%% Error in the compression algorithm.
%%  Unzip Key1: [0,97,124,0,2,2]
%%  Unzip Key2: [0,124,0,2]   
%% sort_key and compare returns different results.
%%  Data1: 20EB 0334
%%  Data2: 0334 0591
%%
%%  Result (it is from eunit's output):
%%  ux_uca_compress:162: lvl4_test_ (Is <<131,158>> < <<158>>?)...[0.001 s] ok

 
%% ux_uca_compress:reassign_fun: Level 2.
%%    COMMON   is 32.
%%    MIN      is 0.
%%    MAX      is 221.
%%    BOUND    is 82.
%%    GAP_SIZE is 33.
%%    TOP_SIZE is 189.
%%    BOT_SIZE is 32.
%%    MAX_BOT  is 66.
%%    MIN_TOP  is 33.
%%    OLD_MAX  is 221.
%% 
%% =INFO REPORT==== 19-Jun-2012::13:41:39 ===
%% ux_uca_compress:reassign_fun: Level 3.
%%    COMMON   is 2.
%%    MIN      is 0.
%%    MAX      is 31.
%%    BOUND    is 337.
%%    GAP_SIZE is 223.
%%    TOP_SIZE is 29.
%%    BOT_SIZE is 2.
%%    MAX_BOT  is 226.
%%    MIN_TOP  is 3.
%%    OLD_MAX  is 31.
%% 
%% =INFO REPORT==== 19-Jun-2012::13:41:39 ===
%% ux_uca_compress:reassign_fun: Level 4.
%%    COMMON   is 65502.
%%    MIN      is 0.
%%    MAX      is 65502.
%%    BOUND    is 65551.
%%    GAP_SIZE is 32.
%%    TOP_SIZE is 0.
%%    BOT_SIZE is 65502.
%%    MAX_BOT  is 65535.
%%    MIN_TOP  is 65503.
%%    OLD_MAX  is 65501.

-endif.
