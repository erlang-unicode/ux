%%% @private
-module(ux_uca_compress).
-export([reassign_fun/3]).


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
    error_logger:info_msg(
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
        , 
        [?MODULE, Lvl,
         Common, Min, Max, Bound, 
         GapSize, TopSize, BottomSize,
         MaxBottom, MinTop]),

%% If a synthetic high weight would be less than BOUND, use a 
%% sequence of high weights of the form (BOUND)..(BOUND)(MAXBOTTOM - 
%% remainder).
%%
%% If a synthetic low weight would not be less than BOUND, use a sequence 
%% of low weights of the form (BOUND-1)..(BOUND-1)(MINTOP + remainder) to 
%% express the length of the sequence.
%%    

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

    SynFn = fun(Cnt, List) when Cnt < Bound -> 
                {Remainder, NewList} = do_seq(Bound, Cnt, List),
                [(MaxBottom - Remainder) | NewList];
               (Cnt, List) -> 
                {Remainder, NewList} = do_seq(Bound - 1, Cnt, List),
                [(MinTop + Remainder) | NewList]
            end,
        
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

do_reassign(Common, SynFn, RaFn, [W|WT], R) 
    when (W =:= Common) -> 
    {Cnt, NewWT} = do_common(W, WT, 1),
    NewR = SynFn(Cnt, R),
    do_reassign(Common, SynFn, RaFn, NewWT, NewR) ;
do_reassign(Common, SynFn, RaFn, [W|WT], R) ->
    NewW = RaFn(W),
    do_reassign(Common, SynFn, RaFn, WT, [NewW|R]);
do_reassign(_Common, _SynFn, _RaFn, []=_W, R) ->
    lists:reverse(R).
    

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
    {Max + 1, Max + 1}.
%get_common_value(_L = 4) -> 16#FFFF.

get_new_max(X) when X<240 -> 16#FF;
get_new_max(X) when X<65530 -> 16#FFFF;
get_new_max(X) -> 16#FFFFFF.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


do_seq_test_() ->
    F = fun do_seq/3,
    [?_assertEqual(F(10, 20, []), {0, [10,10]})
    ].

-endif.
