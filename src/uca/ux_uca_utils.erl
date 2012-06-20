%% Contains private common functions.
-module(ux_uca_utils).
-export([
    do_alt/2, 
    do_alt/3, 
    do_extract/3, 
    get_ducet/0, 
    get_options/0, 
    split_levels/3, 
    get_reassign_function/2]).

%% For debugging only
-export([hangul_type/1,
         implicit_type/1]).

-include("ux.hrl").
-include("ux_uca.hrl").


hangul_type(X) when ?IS_L1_OF_HANGUL_L(X) -> l;
hangul_type(X) when ?IS_L1_OF_HANGUL_V(X) -> v;
hangul_type(X) when ?IS_L1_OF_HANGUL_T(X) -> t;
hangul_type(_) -> x.


implicit_type(X) when ?CHAR_IS_UNIFIED_IDEOGRAPH(X) ->
    if (?CHAR_IS_CJK_COMPATIBILITY_IDEOGRAPH(X)
        or ?CHAR_IS_CJK_UNIFIED_IDEOGRAPH(X)) ->
        base1;
     true ->
        base2
    end;
implicit_type(_) ->
    base3.

%%
%% Helpers
%%     

-spec do_alt(fun(), binary()|integer()) -> [integer()].
do_alt(A, W) -> A(W).
do_alt(A, W, S) -> 
    {NewA, AltW} = A(W),
    NewAltW = lists:sublist(AltW, S),
    {NewA, NewAltW}.

-spec get_ducet() -> fun().
get_ducet() -> ux_unidata:ducet(skip_check).

-spec do_extract(#uca_options{}, string(), fun()) -> 
        {integer(), string()}.
%% Extract a weight from the string.
%% Weights is [<<L1, L2, L3, L4>>, <<L1, L2, L3, L4>>].
do_extract(C, S, D) ->
    {_Weights, _NewS} = ux_uca_extract:extract(C, D, S).

-spec get_options() -> #uca_options{}.
get_options() -> #uca_options{}.
    

%%
%% Sort Key Functions
%%

% L2 is backward.
-spec split_levels(integer(), boolean(), [[integer()]]) -> 
        {[integer()], [[integer()]]}.

split_levels(_L=2, _B=true, W) -> 
    {Res, Rem} = do_split_levels(W, [], []),
    {Res, lists:reverse(Rem)};
split_levels(_L, _B, W) -> 
    {Res, Rem} = do_split_levels(W, [], []),
    {lists:reverse(Res), lists:reverse(Rem)}.
    
do_split_levels([WH|WT], Res, Rem) ->
    case WH of
    [0] -> do_split_levels(WT, Res, Rem);
    [0|T] -> do_split_levels(WT, Res, [T|Rem]);
    [H] -> do_split_levels(WT, [H|Res], Rem);
    [H|T] -> do_split_levels(WT, [H|Res], [T|Rem])
    end;
do_split_levels([], Res, Rem) ->
    {Res, Rem}.
    
get_reassign_function(D, L) ->
    D({reassign_function, L}).
