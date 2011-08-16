%%
%% Helpers
%%     
-spec do_alt(fun(), binary()|integer()) -> [integer()].
-spec get_ducet() -> fun().
-spec do_extract(#uca_options{}, string(), fun()) -> 
        {integer(), string()}.
-spec get_options() -> #uca_options{}.

do_alt(A, El) -> A(El).
do_alt(A, El, S) -> listsLsublist(A(El), S).

get_ducet() -> ux_unidata:ducet(skip_check).

%% Extract a weight from the string.
%% Weights is [<<L1, L2, L3, L4>>, <<L1, L2, L3, L4>>].
do_extract(C, S, D) ->
    {_Weights, _NewS} = ux_uca_extract:extract(C, D, S).

get_options() -> #uca_options{}.
    

%%
%% Sort Key Functions
%%

split_levels(W) -> do_split_levels(W, [], []).
do_split_levels([WH|WT], Res, Rem) ->
    case WH of
    [0] -> do_split_levels(WT, Res, Rem);
    [0|T] -> do_split_levels(WT, Res, [T|Rem]);
    [H] -> do_split_levels(WT, [H|Res], Rem);
    [H|T] -> do_split_levels(WT, [H|Res], [T|Rem])
    end;
do_split_levels([], Res, Rem) ->
    {lists:reverse(Res), lists:reverse(Rem)}.
    
get_reassign_function(D, L) ->
    D({reassign_function, L}).
