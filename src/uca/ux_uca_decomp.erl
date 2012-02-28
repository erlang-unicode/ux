-module(ux_uca_decomp).
-export([compute/2]).

-import(ux_uca_utils, [
    get_ducet/0]).

-include("ux.hrl").
-include("ux_uca.hrl").

compute(Char, List) ->
    F = get_ducet(),
    L3 = ux_unidata:tertiary_weight(Char),
    L3Max = 16#001F,
    cycle(1, List, F, L3, L3Max, []).
    

cycle(Pos, [H|T], Ducet, Value, Max, Acc) ->

    %% Set the first two L3 values to be lookup (L3), where the lookup function 
    %% uses the table in Section 7.2, Tertiary Weight Table. Set the remaining 
    %% L3 values to MAX (which in the default table is 001F). 
    NewL3 = if Pos>2 -> Max;
                true -> Value end,

    %% TODO: what to do when a weight is from more then 1 element? :)

    %% H is a code point.
    Weight = Ducet([H]),
    Weight =:= other andalso erlang:throw({bad_char, H}),
    NewAcc = fill_l3(Weight, NewL3, Acc),
    cycle(Pos+1, T, Ducet, Value, Max, NewAcc);

cycle(_Pos, [], _Ducet, _Value, _Max, Acc) -> 
    ux_unidata_parser_allkeys:el_to_bin(lists:reverse(Acc)).


fill_l3([H|T], NewL3, Acc) ->
    [Var, L1, L2, _L3, L4] = H,
    NewH = [Var, L1, L2, NewL3, L4],
    fill_l3(T, NewL3, Acc);
fill_l3([], _NewL3, Acc) -> Acc.
