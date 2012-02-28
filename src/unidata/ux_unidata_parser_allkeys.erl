%%% Example:
%%% ux_unidata_filelist:get_pid({allkeys, [ducet], code:priv_dir(ux) ++ "/UNIDATA/allkeys.txt"}).
%%% @private
-module(ux_unidata_parser_allkeys).
-include("ux.hrl").
%-include("ux_string.hrl").


-export([parse/1, types/0, get_function/2
    , after_parse/1 % comment to disable post processing
    ]).

%% For ux_uca_decomp 
-export([el_to_bin/1]).


types() -> [ducet].

parse(In) ->
    case ux_unidata_parser:split($;, In) of
    [[_|_] = Code, [_|_] = Element] ->
        InEl = lists:map(fun ux_unidata_parser:hex_to_int/1,
                        string:tokens(Code, " ")),
        OutEl = parse_el(ux_unidata_parser:delete_spaces(Element)),
        %io:format("String: ~ts, From reversed: ~w, To: ~w~n", [In, InEl, OutEl]),

        Res = case InEl of 
            [] -> skip; 
            _ -> {InEl, OutEl} 
        end,
        {ok,
            [{ducet,   Res}
            ]
        };
    _ -> skip
    end.

after_parse(Ets) ->
    do_after(Ets),
    ok.

get_function(ducet, Table) -> 
    % R1 is only for encoding to binary, not reassign.
    R1 = get_reassign_function(Table, 1),

    R2 = get_reassign_function(Table, 2),
    R3 = get_reassign_function(Table, 3),
    R4 = get_reassign_function(Table, 4),
    F = ux_unidata_parser:ets_fun(Table, other),
    LTable = get_val(Table, 'LTable'),
    MTable = get_val(Table, 'MTable'),
    MF = fun(Value) ->
            case ets:member(LTable, Value) of
            true -> true;
            false -> 
                case ets:member(MTable, Value) of
                true -> maybe;
                false -> false
                end
            end
        end,
    
    fun(member_function) -> MF;
       ({reassign_function, 1}) -> R1; % Return fun.
       ({reassign_function, 2}) -> R2; % Return fun.
       ({reassign_function, 3}) -> R3;
       ({reassign_function, 4}) -> R4;
       ([_|_]=Value) -> 
        case F(Value) of
        W when is_binary(W) -> 
            bin_to_list2(W);
        Other -> 
            Other
        end 
      end.

get_reassign_function(Table, Lvl) ->
    [{_, Min}] = ets:lookup(Table, {min, Lvl}),
    [{_, Max}] = ets:lookup(Table, {max, Lvl}),
    ux_uca_compress:reassign_fun(Lvl, Min, Max).

get_val(Table, Val) ->
    [{_, Res}] = ets:lookup(Table, Val),
    Res.






%%
%% Hacks.
%%

do_after([{ducet, Table} | Tail]) ->
    do_after_ranges(Table),
    
    LTable = ets:new(ducet_lookup, []),
    do_after_lookup(Table, LTable),

    MTable = ets:new(ducet_more, []),
    do_after_ducet(Table, MTable),

    ets:insert(Table, {'LTable', LTable}),
    ets:insert(Table, {'MTable', MTable}),

    do_after(Tail);
do_after([_ | Tail]) ->
    do_after(Tail);
do_after([]) -> ok.


%% @doc Add a table that contains all keys of the elements.
do_after_lookup(Table, LTable) ->
%   ets:safe_fixtable(Table, true),
    case ets:first(Table) of
    '$end_of_table' ->
        ok;
    Index when is_list(Index) ->
        add_lookup(LTable, Index),
        do_after_lookup_next(Table, LTable, Index);
    Index ->
        do_after_lookup_next(Table, LTable, Index)
    end.
%   ets:safe_fixtable(Table, false),
    
do_after_lookup_next(Table, LTable, PrevIndex) ->
    case ets:next(Table, PrevIndex) of
    '$end_of_table' ->
        ok;
    Index when is_list(Index) ->
        add_lookup(LTable, Index),
        do_after_lookup_next(Table, LTable, Index);
    Index ->
        do_after_lookup_next(Table, LTable, Index)
    end.

add_lookup(LTable, Index) ->
    Rev = lists:reverse(Index),
    ets:insert(LTable, {Rev}).



%% @doc Add `more' to empty space beetween the elements.
do_after_ducet(Table, MTable) ->
%   ets:safe_fixtable(Table, true),
    case ets:first(Table) of
    '$end_of_table' ->
        ok;
    Index when is_list(Index) ->
        ducet_more(Table, MTable, Index),
        do_after_ducet_next(Table, MTable, Index);
    Index ->
        do_after_ducet_next(Table, MTable, Index)
    end.
%   ets:safe_fixtable(Table, false),
    
do_after_ducet_next(Table, MTable, PrevIndex) ->
    case ets:next(Table, PrevIndex) of
    '$end_of_table' ->
        ok;
    Index when is_list(Index) ->
        ducet_more(Table, MTable, Index),
        do_after_ducet_next(Table, MTable, Index);
    Index ->
        do_after_ducet_next(Table, MTable, Index)
    end.
        
ducet_more(Table, MTable, In) ->
    IF = fun(Val) -> 
        % Insert new value
        ets:insert(Table, {Val, more}),
        Reversed = lists:reverse(Val),
        ets:insert(MTable, {Reversed})
        end,
    
    LF = fun(Val) -> 
        % Lookup
        ets:member(Table, Val)
        end,
        
    do_ducet_more(LF, IF, lists:reverse(In)).

do_ducet_more(LF, IF, []) ->
    ok;
do_ducet_more(LF, IF, [El]) ->
    ok;
do_ducet_more(LF, IF, [_Last|ReversedBody] = _Codes) ->
    Body = lists:reverse(ReversedBody),
    case LF(Body) of
    true  -> ok;
    false -> IF(Body)
    end,
    do_ducet_more(LF, IF, ReversedBody).
        



%% @doc Add max, min, common values.
do_after_ranges(Table) ->
%   ets:safe_fixtable(Table, true),
    case ets:first(Table) of
    '$end_of_table' ->
        ok;
    Index ->
        [{_, Val}] = ets:lookup(Table, Index),
        [Init|ValList] = bin_to_list(Val),
        ?DBG(
            "~w:do_after_ranges: Init values: ~w. ~n", 
            [?MODULE, Init]),
        NewMax = lists:foldl(zip2fun(fun max/2), Init, ValList),
        NewMin = lists:foldl(zip2fun(fun min/2), Init, ValList),
        do_after_ranges_next(Table, Index, NewMax, NewMin)
    end.
%   ets:safe_fixtable(Table, false),
    
do_after_ranges_next(Table, PrevIndex, Min, Max) ->
    case ets:next(Table, PrevIndex) of
    '$end_of_table' ->
        InsFn = fun(Type) ->
                fun(Val, Lvl) ->
                    ets:insert(Table, {{Type, Lvl}, Val}),
                    Lvl + 1
                end
            end,
        lists:foldl(InsFn(min), 1, Min),
        lists:foldl(InsFn(max), 1, Max),
        
        ok;
    Index ->
        [{_, Val}] = ets:lookup(Table, Index),
        ValList = bin_to_list(Val),
        NewMax = lists:foldl(zip2fun(fun max/2), Max, ValList),
        NewMin = lists:foldl(zip2fun(fun min/2), Min, ValList),
        do_after_ranges_next(Table, Index, NewMin, NewMax)
    end.
        





%%
%% do_after helpers
%%

zip2fun(F) ->
    fun(L1, L2) -> lists:zipwith(F, L1, L2) end.

max(V1, V2) when V1 > V2 -> V1;
max(V1, V2) -> V2.
min(V1, V2) when V1 < V2 -> V1;
min(V1, V2) -> V2.

%% bin_to_list(Bin) -> lists:map(fun([H|T]) -> T end, bin_to_list2(Bin)).
bin_to_list(Bin) ->
    do_bin_to_list(Bin, []).

do_bin_to_list(<<_:8, L1:16, L2:8, L3:8, L4:16, Rem/binary>>, Res) ->
    El = [L1, L2, L3, L4],
    do_bin_to_list(Rem, [El|Res]);
do_bin_to_list(<<>>, Res) ->
    lists:reverse(Res).


bin_to_list2(Bin) ->
    do_bin_to_list2(Bin, []).

do_bin_to_list2(<<T:8, L1:16, L2:8, L3:8, L4:16, Rem/binary>>, Res) ->
    El = [type_atom(T), L1, L2, L3, L4],
    do_bin_to_list2(Rem, [El|Res]);
do_bin_to_list2(<<>>, Res) ->
    lists:reverse(Res).


%%
%% Helpers
%%

%% Parses "[.0000.0000.0000.0000]" to [<<0:8,0:16,0:16,0:16,0:16>>]
parse_el(El) -> 
    ListOfInts = lists:reverse(parse_el(El, [], false, [])),
    el_to_bin(ListOfInts).

% Buf - binary bufer
% Acc - string accumulator (f.e. [48,48,48,48])
parse_el([], _, _, Res) -> Res;
parse_el([$[, $. | Tail], _, _, Res) ->
    parse_el(Tail, [], [non_variable], Res); % [.XXXX.XXXX.XXXX.XXXX]
parse_el([$[, $* | Tail], _, _, Res) ->
    parse_el(Tail, [], [variable], Res); % [*XXXX.XXXX.XXXX.XXXX]
parse_el([_|Tail], Acc, false, Res) ->
    parse_el(Tail, Acc, false, Res);
parse_el([$]|Tail], Acc, Buf, Res) ->
    El = lists:reverse(el_res(Acc, Buf)),
    NewRes = split_large_weights(El, Res),
    parse_el(Tail, [], false, NewRes);
parse_el([$.|Tail], Acc, Buf, Res) ->
    parse_el(Tail, [], el_res(Acc, Buf), Res);
parse_el([H|Tail], Acc, Buf, Res) ->
    parse_el(Tail, [H|Acc], Buf, Res).

el_res(Acc, Buf) when is_list(Acc), is_list(Buf) ->
    Hex = ux_unidata_parser:hex_to_int(lists:reverse(Acc)),
    [Hex|Buf].

split_large_weights([Type, 0, 0, 0, 0], Res) -> Res;
split_large_weights([Type, L1, L2, L3, L4], Res) ->
    L1Max = 16#FFDD,
    L2Max = 16#DD,
    L3Max = 16#DD, % 1F?
    L4Max = 16#FFDD,
    split_large_weights(
        [Type, 
        if L1<L1Max -> 0; true -> L1 - L1Max + 1 end,
        if L2<L2Max -> 0; true -> L2 - L2Max + 1 end,
        if L3<L3Max -> 0; true -> L3 - L3Max + 1 end,
        if L4<L4Max -> 0; true -> L4 - L4Max + 1 end],

        [[Type, 
        if L1<L1Max -> L1; true -> L1Max end,
        if L2<L2Max -> L2; true -> L2Max end,
        if L3<L3Max -> L3; true -> L3Max end,
        if L4<L4Max -> L4; true -> L4Max end]|Res]
    ).

    
el_to_bin(List) -> do_el_to_bin(List, <<>>).
do_el_to_bin([[Type,L1,L2,L3,L4]|List], Bin) ->
    T = type_int(Type),
    NewBin = <<Bin/binary, T:8, L1:16, L2:8, L3:8, L4:16>>,
    do_el_to_bin(List, NewBin);
do_el_to_bin([], Bin) -> Bin.

type_int(non_variable) -> 0;
type_int(variable) -> 1.


type_atom(0) -> non_variable;
type_atom(1) -> variable.


%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

parse_el_test_() ->
    F = fun parse_el/1,
    [?_assertEqual(F("[.0000.0000.0000.0000]"), <<>>)
    ,?_assertEqual(F("[.0001.0002.0003.0004]"), <<0:8, 1:16, 2:8, 3:8, 4:16>>)
    ,?_assertEqual(F("[.0001.0002.0003.0004][*0005.0006.0007.0008]"), 
        <<0:8, 1:16, 2:8, 3:8, 4:16,   1:8, 5:16, 6:8, 7:8, 8:16>>)
    ].


-endif.
