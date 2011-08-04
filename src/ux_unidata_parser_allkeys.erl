-module(ux_unidata_parser_allkeys).
-export([parse/1, types/0, get_function/2
    , after_parse/1 % comment to disable post processing
    ]).
%%% Example:
%%% ux_unidata_filelist:get_pid({allkeys, [ducet], code:priv_dir(ux) ++ "/UNIDATA/allkeys.txt"}).

types() -> [ducet, ducet_r].

parse(In) ->
    case ux_unidata_parser:split($;, In) of
    [[_|_] = Code, [_|_] = Element] ->
        InEl = lists:map(fun ux_unidata_parser:hex_to_int/1,
                        string:tokens(Code, " ")),
        OutEl = parse_el(ux_unidata_parser:delete_spaces(Element)),
        %io:format("String: ~ts, From reversed: ~w, To: ~w~n", [In, InEl, OutEl]),
        {ok,
            [{ducet, {InEl, OutEl}} 
            ,{ducet_r, {lists:reverse(InEl), OutEl}}
            ]
        };
    _ -> skip
    end.

after_parse(Ets) ->
    do_after(Ets),
    ok.

get_function(ducet_r, Table) -> 
    ux_unidata_parser:ets_fun(Table, other);
get_function(ducet, Table) -> 
    ux_unidata_parser:ets_fun(Table, other).

%%
%% Hacks.
%%

do_after([{ducet, Table} | Tail]) ->
    do_after_ducet(Table, fun ducet_more/2),
    do_after(Tail);
do_after([{ducet_r, Table} | Tail]) ->
    do_after_ducet(Table, fun ducet_r_more/2),
    do_after(Tail);
do_after([_ | Tail]) ->
    do_after(Tail);
do_after([]) -> ok.

do_after_ducet(Table, MoreFun) ->
%   ets:safe_fixtable(Table, true),
    case ets:first(Table) of
    '$end_of_table' ->
        ok;
    Index ->
        MoreFun(Table, Index),
        do_after_ducet_next(Table, MoreFun, Index)
    end.
%   ets:safe_fixtable(Table, false),
    
do_after_ducet_next(Table, MoreFun, PrevIndex) ->
    case ets:next(Table, PrevIndex) of
    '$end_of_table' ->
        ok;
    Index ->
        MoreFun(Table, Index),
        do_after_ducet_next(Table, MoreFun, Index)
    end.
        
ducet_more(Table, In) ->
    do_ducet_more(Table, lists:reverse(In)).

do_ducet_more(Table, []) ->
    ok;
do_ducet_more(Table, [El]) ->
    ok;
do_ducet_more(Table, [_Last|ReversedBody] = _Codes) ->
    Body = lists:reverse(ReversedBody),
    case ets:member(Table, Body) of
    true  -> ok;
    false -> ets:insert(Table, {Body, more})
    end,
    do_ducet_more(Table, ReversedBody).
        
ducet_r_more(Table, In) ->
    do_ducet_r_more(Table, In).

do_ducet_r_more(Table, []) ->
    ok;
do_ducet_r_more(Table, [El]) ->
    ok;
do_ducet_r_more(Table, [_Last|ReversedBody] = _Codes) ->
    Body = lists:reverse(ReversedBody),
    case ets:member(Table, Body) of
    true  -> ok;
    false -> ets:insert(Table, {lists:reverse(Body), more})
    end,
    do_ducet_r_more(Table, ReversedBody).

%%
%% Helpers
%%

%% Parses "[.0000.0000.0000.0000]" to [<<0/8,0/32,0/32,0/32,0/32>>]
parse_el(El) -> lists:reverse(parse_el(El, [], false, [])).

% Buf - binary bufer
% Acc - string accumulator (f.e. [48,48,48,48])
parse_el([], _, _, Res) -> Res;
parse_el([$[, $. | Tail], _, _, Res) ->
    parse_el(Tail, [], <<0:8>>, Res); % [.XXXX.XXXX.XXXX.XXXX]
parse_el([$[, $* | Tail], _, _, Res) ->
    parse_el(Tail, [], <<1:8>>, Res); % [*XXXX.XXXX.XXXX.XXXX]
parse_el([_|Tail], Acc, false, Res) ->
    parse_el(Tail, Acc, false, Res);
parse_el([$]|Tail], Acc, Buf, Res) ->
    parse_el(Tail, [], false, [el_res(Acc, Buf)|Res]);
parse_el([$.|Tail], Acc, Buf, Res) ->
    parse_el(Tail, [], el_res(Acc, Buf), Res);
parse_el([H|Tail], Acc, Buf, Res) ->
    parse_el(Tail, [H|Acc], Buf, Res).

el_res(Acc, Buf) when length(Acc) == 5 ->
    Hex = ux_unidata_parser:hex_to_int(lists:reverse(Acc)),
    <<Buf/binary,Hex:24>>;
el_res(Acc, Buf) when length(Acc) == 4 ->
    Hex = ux_unidata_parser:hex_to_int(lists:reverse(Acc)),
    <<Buf/binary,Hex:16>>.



%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

parse_el_test_() ->
    F = fun parse_el/1,
    [?_assertEqual(F("[.0000.0000.0000.0000]"), [<<0:8, 0:16, 0:16, 0:16, 0:16>>])
    ,?_assertEqual(F("[.0001.0002.0003.0004]"), [<<0:8, 1:16, 2:16, 3:16, 4:16>>])
    ,?_assertEqual(F("[.0001.0002.0003.0004][*0005.0006.0007.0008]"), 
        [<<0:8, 1:16, 2:16, 3:16, 4:16>>, <<1:8, 5:16, 6:16, 7:16, 8:16>>])
    ].


-endif.
