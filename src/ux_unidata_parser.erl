%%% @doc This module converts UNIDATA from txt files to ETS.
%%%      This module is used by fun ux_unidata_store:init/1.
%%% @end
%%% @private
-module(ux_unidata_parser).

-export([run/1, check/1, get_functions/2]).
-export([check_types/2]).

%% Helpers
-export([split/2, hex_to_int/1, from_hex/1, delete_spaces/1]).

%% Intermodule export
-export([expand_table/1]).
-export([expand_fun/2, ets_fun/2, bool_fun/1]).

-spec check(tuple()) -> ok.
check({FileType, all, FileName}) ->
    Mod = filetype_to_module(FileType),
    Mod:types(),
    check_filename(FileName),
    ok;
check({FileType, DataTypes, FileName}) ->
    Mod = filetype_to_module(FileType),
    AllowedTypes = Mod:types(),
    true = check_types(DataTypes, AllowedTypes),
    check_filename(FileName),
    ok.

check_filename(FileName) ->
    % File exists?
    {ok, _Info} = file:read_file_info(FileName),
    ok.

-spec run(tuple()) -> ok.
run({FileType, all, FileName}) ->
    Mod = filetype_to_module(FileType),
    run({FileType, Mod:types(), FileName});
run({FileType, DataTypes, FileName}) ->
    Fd = open_file(FileName),
    Mod = filetype_to_module(FileType),
    AllowedTypes = Mod:types(),
    true = check_types(DataTypes, AllowedTypes),
    Ets = create_tables(DataTypes),
    SortedEtsTables = lists:sort(Ets),
    read_file({Fd, SortedEtsTables, Mod}),
    file:close(FileName),

    % Some parsers need post-hacks.
    ok = run_after_parse(Mod, SortedEtsTables),

    RemTypes = AllowedTypes -- DataTypes,
    {ok, SortedEtsTables, RemTypes}.

get_functions(FileType, EtsTables) ->
    Mod = filetype_to_module(FileType),
    lists:map(fun({Type, Table}) ->
        {Type, Table, Mod:get_function(Type, Table)} end, EtsTables).

%% Return a file descriptor or throw error.
open_file(FileName) ->
    try
        {ok, Fd} = file:open(FileName, [read]),
        Fd
    catch
        Class:Reason ->
            error_logger:error_msg(
                "~w: Cannot open file ~ts. ~n",
                [?MODULE, FileName]),

            Trace = erlang:get_stacktrace(),
            erlang:raise(Class, Reason, Trace)
    end.


% Try run Mod:after_parse/1 handler.
run_after_parse(Mod, Ets) ->
    case is_after_handler(Mod) of
    true -> Mod:after_parse(Ets);
    false -> ok
    end.

is_after_handler(Mod) ->
    lists:member({after_parse,1}, Mod:module_info(exports)).


%% All elements from D are in A.
check_types(D, A) ->
    do_check_types(lists:sort(D), lists:sort(A)).

do_check_types([H|DT], [H|AT]) ->
    do_check_types(DT, AT);
do_check_types(D, [_|AT]) ->
    do_check_types(D, AT);
do_check_types([], _) ->
    true;
do_check_types([_|_], _) ->
    false.
    
-spec create_tables([atom()]) -> [{atom(), integer()}].
create_tables(DataTypes) ->
    do_create_tables(DataTypes, []).

do_create_tables([Name|T], Acc) ->
    E = ets:new(Name, []),
    error_logger:info_msg(
        "~w:  Create ETS table ~w. ~n",
        [?MODULE, E]),
    do_create_tables(T, [{Name, E}|Acc]);
do_create_tables([], Acc) -> Acc.

-spec filetype_to_module(atom()) -> atom().
filetype_to_module(Type) -> list_to_atom("ux_unidata_parser_"
        ++ atom_to_list(Type)).

read_file({Fd, Ets, Mod} = State) ->
    case file:read_line(Fd) of
    {ok, []} -> 
        read_file(State);
    {ok, Line} ->
        case Mod:parse(delete_nr(delete_comments(Line))) of
        skip -> 
            read_file(State);
        {ok, Val} ->
            ok = write_to_ets(Ets, lists:sort(Val)),
            read_file(State)
        end;
    eof -> ok
    end.
        
-spec delete_comments(string()) -> string().
delete_comments(Line) ->
    lists:reverse(do_delete_comments(Line, [])).

do_delete_comments([], Acc) -> Acc;
do_delete_comments([$# | _], Acc) -> Acc;
do_delete_comments([H|T], Acc) -> 
    do_delete_comments(T, [H|Acc]).

-spec write_to_ets([{atom(), integer()}], [{atom(), tuple()}]) -> ok.
write_to_ets([{Name, _Table} | EtsTail], [{Name, skip} | ValTail]) ->
    write_to_ets(EtsTail, ValTail);
write_to_ets([{Name, Table} | EtsTail], [{Name, Val} | ValTail]) ->
    ets:insert(Table, Val),
    write_to_ets(EtsTail, ValTail);
write_to_ets(Ets, [_ | ValTail]) ->
    write_to_ets(Ets, ValTail);
write_to_ets([], []) -> ok.


delete_nr(Str) -> [X || X <- Str, X =/= $\n, X =/= $\r].


%% This functions are used in parser modules.
-spec split(char(), string()) -> [string].
split(Char, Str) -> lists:reverse(do_split(Char, Str, [], [])).

do_split(Char, [Char|Tail], Acc1, Acc2) ->
    do_split(Char, Tail, [], [lists:reverse(Acc1) | Acc2]);
do_split(_Char, [], Acc1, Acc2) ->
    [lists:reverse(Acc1) | Acc2];
do_split(Char, [Head|Tail], Acc1, Acc2) ->
    do_split(Char, Tail, [Head|Acc1], Acc2).

hex_to_int(Code) ->
    case io_lib:fread("~16u", Code) of 
    {ok, [Int], []} -> Int;
    _ -> false
    end.

from_hex([$<|Str]) -> 
    SubStr = string:sub_string(Str, string:chr(Str, $>)+1),
    from_hex(SubStr);
from_hex(Str) -> 
    lists:map(fun hex_to_int/1, string:tokens(Str, " ")).

delete_spaces(Str) -> [X || X <- Str, X =/= $ ].

%%
%% Helpers
%%

%% Expand table with one colums.
expand_table(Table) ->
    ets:safe_fixtable(Table, true),
    case ets:first(Table) of
    '$end_of_table' ->
        ok;
    {From, To} = El ->
        do_expand(Table, From, To),
        expand_table_next(Table, El);
    El ->
        expand_table_next(Table, El) 
    end,
    ets:safe_fixtable(Table, false),
    ok.

expand_table_next(Table, Prev) ->
    case ets:next(Table, Prev) of
    '$end_of_table' ->
        ok;
    {From, To} = El ->
        ets:delete(Table, El),
        do_expand(Table, From, To),
        expand_table_next(Table, El);
    El ->
        expand_table_next(Table, El) 
    end.

do_expand(Table, From, To) when From =< To ->
    ets:insert(Table, {From}),
    do_expand(Table, From+1, To);
do_expand(_, _, _) -> ok.





%% Expand table with two colums: [{Key, Value} or {{From, To}, Value}].
expand_fun(Table, DefaultValue) ->
    Body = do_fun_def(DefaultValue),
    NewBody = "fun " ++ do_expand_fun(Table, Body, DefaultValue),
%   io:format(user, "Fun: ~s~n",[NewBody]),

    %% Scan the code into tokens
    {ok, ErlTokens, _} = erl_scan:string(lists:flatten(NewBody)),

    %% Now parse the tokens into the abstract form
    {ok, ErlAbsForm} = erl_parse:parse_exprs(ErlTokens),

    %% Now evaluate the string
    Bindings = erl_eval:new_bindings(),
    {value, Value, _} = erl_eval:exprs(ErlAbsForm, Bindings),
    Value.

-spec do_expand_fun(Table::integer(), Body::string(), DefaultValue::term()) 
    -> string().
do_expand_fun(Table, Body, DefaultValue) ->
    case ets:first(Table) of
    '$end_of_table' ->
        Body;
    Key -> NewBody = case ets:lookup(Table, Key) of
            [{Key, DefaultValue}] -> Body;
            [{Key, Val}] -> do_fun_meta(Key, Val) ++ Body
            end,
        do_expand_fun_next(Table, NewBody, DefaultValue, Key)
    end.

-spec do_expand_fun_next(Table::integer(), Body::string(), 
    DefaultValue::term(), Prev::term()) -> string().
do_expand_fun_next(Table, Body, DefaultValue, Prev)  ->
    case ets:next(Table, Prev) of
    '$end_of_table' ->
        Body;
    Key -> NewBody = case ets:lookup(Table, Key) of
            [{Key, DefaultValue}] -> Body;
            [{Key, Val}] -> do_fun_meta(Key, Val) ++ Body
            end,
        do_expand_fun_next(Table, NewBody, DefaultValue, Key)
    end.



-spec do_fun_meta(Key::term(), Value::term()) -> string().
do_fun_meta({From, To}, Val) ->
    io_lib:format("(V) when V >= ~w andalso V =< ~w -> ~w; ",
            [From, To, Val]);
do_fun_meta(Key, Val) ->
    io_lib:format("(~w) -> ~w; ",
            [Key, Val]).

-spec do_fun_def(Value::term()) -> string().
do_fun_def(noop) ->
    "(C) -> C end.";
do_fun_def(Val) ->
    io_lib:format("(_) -> ~w end.",
            [Val]).
    

ets_fun(Table, DefaultValue) ->
    fun(Key) ->
        case ets:lookup(Table, Key) of
        [] when DefaultValue =:= noop
           -> Key;
        [] -> DefaultValue;
        [{Key, Val}] -> Val
        end
    end.

bool_fun(Table) ->
    fun(Key) ->
        ets:member(Table, Key)
    end.

%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

delete_comments_test_() ->
    F = fun delete_comments/1,
    [?_assertEqual(F("Test string"), "Test string")
    ,?_assertEqual(F("Test#string"), "Test")
    ,?_assertEqual(F("Test#string#comment"), "Test")
    ].


check_types_test_() ->
    F = fun check_types/2,
    [?_assert(F([a,b,c,d,e], [a,b,c,d,e]))
    ,?_assert(F([a,c], [a,b,c,d,e]))
    ,?_assert(F([c,d,a], [a,b,c,d,e]))
    ,?_assert(not F([a,b,c,d,e], [a,b,c,d]))
    ,?_assert(not F([a,b,d,c,d,e], [a,b,c,e]))
    ].


split_test_() ->
    F = fun split/2,
    [?_assertEqual(F($;, "Long;long;time;ago"), ["Long", "long", "time", "ago"])
    ,?_assertEqual(F($;, "Long;long;time;ago;"), ["Long", "long", "time", "ago", ""])
    ,?_assertEqual(F($;, ";Long;long;time;ago"), ["", "Long", "long", "time", "ago"])
    ,?_assertEqual(F($;, "Long;;time;ago"), ["Long", "", "time", "ago"])
    ].

delete_spaces_test_() ->
    F = fun delete_spaces/1,
    [?_assertEqual(F("delete spaces"), "deletespaces")
    ,?_assertEqual(F(" del e te  "), "delete")
    ].


from_hex_test_() ->
    F = fun from_hex/1,
    [?_assertEqual(F("<compat> 0069 0328"), [16#0069, 16#0328])
    ,?_assertEqual(F("0033 0043"), [16#0033, 16#0043])
    ].

expand_fun_test_() ->
    {setup,
     fun () -> T = ets:new(expand_fun_ets, []),
        ets:insert(T, {2, two}),
        ets:insert(T, {3, three}),
        ets:insert(T, {{4,10}, interval}),

        {T, expand_fun(T, default)}
        
        end,
     fun ({T, _F}) -> ets:delete(T) end,
     fun ({_T, F}) ->
        [?_assertEqual(F(2),   two)
        ,?_assertEqual(F(3),   three)
        ,?_assertEqual(F(4),   interval)
        ,?_assertEqual(F(6),   interval)
        ,?_assertEqual(F(10),  interval)
        ,?_assertEqual(F(1),   default)
        ,?_assertEqual(F(11),  default)
        ,?_assertEqual(F(666), default)
        ]
     end}.

-endif.
