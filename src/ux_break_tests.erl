-module(ux_break_tests).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

parse(S) -> 
    Res = [],
    ResWithDelims = [],
    io:format(user, "~p~n", [S]),
    P = do_parse(S, Res, ResWithDelims),
    io:format(user, "~p~n", [P]),
    P.

do_parse([[247]|T], R, RD) ->
    NewRD = ['-'|RD],
    do_parse(T, R, NewRD);

do_parse([[215]|T], R, RD) ->
    NewRD = ['x'|RD],
    do_parse(T, R, NewRD);

do_parse([[]|T], R, RD) ->
    do_parse(T, R, RD);

do_parse([H|T], R, RD) ->
    Int = ux_unidata_parser:hex_to_int(H),
    true = (Int=/=false),
    
    NewRD = [Int|RD],
    NewR = [Int|R],
    do_parse(T, NewR, NewRD);

do_parse([], R, RD) ->
    {lists:reverse(R), lists:reverse(RD)}.
    

run_test(FileName, Fun, Fun2) ->
    {ok, Fd} = file:open(FileName, [read]),
    io:setopts(Fd, [{encoding,utf8}]),
    do_test(Fd, Fun, Fun2),
    file:close(Fd).

do_test(Fd, Fun, Fun2) ->
    case io:get_line(Fd, "") of
    eof -> ok;
    Data ->
        Data1 = ux_unidata_parser:delete_comments(Data),
        Data2 = ux_unidata_parser:delete_spaces(Data1, $\t),
        Tokens = ux_unidata_parser:split($ , Data2),
        {Str, StrWithDelims} = parse(Tokens),
        ?assertEqual(Fun2(StrWithDelims), Fun(Str)),
        do_test(Fd, Fun, Fun2)
    end.
    

grapheme_break_test_() ->
    Fun = fun(S) -> 
        ux_gb:split('extended', S) 
        end,
    Fun2 = fun(S) -> 
        [X || X <- S, X=/='-']
        end,
    File = ux_unidata:get_test_file('grapheme_break_test'),
    {timeout, 600, 
        fun() -> 
        run_test(File, Fun, Fun2)
        end}.



word_break_test_() ->
    Fun = fun(S) -> 
        R = ux_wb:split(S),
        case R of
        [] -> [];
        _ -> ['-'] ++ R ++ ['-']
        end
        end,
    Fun2 = fun(S) -> 
        [X || X <- S, X=/='x']
        end,
    File = ux_unidata:get_test_file('word_break_test'),
    {timeout, 600, 
        fun() -> 
        run_test(File, Fun, Fun2)
        end}.


-endif.
    
