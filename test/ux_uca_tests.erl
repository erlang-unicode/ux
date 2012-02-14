%%% @private
-module(ux_uca_tests).




-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include("ux_tests.hrl").
    

simple_sort_test_() ->
    F = fun ux_uca:sort/1,
    {"Simple input data.",
        ?_assertEqualTO(F(["A", "C", "B"]), ["A", "B", "C"])}.

natural_sort_test_() ->
    Unsorted = ["1000X Radonius Maximus",
                "10X Radonius",
                "200X Radonius",
                "20X Radonius",
                "20X Radonius Prime",
                "30X Radonius",
                "40X Radonius",
                "Allegia 50 Clasteron",
                "Allegia 500 Clasteron",
                "Allegia 51 Clasteron",
                "Allegia 51B Clasteron",
                "Allegia 52 Clasteron",
                "Allegia 60 Clasteron",
                "Alpha 100",
                "Alpha 2",
                "Alpha 200",
                "Alpha 2A",
                "Alpha 2A-8000",
                "Alpha 2A-900",
                "Callisto Morphamax",
                "Callisto Morphamax 500",
                "Callisto Morphamax 5000",
                "Callisto Morphamax 600",
                "Callisto Morphamax 700",
                "Callisto Morphamax 7000",
                "Callisto Morphamax 7000 SE",
                "Callisto Morphamax 7000 SE2",
                "QRS-60 Intrinsia Machine",
                "QRS-60F Intrinsia Machine",
                "QRS-62 Intrinsia Machine",
                "QRS-62F Intrinsia Machine",
                "Xiph Xlater 10000",
                "Xiph Xlater 2000",
                "Xiph Xlater 300",
                "Xiph Xlater 40",
                "Xiph Xlater 5",
                "Xiph Xlater 50",
                "Xiph Xlater 500",
                "Xiph Xlater 5000",
                "Xiph Xlater 58"],

    Sorted = ["10X Radonius",
         "20X Radonius",
         "20X Radonius Prime",
         "30X Radonius",
         "40X Radonius",
         "200X Radonius",
         "1000X Radonius Maximus",
         "Allegia 50 Clasteron",
         "Allegia 51 Clasteron",
         "Allegia 51B Clasteron",
         "Allegia 52 Clasteron",
         "Allegia 60 Clasteron",
         "Allegia 500 Clasteron",
         "Alpha 2",
         "Alpha 2A",
         "Alpha 2A-900",
         "Alpha 2A-8000",
         "Alpha 100",
         "Alpha 200",
         "Callisto Morphamax",
         "Callisto Morphamax 500",
         "Callisto Morphamax 600",
         "Callisto Morphamax 700",
         "Callisto Morphamax 5000",
         "Callisto Morphamax 7000",
         "Callisto Morphamax 7000 SE",
         "Callisto Morphamax 7000 SE2",
         "QRS-60 Intrinsia Machine",
         "QRS-60F Intrinsia Machine",
         "QRS-62 Intrinsia Machine",
         "QRS-62F Intrinsia Machine",
         "Xiph Xlater 5",
         "Xiph Xlater 40",
         "Xiph Xlater 50",
         "Xiph Xlater 58",
         "Xiph Xlater 300",
         "Xiph Xlater 500",
         "Xiph Xlater 2000",
         "Xiph Xlater 5000",
         "Xiph Xlater 10000"],

    C1 = ux_uca_options:get_options([
                {natural_sort, true}, 
                {alternate, non_ignorable}]),

    C2 = ux_uca_options:get_options([
                    {natural_sort, true}, 
                    {alternate, non_ignorable},
                    {case_first, upper}]),

    F = fun ux_uca:sort/2,


    [{"Using official test strings from Dave Koelle", 
        ?_assertEqualTO(F(C1, Unsorted), Sorted)},

     {"Case and natural sort hacks together.", 
        ?_testTO(F(C2, Unsorted))}
    ].



%---------------------------------------------------------------
% SLOW TESTS 
%---------------------------------------------------------------
-ifdef(SLOW_TESTS).
non_ignorable_test_() ->
    {timeout, 600, 
        fun() -> 
            prof(ux_unidata:open_test_file('collation_test_non_ignorable'), 
                ux_uca_options:get_options(non_ignorable), 
                10) 
        end}.

shifted_test_() ->
    {timeout, 600, 
        fun() -> 
            prof(ux_unidata:open_test_file('collation_test_shifted'), 
                ux_uca_options:get_options(shifted), 
                10) end}.

















natural_sort_long_test_() ->
    {timeout, 600,
        fun() ->
            nat_prof(lists:seq(1, 10000, 1)),
            nat_prof(lists:seq(1, 10000000, 1000)),
            nat_prof(lists:seq(1, 100000000000000, 9999999999)),
            io:format(user, "~n", [])
        end}.

-endif.

%%
%% Helpers
%%

%% Collation Test
%% Parse data files from 
%% http://www.unicode.org/Public/UCA/latest/
%% README: http://www.unicode.org/Public/UCA/latest/CollationTest.html
collation_test(_Fd, _F, _OldVal, StrNum, 0 = _Max, Res) ->
    io:format(user, "Only ~w strings were tested. Exit.~n", [StrNum]),
    Res; % max
% Read first string with data from file.
collation_test(Fd, P, false, StrNum, Max, Res) ->
    OldVal = test_read(Fd, StrNum),
    collation_test(Fd, P, OldVal, StrNum, Max, Res);
collation_test(Fd, Params, {OldFullStr, OldVal, StrNum}, _OldStrNum, Max, Res) ->
    % Add new string.
    case StrNum of
    100 -> io:format(user, "~n", []);
    _   -> boo
    end,
    % Show message
    case StrNum rem 10000 of
    0 -> io:format(user, "~w strings were tested. ~n", [StrNum]);
    _ -> boo
    end,

    case test_read(Fd, StrNum) of
    {FullStr, Val, NewStrNum} = Result when is_list(Val) -> 
        % 1. check compare/3.
        Res2 = case ux_uca:compare(Params, Val, OldVal) of % collation compare
            % error
            lower -> io:format(user, "Error (compare): ~w ~w ~w ~n", 
                    [Val, lower, OldVal]),
                io:format(user,
                    " Data1: ~ts Data2: ~ts",
                    [OldFullStr, FullStr]),
                    error;
            % OK (equal or upper). 
            _ -> Res
            end,

        % 2. check sort_key/2.
        Key1 = ux_uca:sort_key(Params, OldVal),
        Key2 = ux_uca:sort_key(Params, Val),
        Res3 = if
                Key1 > Key2 ->
                io:format(user, "Error (key): ~w ~w ~w ~n", 
                    [Val, lower, OldVal]),
                io:format(user,
                    " Data1: ~ts Data2: ~ts",
                    [OldFullStr, FullStr]),
                io:format(user,
                    " Key1: ~w ~n Key2: ~w~n",
                    [Key1, Key2]),
                
                Arr1 = ux_uca:sort_array(Params, OldVal),
                Arr2 = ux_uca:sort_array(Params, Val),
                io:format(user,
                    " Arr1: ~w ~n Arr2: ~w~n",
                    [Arr1, Arr2]),
                    error;
                true -> Res2
            end,
            
        collation_test(Fd, Params, Result, NewStrNum, Max - 1, Res3);
    _ -> ok
    end.

%% @doc Read line from a testdata file Fd (see CollationTest.html).
%% Return list of codepaints.
%% Used by test/4.
%% @end
%% @private
test_read(Fd, StrNum) ->
    case io:get_line(Fd, "") of
    eof -> ok;
    {error,Mess} -> throw({error, "Error while reading file", Mess});
    Data -> 
        try % parse Data
            [Value|_] = ux_string:split(["#", ";", "\n"], Data), 
            %% Converts "0009 0021" to [16#0009, 16#0021]
            Parsed = lists:map(fun ux_unidata_parser:hex_to_int/1, 
                      string:tokens(Value, " ")),
            %% Delete false values.
            [X || X <- Parsed, X] 
        of Res -> {Data, Res, StrNum + 1} % {FullStr, Codepaints}
        catch                   
        error:_Reason -> 
%            io:format(user, "~w: Data=~w ~n", [Reason, Data]),
            test_read(Fd, StrNum + 1)
        end
    end.

prof(Fd, Params, Count) ->
%    io:setopts(Fd,[{encoding,utf8}]),
    Res = collation_test(Fd, Params, false, 0, Count, ok),
    file:close(Fd),
    ?assertEqual(Res, ok).


nat_prof(Seq) ->
    Lists = [lists:flatten(io_lib:format("Abr~w", [X])) || X <- Seq],
    Params = ux_uca_options:get_options(
            [{alternate, non_ignorable}, {natural_sort, true}]
        ),
    {Time, SortedLists} = timer:tc(ux_uca, sort, [Params, Lists]),
    io:format(user, "~n Sort Time, ~.3gs ", [Time / 1000000]),
    ?_assertEqual(Lists, SortedLists).

-endif. % TEST
