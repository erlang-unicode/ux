%%% @private
-module(ux_uca_tests).





-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include("ux_tests.hrl").
% There is an error in normalization:
%
%(ux@omicron)3> ux_string:to_nfd([818,820]).
%[820,818]
%(ux@omicron)5> ux_unidata:ducet([818]).
%[[non_variable,0,33,2,818]]
%(ux@omicron)9> ux_unidata:ducet([119364]).
%[[non_variable,0,0,0,65501],[non_variable,0,0,0,53864]]
%(ux@omicron)10> ux_unidata:ducet([820]).
%[[non_variable,0,124,2,820]]
% non_ignorable_test_:
% Error (compare): [119364,820] greater [818,820]
% Error (key): [119364,820] greater [818,820]
%  Key1: <<0,0,158,0,225>>
%  Key2: <<0,0,67,158,0,224>>
%  Arr1: [[non_variable,0,0,0,65501],[non_variable,0,0,0,53864],[non_variable,0,124,2,820]]
%  Arr2: [[non_variable,0,33,2,818],[non_variable,0,124,2,820]]
%  Unzip Key1: [0,124,0,2]
%  Unzip Key2: [0,33,124,0,2,2]
%  Data1: 1D244 0334
%  Data2: 0332 0334



%Error (compare): [44032,33] greater [12910,33]
%Error (key): [44032,33] greater [12910,33]
% Key1: <<49,59,49,185,49,185,49,185,2,94,0,0,61,0,221>>
% Key2: <<49,59,49,185,2,94,2,94,0,0,62,0,230,230,224>>
% Arr1: [[non_variable,12603,32,2,4352],[non_variable,12729,32,2,4449],[non_variable,12729,32,2,4449],[non_variable,12729,32,2,4449],[variable,606,32,2,33]]
% Arr2: [[non_variable,12603,32,6,12910],[non_variable,12729,32,6,12910],[variable,606,32,2,33],[variable,606,32,2,33]]
% Unzip Key1: [12603,12729,12729,12729,606,0,32,32,32,32,32,0,2,2,2,2,2]
% Unzip Key2: [12603,12729,606,606,0,32,32,32,32,0,6,6,2,2]
%Error (key): [4352,4449,33] greater [12910,33]
% Key1: <<49,59,49,185,49,185,49,185,2,94,0,0,61,0,221>>
% Key2: <<49,59,49,185,2,94,2,94,0,0,62,0,230,230,224>>
% Arr1: [[non_variable,12603,32,2,4352],[non_variable,12729,32,2,4449],[non_variable,12729,32,2,4449],[non_variable,12729,32,2,4449],[variable,606,32,2,33]]
% Arr2: [[non_variable,12603,32,6,12910],[non_variable,12729,32,6,12910],[variable,606,32,2,33],[variable,606,32,2,33]]
% Unzip Key1: [12603,12729,12729,12729,606,0,32,32,32,32,32,0,2,2,2,2,2]
% Unzip Key2: [12603,12729,606,606,0,32,32,32,32,0,6,6,2,2]
% Data1: AC00 0021
% Data2: 326E 0021
%
%Error (compare): [44032,63] greater [12910,63]
%Error (key): [44032,63] greater [12910,63]
% Key1: <<49,59,49,185,49,185,49,185,2,99,0,0,61,0,221>>
% Key2: <<49,59,49,185,2,99,2,99,0,0,62,0,230,230,224>>
% Arr1: [[non_variable,12603,32,2,4352],[non_variable,12729,32,2,4449],[non_variable,12729,32,2,4449],[non_variable,12729,32,2,4449],[variable,611,32,2,63]]
% Arr2: [[non_variable,12603,32,6,12910],[non_variable,12729,32,6,12910],[variable,611,32,2,63],[variable,611,32,2,63]]
% Unzip Key1: [12603,12729,12729,12729,611,0,32,32,32,32,32,0,2,2,2,2,2]
% Unzip Key2: [12603,12729,611,611,0,32,32,32,32,0,6,6,2,2]
%Error (key): [4352,4449,63] greater [12910,63]
% Key1: <<49,59,49,185,49,185,49,185,2,99,0,0,61,0,221>>
% Key2: <<49,59,49,185,2,99,2,99,0,0,62,0,230,230,224>>
% Arr1: [[non_variable,12603,32,2,4352],[non_variable,12729,32,2,4449],[non_variable,12729,32,2,4449],[non_variable,12729,32,2,4449],[variable,611,32,2,63]]
% Arr2: [[non_variable,12603,32,6,12910],[non_variable,12729,32,6,12910],[variable,611,32,2,63],[variable,611,32,2,63]]
% Unzip Key1: [12603,12729,12729,12729,611,0,32,32,32,32,32,0,2,2,2,2,2]
% Unzip Key2: [12603,12729,611,611,0,32,32,32,32,0,6,6,2,2]
% Data1: AC00 003F
% Data2: 326E 003F


%(ux@omicron)2> ux_string:to_nfd([55176,4469]).
%[4370,4469,4469]
%(ux@omicron)3> ux_string:to_nfd([55176]).
%[4370,4469]
%(ux@omicron)4> ux_string:to_nfd([4469,4469]).
%[4469,4469]
%(ux@omicron)6> ux_string:to_nfd([55198]).
%[4370,4469,4541]
%(ux@omicron)10> ux_string:to_nfd([55176]).
%[4370,4469]
% ux_unidata:ducet([4541]).
% [[non_variable,12844,32,2,4541]]
% Is 4541 a hangul T?

%Error (compare): [55176,4469,98] greater [55198,33]
%Error (key): [55176,4469,98] greater [55198,33]
% Key1: <<49,77,49,205,49,205,23,251,21,234,0,0,62,0,222>>
% Key2: <<49,77,49,205,23,251,50,44,2,94,0,0,62,0,222>>
% Arr1: [[non_variable,12621,32,2,4370],[non_variable,12749,32,2,4469],[non_variable,12749,32,2,4469],[non_variable,6139,0,0,0],[non_variable,5610,32,2,98]]
% Arr2: [[non_variable,12621,32,2,4370],[non_variable,12749,32,2,4469],[non_variable,6139,0,0,0],[non_variable,12844,32,2,4541],[variable,606,32,2,33]]
% Unzip Key1: [12621,12749,12749,6139,5610,0,32,32,32,32,0,2,2,2,2]
% Unzip Key2: [12621,12749,6139,12844,606,0,32,32,32,32,0,2,2,2,2]
%Error (key): [4370,4469,4469,98] greater [4370,4469,4541,33]
% Key1: <<49,77,49,205,49,205,23,251,21,234,0,0,62,0,222>>
% Key2: <<49,77,49,205,23,251,50,44,2,94,0,0,62,0,222>>
% Arr1: [[non_variable,12621,32,2,4370],[non_variable,12749,32,2,4469],[non_variable,12749,32,2,4469],[non_variable,6139,0,0,0],[non_variable,5610,32,2,98]]
% Arr2: [[non_variable,12621,32,2,4370],[non_variable,12749,32,2,4469],[non_variable,6139,0,0,0],[non_variable,12844,32,2,4541],[variable,606,32,2,33]]
% Unzip Key1: [12621,12749,12749,6139,5610,0,32,32,32,32,0,2,2,2,2]
% Unzip Key2: [12621,12749,6139,12844,606,0,32,32,32,32,0,2,2,2,2]
% Data1: D788 1175 0062
% Data2: D79E 0021
%

%Error (key): [108,119141,903,97] greater [108,97]
% Key1: <<22,246,21,212,0,0,64,0,224,0,255,254,2,133,255,254>>
% Key2: <<22,246,21,212,0,0,64,0,224,0,255,253>>
% Arr1: [[non_variable,5878,32,2,108],[non_variable,0,0,0,65501],[non_variable,0,0,0,53641],[variable,644,32,2,183],[non_variable,5588,32,2,97]]
% Arr2: [[non_variable,5878,32,2,108],[non_variable,5588,32,2,97]]
%Error in the compression algorithm.
% Unzip Key1: [5878,5588,0,32,32,0,2,2,0,65502,644,65502]
% Unzip Key2: [5878,5588,0,32,32,0,2,2,0,65502,65502]
%Error (key): [108,119141,183,97] greater [108,97]
% Key1: <<22,246,21,212,0,0,64,0,224,0,255,254,2,133,255,254>>
% Key2: <<22,246,21,212,0,0,64,0,224,0,255,253>>
% Arr1: [[non_variable,5878,32,2,108],[non_variable,0,0,0,65501],[non_variable,0,0,0,53641],[variable,644,32,2,183],[non_variable,5588,32,2,97]]
% Arr2: [[non_variable,5878,32,2,108],[non_variable,5588,32,2,97]]
%Error in the compression algorithm.
% Unzip Key1: [5878,5588,0,32,32,0,2,2,0,65502,644,65502]
% Unzip Key2: [5878,5588,0,32,32,0,2,2,0,65502,65502]
%sort_key and compare returns different results.
% Data1: 006C 1D165 0387 0061
% Data2: 006C 0061
%
%Error (key): [76,119141,903,97] greater [76,97]
% Key1: <<22,246,21,212,0,0,64,0,232,225,0,255,254,2,133,255,254>>
% Key2: <<22,246,21,212,0,0,64,0,232,225,0,255,253>>
% Arr1: [[non_variable,5878,32,8,76],[non_variable,0,0,0,65501],[non_variable,0,0,0,53641],[variable,644,32,2,183],[non_variable,5588,32,2,97]]
% Arr2: [[non_variable,5878,32,8,76],[non_variable,5588,32,2,97]]
%Error in the compression algorithm.
% Unzip Key1: [5878,5588,0,32,32,0,8,2,0,65502,644,65502]
% Unzip Key2: [5878,5588,0,32,32,0,8,2,0,65502,65502]
%Error (key): [76,119141,183,97] greater [76,97]
% Key1: <<22,246,21,212,0,0,64,0,232,225,0,255,254,2,133,255,254>>
% Key2: <<22,246,21,212,0,0,64,0,232,225,0,255,253>>
% Arr1: [[non_variable,5878,32,8,76],[non_variable,0,0,0,65501],[non_variable,0,0,0,53641],[variable,644,32,2,183],[non_variable,5588,32,2,97]]
% Arr2: [[non_variable,5878,32,8,76],[non_variable,5588,32,2,97]]
%Error in the compression algorithm.
% Unzip Key1: [5878,5588,0,32,32,0,8,2,0,65502,644,65502]
% Unzip Key2: [5878,5588,0,32,32,0,8,2,0,65502,65502]
%sort_key and compare returns different results.
% Data1: 004C 1D165 0387 0061
% Data2: 004C 0061

% non-ignorable:
%Error (compare): [40908,98] greater [64014,33]
%Error (key): [40908,98] greater [64014,33]
%Error (key): [40908,98] greater [64014,33]
% Key1: <<251,193,159,204,21,234,0,0,35,0,5>>
% Key2: <<251,65,250,14,2,94,0,0,35,0,5>>
% Arr1: [[non_variable,64449,32,2,0],[non_variable,40908,0,0,0],[non_variable,5610,32,2,98]]
% Arr2: [[non_variable,64321,32,2,64014],[non_variable,64014,0,0,64014],[variable,606,32,2,33]]
% Unzip Key1: [64449,40908,5610,0,32,32,0,2,2]
% Unzip Key2: [64321,64014,606,0,32,32,0,2,2]
% Data1: 9FCC 0062
% Data2: FA0E 0021
%
% variable:
%Error (compare): [40908,98] greater [64014,33]
%Error (key): [40908,98] greater [64014,33]
% Key1: <<251,193,159,204,21,234,0,0,35,0,5,0,255,226>>
% Key2: <<251,65,250,14,0,0,34,0,4,0,255,225,2,95>>
% Arr1: [[non_variable,64449,32,2,0],[non_variable,40908,0,0,0],[non_variable,5610,32,2,98]]
% Arr2: [[non_variable,64321,32,2,64014],[non_variable,64014,0,0,64014],[variable,606,32,2,33]]
% Unzip Key1: [64449,40908,5610,0,32,32,0,2,2,0,65502,65502,65502]
% Unzip Key2: [64321,64014,0,32,0,2,0,65502,65502,606]
% Data1: 9FCC 0062
% Data2: FA0E 0021

% non-ignorable:
%Error (compare): [19894,98] greater [40909,33]
%Error (key): [19894,98] greater [40909,33]
% Key1: <<251,192,205,182,21,234,0,0,35,0,5>>
% Key2: <<251,65,159,205,2,94,0,0,35,0,5>>
% Arr1: [[non_variable,64448,32,2,0],[non_variable,52662,0,0,0],[non_variable,5610,32,2,98]]
% Arr2: [[non_variable,64321,32,2,0],[non_variable,40909,0,0,0],[variable,606,32,2,33]]
% Unzip Key1: [64448,52662,5610,0,32,32,0,2,2]
% Unzip Key2: [64321,40909,606,0,32,32,0,2,2]
% Data1: 4DB6 0062
% Data2: 9FCD 0021
%
% shifted:
%Error (compare): [19894,98] greater [40909,33]
%Error (key): [19894,98] greater [40909,33]
% Key1: <<251,192,205,182,21,234,0,0,35,0,5,0,255,226>>
% Key2: <<251,65,159,205,0,0,34,0,4,0,255,225,2,95>>
% Arr1: [[non_variable,64448,32,2,0],[non_variable,52662,0,0,0],[non_variable,5610,32,2,98]]
% Arr2: [[non_variable,64321,32,2,0],[non_variable,40909,0,0,0],[variable,606,32,2,33]]
% Unzip Key1: [64448,52662,5610,0,32,32,0,2,2,0,65502,65502,65502]
% Unzip Key2: [64321,40909,0,32,0,2,0,65502,65502,606]
% Data1: 4DB6 0062
% Data2: 9FCD 0021


% Error (compare): [64041,98] greater [13312,33] 
% Error (key): [64041,98] greater [13312,33] 
%  Key1: <<251,65,250,41,21,234,0,0,35,0,5>> 
%  Key2: <<251,64,180,0,2,94,0,0,35,0,5>>
%  Arr1: [[non_variable,64321,32,2,64041],[non_variable,64041,0,0,64041],[non_variable,5610,32,2,98]] 
%  Arr2: [[non_variable,64320,32,2,0],[non_variable,46080,0,0,0],[variable,606,32,2,33]]
%  Unzip Key1: [64321,64041,5610,0,32,32,0,2,2] 
%  Unzip Key2: [64320,46080,606,0,32,32,0,2,2]
%  Data1: FA29 0062
%  Data2: 3400 0021

% Error (compare): [888,98] greater [19894,33]
% Error (key): [888,98] greater [19894,33]
%  Key1: <<251,192,131,120,21,234,0,0,35,0,5>>
%  Key2: <<251,64,205,182,2,94,0,0,35,0,5>>
%  Arr1: [[non_variable,64448,32,2,0],[non_variable,33656,0,0,0],[non_variable,5610,32,2,98]]
%  Arr2: [[non_variable,64320,32,2,0],[non_variable,52662,0,0,0],[variable,606,32,2,33]]
%  Unzip Key1: [64448,33656,5610,0,32,32,0,2,2]
%  Unzip Key2: [64320,52662,606,0,32,32,0,2,2]
%  Data1: 0378 0062
%  Data2: 4DB6 0021
%
% 2> ux_uca_utils:implicit_type(888).
% base3
% (ux@omicron)3> ux_uca_utils:implicit_type(19894).
% base1

selected_test_() ->
    Params1 = ux_uca_options:get_options(non_ignorable),
    Params2 = ux_uca_options:get_options(shifted),
    [{"Hangul tests",
        [?_assertEqual(compare2(Params1, [44032,33], [12910,33]), ok)
        ,?_assertEqual(compare2(Params1, [44032,63], [12910,63]), ok)
        ,?_assertEqual(compare2(Params1, [55176,4469,98], [55198,33]), ok)]}

    ,?_assertEqual(compare2(Params2, [108,119141,903,97], [108,97]), ok)
    ,?_assertEqual(compare2(Params2, [76,119141,903,97], [76,97]), ok)


    ,{"Implicit weight tests", 
        [?_assertEqual(compare2(Params1, [40908,98], [64014,33]), ok)
        ,?_assertEqual(compare2(Params2, [40908,98], [64014,33]), ok)

        ,?_assertEqual(compare2(Params1, [19894,98], [40909,33]), ok)
        ,?_assertEqual(compare2(Params2, [19894,98], [40909,33]), ok)

        %% 64031 is 16#FA29 (base1). 
        %% 13312 is 16#3400 (base1), non in ducet.
        %% ux_unidata:ducet([64041]). 
        %% [[non_variable,64321,32,2,64041],
        %%  [non_variable,64041,0,0,64041]]
        ,?_assertEqual(compare2(Params1, [64041,98], [13312,33]), ok)
        ,?_assertEqual(compare2(Params2, [64041,98], [13312,33]), ok)

        %% 19894 is 16#4DB6, not in ducet.
        ,?_assertEqual(compare2(Params1, [888,98], [19894,33]), ok)
        ,?_assertEqual(compare2(Params2, [888,98], [19894,33]), ok)]}
    ].


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
        ?_testTO(F(C2, Unsorted))},

     ?_assertEqual(ok, compare1(C1, "10X", "1000X")),
     ?_assertEqual(ok, compare2(C1, "10X", "1000X"))
    ].



%---------------------------------------------------------------
% SLOW TESTS 
%---------------------------------------------------------------
-ifdef(SLOW_TESTS).
non_ignorable_test_() ->
    {timeout, 1200, 
        fun() -> 
            prof(ux_unidata:open_test_file('collation_test_non_ignorable'), 
                ux_uca_options:get_options(non_ignorable), 
                1000000) 
        end}.

shifted_test_() ->
    {timeout, 1200, 
        fun() -> 
            prof(ux_unidata:open_test_file('collation_test_shifted'), 
                ux_uca_options:get_options(shifted), 
                1000000) end}.

















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
collation_test(_Fd, _F, _OldVal, StrNum, 0 = _Max, EC) ->
    io:format(user, "Only ~w strings were tested. Exit.~n", [StrNum]),
    EC; % max
% Read first string with data from file.
collation_test(Fd, P, false, StrNum, Max, EC) ->
    OldVal = read_line(Fd, StrNum),
    collation_test(Fd, P, OldVal, StrNum, Max, EC);
collation_test(Fd, Params, {OldFullStr, OldVal, StrNum}, _OldStrNum, Max, ErrorCounter) ->
    % Add new string.
    [io:format(user, "~n", []) || StrNum =:= 100],

    % Show message
    [io:format(user, "~w strings were tested. ~n", [StrNum]) 
        || StrNum rem 10000 =:= 0],

    case read_line(Fd, StrNum) of
    {FullStr, Val, NewStrNum} = Result when is_list(Val) -> 
        Res1 = compare1(Params, OldVal, Val),
        Res2 = compare2(Params, OldVal, Val),
        Res3 = merge_error(Res1, Res2),

        Res4 = 
        case {ux_string:to_nfd(OldVal), ux_string:to_nfd(Val)} of
            {OldVal, Val} -> 
                Res3;
            {Nfd1, Nfd2} ->
                compare2(Params, Nfd1, Nfd2)
        end,

        Res5 = merge_error(Res3, Res4),

        [io:format(user,
                "sort_key and compare returns different results.~n",
                []) || Res1 =/= Res2],

        [io:format(user,
                "Normalization affects on collation.~n",
                []) || Res2 =/= Res4],

        [io:format(user,
                " Data1: ~ts Data2: ~ts~n",
                [OldFullStr, FullStr]) || Res3 =:= error],

        collation_test(Fd, Params, Result, NewStrNum, Max - 1, 
                       ErrorCounter + error_code(Res5));
    _ -> ErrorCounter
    end.


read_line(Fd, StrNum) ->
    ux_uca_testdata:read_line(Fd, StrNum).


prof(Fd, Params, Count) ->
%    io:setopts(Fd,[{encoding,utf8}]),
    ErrorCount = collation_test(Fd, Params, false, 0, Count, 0),
    file:close(Fd),
    ?assertEqual(ErrorCount, 0).


nat_prof(Seq) ->
    Lists = [lists:flatten(io_lib:format("Abr~w", [X])) || X <- Seq],
    Params = ux_uca_options:get_options(
            [{alternate, non_ignorable}, {natural_sort, true}]
        ),
    {Time, SortedLists} = timer:tc(ux_uca, sort, [Params, Lists]),
    io:format(user, "~n Sort Time, ~.3gs ", [Time / 1000000]),
    ?_assertEqual(Lists, SortedLists).



compare1(Params, Val1, Val2) ->
    % 1. check compare/3.
    case ux_uca:compare(Params, Val1, Val2) of % collation compare
        lower -> ok;
        equal -> ok;
        greater -> 
            io:format(user, "Error (compare): ~w ~w ~w ~n", 
                [Val1, greater, Val2]),
                error
    end.

%% Val1 =< Val2
compare2(Params, Val1, Val2) ->
    % 2. check sort_key/2.
    Key1 = ux_uca:sort_key(Params, Val1),
    Key2 = ux_uca:sort_key(Params, Val2),
    if
        Key1 > Key2 ->
            io:format(user, "Error (key): ~w ~w ~w ~n", 
                [Val1, greater, Val2]),
            io:format(user,
                " Key1: ~w ~n Key2: ~w~n",
                [Key1, Key2]),
            
            Arr1 = ux_uca:sort_array(Params, Val1),
            Arr2 = ux_uca:sort_array(Params, Val2),
            io:format(user,
                " Arr1: ~w ~n Arr2: ~w~n",
                [Arr1, Arr2]),


            UCParams = ux_uca_options:get_options(Params, 
                        [{sort_key_format, uncompressed}]),
            UCKey1 = ux_uca:sort_key(UCParams, Val1),
            UCKey2 = ux_uca:sort_key(UCParams, Val2),
            if UCKey1 > UCKey2 -> ok; 
                true -> io:format(user, "Error in the compression algorithm.~n", []) end,

            io:format(user,
                " Unzip Key1: ~w ~n Unzip Key2: ~w~n",
                [UCKey1, UCKey2]),
            error;
        true -> ok
    end.


merge_error(ok, Acc) -> Acc;
merge_error(error, Acc) -> error.

error_code(ok) -> 0;
error_code(error) -> 1.

-endif. % TEST
