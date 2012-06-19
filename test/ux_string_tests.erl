-module(ux_string_tests).

%%
%% Tests
%%

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include("ux_tests.hrl").



explode_test_() ->
    M = 'ux_string',
    F = 'explode',
    [?_assertEqual(M:F(":", "1:2:3"), ["1", "2", "3"])
    ,?_assertEqual(M:F(":", "aa::aa"), ["aa", "", "aa"])
    ,?_assertEqual(M:F(":", "aa::"), ["aa", "", ""])
    ,?_assertEqual(M:F("::", "aa::aa"), ["aa", "aa"])
    ,?_assertEqual(M:F("::", "aa:::aa"), ["aa", ":aa"])
    ,?_assertEqual(M:F("::", "aa:::"), ["aa", ":"])

    ,?_assertEqual(M:F([":", ";"], "aa:;:aa"), ["aa", "", "", "aa"])
    ,?_assertEqual(M:F([";:", ";"], "aa:;:aa"), ["aa:", "aa"])

    ,?_assertEqual(M:F($c, "dfsawcddcs"), ["dfsaw", "dd", "s"])
    ,?_assertEqual(M:F($c, "dfsawcddcs",2 ), ["dfsaw", "ddcs"])

    ,{"Limit>0",
        ?_assertEqual(M:F("|", "one|two|three|four", 2), ["one", "two|three|four"])}

    ,{"Limit<0",
        [?_assertEqual(M:F("|", "one|two|three|four", -1), ["one", "two", "three"])
        ,?_assertEqual(M:F("-", "one|two|three|four", -1), [])
        ,?_assertEqual(M:F("-", "one|two|three|four"), ["one|two|three|four"])
        ]}

    ,?_assertEqual(M:F("-", ""), [])
    % Empty delimeter. 
    % PHP behaviour: return false.
    % Erlang behaviour: throw error.
    ,{"Check an error in matching.",
        [?_assertError(function_clause, M:F("", "test"))
        ,?_assertError(function_clause, M:F("", ""))
        ,?_assertError(function_clause, M:F("", "", 0))
        ,?_assertError(function_clause, M:F("", "", 1))
        ,?_assertError(function_clause, M:F("", "", -1))
        ,?_assertError(function_clause, M:F("", "test", 0))
        ,?_assertError(function_clause, M:F("", "test", 1))
        ,?_assertError(function_clause, M:F("", "test", -1))
        ]}
    ].




























%%
%% With Unidata
%%

to_lower_test_() ->
    M = 'ux_string',
    F = 'to_lower',
    [?_assertEqualTO(M:F("small BIG"), "small big")
    ,?_assertEqualTO(M:F("You want your freedom?"), 
                         "you want your freedom?")
    % Russian text
    ,?_assertEqualTO(M:F([1069,1056,1051,1040,1053,1043]), 
                         [1101,1088,1083,1072,1085,1075])

    ,?_assertEqualTO(M:F(""), "")
    ].

to_upper_test_() ->
    M = 'ux_string',
    F = 'to_upper',
    [?_assertEqualTO(M:F("small BIG"), "SMALL BIG")
    ,?_assertEqualTO(M:F("I'm making a note here: HUGE SUCCESS."), 
                         "I'M MAKING A NOTE HERE: HUGE SUCCESS.")
    ,?_assertEqualTO(M:F([1101,1088,1083,1072,1085,1075]),
                         [1069,1056,1051,1040,1053,1043])

    ,?_assertEqualTO(M:F(""), "")
    ].

delete_types_test_() ->
    M = 'ux_string',
    F = 'delete_types',
    [?_assertEqualTO(M:F(['Ll', 'Lu'], "Tom Cat!"), " !")
    ,?_assertEqualTO(M:F(['Ll'], "Tom Cat!"), "T C!")
    ,?_assertEqualTO(M:F(['Po'], "Tom Cat!"), "Tom Cat")
    ,{"Skip 2 chars (A,B).",
        ?_assertEqualTO(M:F(['Ll'], "AaBbCc44ff", -2), "ABbCc44ff")}
    ,{"Delete only 2 chars (A,B).",
        ?_assertEqualTO(M:F(['Ll'], "AaBbCc44ff",  2), "ABCc44ff")}
    ,?_assertEqualTO(M:F(['Ll'], "AaBbCc44ffdsBAF",  4), "ABC44fdsBAF")
    ,?_assertEqualTO(M:F(['Ll'], "AaBbCc44ffdsBAF", -4), "ABC44ffdsBAF")

    ,?_assertEqualTO(M:F(['Ll'], "cat"), "")
    ,?_assertEqualTO(M:F(['Ll'], ""), "")
    ,?_assertEqualTO(M:F([], ""), "")
    ].

filter_types_test_() ->
    M = 'ux_string',
    F = 'filter_types',
    [?_assertEqualTO(M:F(['Ll', 'Lu'], "Tom Cat!"), "TomCat")
    ,?_assertEqualTO(M:F(['Ll'], "Tom Cat!"), "omat")
    ,?_assertEqualTO(M:F(['Po'], "Tom Cat!"), "!")
    ,?_assertEqualTO(M:F(['Ll'], "AaBbCc44ffds",  3), "abc44ffds")
    ,?_assertEqualTO(M:F(['Ll'], "AaBbCc44ffds",  4), "abcffds")
    ,?_assertEqualTO(M:F(['Ll'], "AaBbCc44ffds", -2), "abCc44ffds")
    ,?_assertEqualTO(M:F(['Ll'], "AaBbCc44ffds", -4), "abc4ffds")

    ,?_assertEqualTO(M:F(['Lu'], "cat"), "")
    ,?_assertEqualTO(M:F(['Lu'], ""), "")
    ,?_assertEqualTO(M:F([], ""), "")
    ].

types_test_() ->
    M = 'ux_string',
    F = 'types',
    [?_assertEqualTO(M:F("Tom Cat!"), ['Lu','Ll','Ll','Zs','Lu','Ll','Ll','Po'])
    ,?_assertEqualTO(M:F(""), [])
    %,?_assertEqual(M:F(), )
    ].

last_types_test_() ->
    M = 'ux_string',
    F = 'last_types',
    [?_assertEqualTO(M:F(['Ll'], "AavbfFDsdfffd9s9999", -5), "99999")
    ,?_assertEqualTO(M:F(['Ll'], "AavbfFDsdfffd9s9999", -6), "D99999")
    ,?_assertEqualTO(M:F(['Ll'], "AavbfFDsdfffd9s9999", -7), "FD99999")
    ,?_assertEqualTO(M:F(['Ll'], "AavbfFDsdfffd9s9999", -8), "AFD99999")
    ,?_assertEqualTO(M:F([], "", -5), [])
    ].

first_types_test_() ->
    M = 'ux_string',
    F = 'first_types',
    [?_assertEqualTO(M:F(['Ll'], "AavbfFDsdfffds", 4), "avbf")
    ,?_assertEqualTO(M:F(['Ll'], "AavbfFDsdfffds", 5), "avbfs")
    ,?_assertEqualTO(M:F([], "", 5), [])
    ].




to_graphemes_test_() ->
    M = 'ux_string',
    F = 'to_graphemes',
    [{"Simple example", 
        ?_assertEqualTO(M:F("Octocat!"), ["O","c","t","o","c","a","t","!"])},
     {"U+1EE5 LATIN SMALL LETTER U WITH DOT BELOW, U+031B COMBINING HORN, a, b",
        ?_assertEqualTO(M:F([16#1EE5, 16#031B, $a, $b]), [[7909,795],"a","b"])},
     ?_assertEqualTO(M:F(""), [])
    ].

first_test_() ->
    M = 'ux_string',
    F = 'first',
    [?_assertEqualTO(M:F("Octocat!", 4), "Octo")
    ,?_assertEqualTO(M:F("", 4), "")
    ,?_assertEqualTO(M:F("cat", 4), "cat")
    ].

last_test_() ->
    M = 'ux_string',
    F = 'last',
    [?_assertEqualTO(M:F("Octocat!", 4), "cat!")
    ,?_assertEqualTO(M:F("", 4), "")
    ,?_assertEqualTO(M:F("cat", 4), "cat")
    ].

length_test_() ->
    M = 'ux_string',
    F = 'length',
    [?_assertEqualTO(M:F("Octo"), 4)
    ,?_assertEqualTO(M:F(""), 0)
    ].


script_test_() ->
    M = 'ux_string',
    F = 'script',
    [?_assertEqualTO(M:F("Octocat!"), 'Latin')
    ,?_assertEqualTO(M:F([1086,1082,1090,1086,1082,1101,1090]), 'Cyrillic')
    ,?_assertEqualTO(M:F(""), false)
    ].



scripts_test_() ->
    M = 'ux_string',
    F = 'scripts',
    S = fun lists:sort/1,
    [?_assertEqualTO(S(M:F("Octocat!")), S(['Latin','Common']))
    ,?_assertEqualTO(M:F([1086,1082,1090,1086,1082,1101,1090]), ['Cyrillic'])
    ,?_assertEqualTO(M:F(""), [])
    ].



to_nfc_test_() ->
    M = 'ux_string',
    F = 'to_nfc',
    [?_assertEqualTO(M:F(""), "")
    ].

to_nfd_test_() ->
    M = 'ux_string',
    F = 'to_nfd',
    [?_assertEqualTO(M:F(""), "")
    ].

to_nfkc_test_() ->
    M = 'ux_string',
    F = 'to_nfkc',
    [?_assertEqualTO(M:F(""), "")
    ].

to_nfkd_test_() ->
    M = 'ux_string',
    F = 'to_nfkd',
    [?_assertEqualTO(M:F(""), "")
    ].





is_nfc_test_() ->
    M = 'ux_string',
    F = 'is_nfc',
    [?_assertEqualTO(M:F(""), 'yes')
    ].

is_nfd_test_() ->
    M = 'ux_string',
    F = 'is_nfd',
    [?_assertEqualTO(M:F(""), 'yes')
    ].

is_nfkc_test_() ->
    M = 'ux_string',
    F = 'is_nfkc',
    [?_assertEqualTO(M:F(""), 'yes')
    ].

is_nfkd_test_() ->
    M = 'ux_string',
    F = 'is_nfkd',
    [?_assertEqualTO(M:F(""), 'yes')
    ].
















-ifdef(SLOW_TESTS).

nfc_test_() ->
    {timeout, 300, 
        {"Normalization Conformance Test", 
            fun() -> 
                nfc_prof(1000000),
                io:format(user, "~n", []) end}}.




%% @doc Normalization Conformance Test
%% http://unicode.org/reports/tr41/tr41-7.html#Tests15
%%
%%    NFC
%%      c2 ==  NFC(c1) ==  NFC(c2) ==  NFC(c3)
%%      c4 ==  NFC(c4) ==  NFC(c5)
%%
%%    NFD
%%      c3 ==  NFD(c1) ==  NFD(c2) ==  NFD(c3)
%%      c5 ==  NFD(c4) ==  NFD(c5)
%%
%%    NFKC
%%      c4 == NFKC(c1) == NFKC(c2) == NFKC(c3) == NFKC(c4) == NFKC(c5)
%%
%%    NFKD
%%      c5 == NFKD(c1) == NFKD(c2) == NFKD(c3) == NFKD(c4) == NFKD(c5)
%% @end
%% @private
nfc_test(_Fd, 0, StrNum) -> 
    io:format(user, "Only ~w strings were tested. Exit.~n", [StrNum]),
    ok;
nfc_test(Fd, Max, StrNum) ->
    % Show message
    case StrNum rem 1000 of
    0 -> io:format(user, "~n~w strings were tested. ", [StrNum]);
    _ -> next
    end,

    NFC  = fun ux_string:to_nfc/1,
    NFD  = fun ux_string:to_nfd/1,
    NFKC = fun ux_string:to_nfkc/1,
    NFKD = fun ux_string:to_nfkd/1,

    case file:read_line(Fd) of
    eof -> ok;
    {ok, Data} -> 
        try
        [LineWithoutComment|_] = ux_string:explode("#", Data),
        % Convert string from file to list of integers 
        lists:map(fun (Str) -> 
                lists:map(fun ux_unidata_parser:hex_to_int/1, 
                    string:tokens(Str, " ")) 
            end,
            ux_string:explode(";", LineWithoutComment))
        of 
        [C1,C2,C3,C4,C5,_] ->
            % start of the body
            % {Test info atom, Result from function, From, To}
            %NFD
            ?assertEqual({c3__nfd_c1, C3, C1, C3}, {c3__nfd_c1, NFD(C1), C1, C3}),
            ?assertEqual({c3__nfd_c2, C3, C2, C3}, {c3__nfd_c2, NFD(C2), C2, C3}),
            ?assertEqual({c3__nfd_c3, C3, C3, C3}, {c3__nfd_c3, NFD(C3), C3, C3}),
            ?assertEqual({c3__nfd_c4, C5, C4, C5}, {c3__nfd_c4, NFD(C4), C4, C5}),
            ?assertEqual({c3__nfd_c5, C5, C5, C5}, {c3__nfd_c5, NFD(C5), C5, C5}),

            %NFC
            ?assertEqual({c2__nfc_c1, C2, C1, C2}, {c2__nfc_c1, NFC(C1), C1, C2}),
            ?assertEqual({c2__nfc_c2, C2, C2, C2}, {c2__nfc_c2, NFC(C2), C2, C2}),
            ?assertEqual({c2__nfc_c3, C2, C3, C2}, {c2__nfc_c3, NFC(C3), C3, C2}),
            ?assertEqual({c2__nfc_c4, C4, C4, C4}, {c2__nfc_c4, NFC(C4), C4, C4}),
            ?assertEqual({c2__nfc_c5, C4, C5, C4}, {c2__nfc_c5, NFC(C5), C5, C4}),

            %NFKC
            ?assertEqual({c4__nfkc_c1, C4, C1}, {c4__nfkc_c1, NFKC(C1), C1}),
            ?assertEqual({c4__nfkc_c2, C4, C2}, {c4__nfkc_c2, NFKC(C2), C2}),
            ?assertEqual({c4__nfkc_c3, C4, C3}, {c4__nfkc_c3, NFKC(C3), C3}),
            ?assertEqual({c4__nfkc_c4, C4, C4}, {c4__nfkc_c4, NFKC(C4), C4}),
            ?assertEqual({c4__nfkc_c5, C4, C5}, {c4__nfkc_c5, NFKC(C5), C5}),

            %NFKD
            ?assertEqual({c5__nfkd_c1, C5, C1}, {c5__nfkd_c1, NFKD(C1), C1}),
            ?assertEqual({c5__nfkd_c2, C5, C2}, {c5__nfkd_c2, NFKD(C2), C2}),
            ?assertEqual({c5__nfkd_c3, C5, C3}, {c5__nfkd_c3, NFKD(C3), C3}),
            ?assertEqual({c5__nfkd_c4, C5, C4}, {c5__nfkd_c4, NFKD(C4), C4}),
            ?assertEqual({c5__nfkd_c5, C5, C5}, {c5__nfkd_c5, NFKD(C5), C5});

            % end of the body
        _ -> next
        catch error:_ -> next
        after 
            nfc_test(Fd, Max - 1, StrNum + 1)
        end
    end.

nfc_prof(Count) ->
    Fd = ux_unidata:open_test_file('normalization_test'),
    io:setopts(Fd,[{encoding,utf8}]),
    nfc_test(Fd, Count, 0),
    file:close(Fd),
    ok.


-endif. % SLOW_TESTS
-endif. % TEST
