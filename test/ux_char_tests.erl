-module(ux_char_tests).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include("ux_tests.hrl").
    

is_letter_test_() ->
    M = 'ux_char',
    F = 'is_letter',
    [?_assertTO(not M:F($ ))
    ,?_assertTO(not M:F($-))
    ,?_assertTO(not M:F($1))
    ,?_assertTO(M:F($r))
    ,?_assertTO(M:F($G))
    ,?_assertTO(M:F(1099))
    ].

is_ascii_test_() ->
    M = 'ux_char',
    F = 'is_acsii',
    [?_assertTO(M:F($ ))
    ,?_assertTO(M:F($-))
    ,?_assertTO(M:F($1))
    ,?_assertTO(M:F($r))
    ,?_assertTO(M:F($G))
    ,?_assertTO(not M:F(1099))
    ].

is_lower_test_() ->
    M = 'ux_char',
    F = 'is_lower',
    [?_assertTO(not M:F($ ))
    ,?_assertTO(not M:F($-))
    ,?_assertTO(not M:F($1))
    ,?_assertTO(M:F($r))
    ,?_assertTO(not M:F($G))
    ,?_assertTO(M:F(1099))
    ].


is_upper_test_() ->
    M = 'ux_char',
    F = 'is_upper',
    [?_assertTO(not M:F($ ))
    ,?_assertTO(not M:F($-))
    ,?_assertTO(not M:F($1))
    ,?_assertTO(not M:F($r))
    ,?_assertTO(M:F($G))
    ,?_assertTO(not M:F(1099))
    ].

is_punctuation_mark_test_() ->
    M = 'ux_char',
    F = 'is_punctuation_mark',
    [?_assertTO(not M:F($ ))
    ,?_assertTO(M:F($-))
    ,?_assertTO(not M:F($1))
    ,?_assertTO(not M:F($r))
    ,?_assertTO(not M:F($G))
    ,?_assertTO(not M:F(1099))
    ].

is_decimal_test_() ->
    M = 'ux_char',
    F = 'is_decimal',
    [?_assertTO(not M:F($ ))
    ,?_assertTO(not M:F($-))
    ,?_assertTO(M:F($1))
    ,?_assertTO(not M:F($r))
    ,?_assertTO(not M:F($G))
    ,?_assertTO(not M:F(1099))
    ].

is_number_test_() ->
    M = 'ux_char',
    F = 'is_number',
    [?_assertTO(not M:F($ ))
    ,?_assertTO(not M:F($-))
    ,?_assertTO(M:F($1))
    ,?_assertTO(not M:F($r))
    ,?_assertTO(not M:F($G))
    ,?_assertTO(not M:F(1099))
    ].

is_separator_test_() ->
    M = 'ux_char',
    F = 'is_separator',
    [?_assertTO(M:F($ ))
    ,?_assertTO(not M:F($-))
    ,?_assertTO(not M:F($1))
    ,?_assertTO(not M:F($r))
    ,?_assertTO(not M:F($G))
    ,?_assertTO(not M:F(1099))
    ].

-endif.
