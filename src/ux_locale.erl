-module(ux_locale).

    
%%% (ux@delta)32> ux_unicode_language_identifier:decode('unicode-language-id', "en_GB").
%%% {ok,[["en",[]],[],[[95,"GB"]],[]],[]}
%%% (ux@delta)33> ux_unicode_language_identifier:decode('unicode-language-id', "es-419").
%%% {ok,[["es",[]],[],[[45,"419"]],[]],[]}

%%% (ux@delta)44> ux_unicode_locale_identifier:decode('unicode-locale-id', "es-419").
%%% {ok,[[["es",[]],[],[[45,"419"]],[]],[]],[]}
%%% (ux@delta)45> ux_unicode_locale_identifier:decode('unicode-locale-id', "de_DE_u_co_phonebk").
%%% {ok,[[["de",[]],[],[[95,"DE"]],[]],
%%%     [[95,117,[[95,["co",[[95,"phonebk"]]]]]]]],
%%%    []}

    
decode(LocaleId) ->
    ux_unicode_locale_identifier:decode('unicode-locale-id', LocaleId).
%    ux_unicode_locale_identifier:decode('unicode-locale-id', "la_scri-RE_variant_variant2").
%{ok,[[["la",[]],
%      [[95,"scri"]],
%      [[45,"RE"]],
%      [[95,"variant"],[95,"variant2"]]],
%     []],
%    []}
%
%ux_unicode_locale_identifier:decode('unicode-locale-id', "la_scri-RE_variant_variant2_u_co_phonebk").
%{ok,[[["la",[]],
%      [[95,"scri"]],
%      [[45,"RE"]],
%      [[95,"variant"],[95,"variant2"]]],
%     [[95,117,[[95,["co",[[95,"phonebk"]]]]]]]],
%    []}
%
%ux_unicode_locale_identifier:decode('unicode-locale-id', "la-EXT_scri-RE_variant_variant2_u_co_phonebk").
%{ok,[[["la",[[45,["EXT",[]]]]],
%      [[95,"scri"]],
%      [[45,"RE"]],
%      [[95,"variant"],[95,"variant2"]]],
%     [[95,117,[[95,["co",[[95,"phonebk"]]]]]]]],
%    []}


