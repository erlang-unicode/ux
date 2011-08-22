-module(ux_html).
-export([
        encode/1,
        strip_tags/1, strip_tags/2, strip_tags/3]).


%% @doc Encodes html special chars.
-spec encode(string()) -> string().

encode(Str) -> encode(Str, []).

%% @private
encode([      ], Buf) -> lists:reverse(Buf);
encode([$" | T], Buf) -> encode(T, lists:reverse("&quot;", Buf));
encode([$' | T], Buf) -> encode(T, lists:reverse("&#39;", Buf));
encode([$& | T], Buf) -> encode(T, lists:reverse("&amp;", Buf));
encode([$< | T], Buf) -> encode(T, lists:reverse("&lt;", Buf));
encode([$> | T], Buf) -> encode(T, lists:reverse("&gt;", Buf));
encode([H  | T], Buf) -> encode(T, [H|Buf]).



%% @doc Deletes tags from the string.
%%
%%      Example: 
%%   ```> ux_html:strip_tags("<b>some string</b>").
%%      "some string"
%%      > ux_html:strip_tags("<h1>Head</h1><p>and paragraf</p>", ["h1"]).        
%%      "<h1>Head</h1>and paragraf"
%%      ux_html:strip_tags("<h1>Head</h1><p><!-- and paragraf --></p>", ["!--"]).
%%      "Head<!-- and paragraf -->"
%%      ux_html:st("a<br />b", [], " ").
%%      "a b"'''
%% @end
-spec strip_tags(string()) -> string().

strip_tags(Str) -> 
    st(Str, []).


-spec strip_tags(string, [string() | atom() | char()]) -> string().

strip_tags(Str, Allowed) -> 
    st(Str, Allowed).



-spec strip_tags(string, [string() | atom() | char()], string()) -> string().

strip_tags(Str, Allowed, Alt) -> 
    st(Str, Allowed, Alt).


%% @see ux_html:strip_tags/1
%% @private
st(Str) -> st_cycle(Str, [], 0, []).
%% @see ux_html:strip_tags/2
%% @private
st(Str, []) -> st(Str); 
st(Str, [$<|Allowed]) -> st(Str, tags_to_list(Allowed));
st(Str, Allowed) -> st(Str, Allowed, []). 
%% @see ux_html:strip_tags/3
%% @private
st(Str, [], []) -> st(Str); 
st(Str, [$<|Allowed], Alt) -> st(Str, tags_to_list(Allowed), Alt);
st(Str, [], Alt) -> st_cycle(Str, [], 0, lists:reverse(Alt)); 
st(Str, Allowed, Alt) -> 
    Fun = ux_char:to_lower(skip_check),
    st_cycle_with_allowed(Str, [],
        lists:map(fun lists:reverse/1,
            lists:map(Fun,
                lists:map(fun ux_string:to_string/1, Allowed))), 
        lists:reverse(Alt)).

%% @doc Drops all tags from the string.
%%   ```Cnt is a count of not closed <
%%      If we found <, then Cnt++
%%      If we found >, then Cnt--'''
%% @end
%% @private
st_cycle([$<| Tail], Buf, Cnt, Alt) -> st_cycle(Tail,        Buf, Cnt + 1, Alt);
st_cycle([$>| Tail], Buf, 1,   Alt) -> st_cycle(Tail, Alt ++ Buf, 0,       Alt);
st_cycle([$>| Tail], Buf, 0,   Alt) -> st_cycle(Tail,        Buf, 0,       Alt);
st_cycle([$>| Tail], Buf, Cnt, Alt) -> st_cycle(Tail,        Buf, Cnt - 1, Alt);
st_cycle([H | Tail], Buf, 0,   Alt) -> st_cycle(Tail, [H | Buf] , 0,       Alt);
st_cycle([_ | Tail], Buf, Cnt, Alt) -> st_cycle(Tail,        Buf, Cnt,     Alt);
st_cycle([        ], Buf, _,   _  ) -> lists:reverse(Buf).

%% @doc Is used by st_cycle_with_allowed
%% @private
%% If Flag = false, then don't append chars (as name of a tag <name>).
%% If Flag = true (default), then append chars (as the body of the tag).
%% Cnt is a level of subtag (`<a> Cnt=1 <b> Cnt=2 </b> Cnt=1</a>')
%% Returns: {tag_name, tag_body, string_tail}
st_get_tag([$>|T], Buf, Tag, _Flag, 1) ->
    {Tag, [$>|Buf], T};
st_get_tag([$>|T], Buf, Tag, _Flag, Cnt) ->
    st_get_tag(T, Buf, Tag, false, Cnt - 1);
st_get_tag([$<|T], Buf, Tag, false, Cnt) ->
    st_get_tag(T, Buf, Tag, false, Cnt + 1);
st_get_tag([$ |T], Buf, Tag, _, Cnt) ->
    st_get_tag(T, [$ |Buf], Tag, false, Cnt);
st_get_tag([$/|T], Buf, Tag, true, Cnt) ->
    st_get_tag(T, [$/|Buf], Tag, true, Cnt);
st_get_tag([H|T], Buf, Tag, true, Cnt) ->
    st_get_tag(T, [H|Buf], [H|Tag], true, Cnt);
% TODO: control atributes (onclick, for example. xss fix!)
st_get_tag([H|T], Buf, Tag, false, Cnt) ->
    st_get_tag(T, [H|Buf], Tag, false, Cnt);
st_get_tag([], _, _, _, _) -> false; 
st_get_tag(_, [], _, _, _) -> false. 

%% @doc Drops tags, but saves tags in the Allowed list.
%% @private
st_cycle_with_allowed([$<|T], Res, Allowed, Alt) ->
    case st_get_tag(T, [$<], [], true, 1) of 
    {Tag, SubStr, Tail} -> 
        case lists:member(string:to_lower(Tag), Allowed) of 
        true  -> st_cycle_with_allowed(Tail, 
            SubStr ++ Res, Allowed, Alt); % Allowed tag
        false -> st_cycle_with_allowed(Tail,
            Alt ++ Res, Allowed, Alt)  % Alt is replacement
       end;
    _ -> lists:reverse(Res) % deletes unclosed string 
    end;
st_cycle_with_allowed([$>|T], Res, Allowed, Alt) ->
    st_cycle_with_allowed(T, Res, Allowed, Alt);
st_cycle_with_allowed([Ch|T], Res, Allowed, Alt) ->
    st_cycle_with_allowed(T, [Ch | Res], Allowed, Alt);
st_cycle_with_allowed([], Res, _, _) -> lists:reverse(Res).

%% @doc Convert string of tags to list
%%      Example:
%%   ```> tags_to_list("<a><b>").
%%      ["a", "b"]'''
%% @end
%% @private
tags_to_list(Str) -> tags_to_list(Str, [], []).

%% @private
tags_to_list([$<|Str], Res, Buf) -> tags_to_list(Str, Res, Buf);
tags_to_list([$/|Str], Res, Buf) -> tags_to_list(Str, Res, Buf);
tags_to_list([$>|Str], Res, Buf) -> tags_to_list(Str, 
        [lists:reverse(Buf)|Res], []);
tags_to_list([Ch|Str], Res, Buf) -> tags_to_list(Str, Res, [Ch|Buf]);
tags_to_list([], Res, _) -> Res. 














































%%
%% Tests
%%

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


tags_to_list_test_() ->
    F = fun tags_to_list/1,
    [?_assertEqual(F("<a><b>"), ["b", "a"])
    ,?_assertEqual(F("<span>"), ["span"])
    ,?_assertEqual(F("<b><span>"), ["span", "b"])
    ,?_assertEqual(F("<i>"), ["i"])
    ].



html_special_chars_test_() ->
    M = 'ux_html',
    F = 'encode',
    [?_assertEqual(M:F("ddf2#$\""), "ddf2#$&quot;")
    ,?_assertEqual(M:F("test1 & test2"), "test1 &amp; test2")

    ,?_assertEqual(M:F(""), "")
    ].

strip_tags_test_() ->
    M = 'ux_html',
    F = 'strip_tags',
    [?_assertEqual(M:F("<b>a</b>"), "a")
    ,?_assertEqual(M:F("<b>a b c</b>"), "a b c")
    ,?_assertEqual(M:F("<b >a b c</b>"), "a b c")
    ,?_assertEqual(M:F("<b>a b c</b >"), "a b c")
    ,{"Check a long tag."
        ,[?_assertEqual(M:F("<H1>A B C</H1>"), "A B C")
         ,?_assertEqual(M:F("a<img src='i.img' />b"), "ab")]}
    ,{"Check allowed tags."
        ,[?_assertEqual(M:F("<b>a b c</b>", ["b"]), "<b>a b c</b>")
         ,?_assertEqual(M:F("<B>a b c</B>", ["b"]), "<B>a b c</B>")
         ,?_assertEqual(M:F("<code>a b c</code>", ["b"]), "a b c")
         ,?_assertEqual(M:F("<code>a b c</code>", ["b", "code"]), "<code>a b c</code>")
         ,?_assertEqual(M:F("<span>a b c</span>", ["b", "span"]), "<span>a b c</span>")
         ]}
    ,{"Check a tag with an attribute."
        ,[?_assertEqual(M:F("a<img src='i.gif' />b", ["b"]), "ab")
         ,?_assertEqual(M:F("a<img src='i.gif' />b", ["img"]), "a<img src='i.gif' />b")
         ,?_assertEqual(M:F("a<br/>b", ["br"]), "a<br/>b")]}
    ,{"Check an atom in the list allowed tags."
        ,[?_assertEqual(M:F("a<br/>b", [br]), "a<br/>b")
         ,?_assertEqual(M:F("a<br/><b>b</b>", [br]), "a<br/>b")]}
    ,{"Check a replacement argument."
        ,[?_assertEqual(M:F("<b>a b c</b>", [], " "), " a b c ")
         ,?_assertEqual(M:F("<b>a b c</b>", [], "tag"), "taga b ctag")
         ,?_assertEqual(M:F("<b>a b c</b>", [test], "tag"), "taga b ctag")]}
    ,{"PHP format."
        ,[?_assertEqual(M:F("<b>a b c</b>", "<b>"), "<b>a b c</b>")
         ,?_assertEqual(M:F("<span>a b c</span>", "<b><span>"), "<span>a b c</span>")
         ,?_assertEqual(M:F("<a><b>test<a", "a"), "<a>test")
         ,?_assertEqual(M:F("<a ><b>test<a", "<a>"), "<a >test")]}

    ,{"Empty string."
        ,[?_assertEqual(M:F("", ""), "")
         ,?_assertEqual(M:F("", "<b><span>"), "")
         ,?_assertEqual(M:F("", "a"), "")
         ,?_assertEqual(M:F("<", "<a>"), "")]}
    ].

-endif.
