% vim: set filetype=erlang shiftwidth=4 tabstop=4 expandtab tw=80:
%%% =====================================================================
%%% This library is free software; you can redistribute it and/or modify
%%% it under the terms of the GNU Lesser General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This library is distributed in the hope that it will be useful, but
%%% WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
%%% Lesser General Public License for more details.
%%%
%%% You should have received a copy of the GNU Lesser General Public
%%% License along with this library; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
%%% USA
%%%
%%% $Id$
%%%
%%% @copyright 2010-2011 Michael Uvarov
%%% @author Michael Uvarov <freeakk@gmail.com>
%%% =====================================================================

%%% @doc String functions.


-module(ux_string).
-author('Uvarov Michael <freeakk@gmail.com>').

-export([
        html_special_chars/1,

        explode/2, explode/3,
        split/2, split/3,

        to_lower/1, to_upper/1, to_string/1,
        strip_tags/1, strip_tags/2, strip_tags/3,
        delete_types/2, delete_types/3, 
        filter_types/2, filter_types/3, 
        explode_types/2, split_types/2,
        first_types/3, last_types/3,

        script/1, scripts/1,

        freq/1, 
        is_nfc/1, is_nfd/1, is_nfkc/1, is_nfkd/1,
        to_nfc/1, to_nfd/1, to_nfkc/1, to_nfkd/1,
        to_ncr/1,

        to_graphemes/1, reverse/1,
        length/1, 
        first/2, last/2,

        extract_words/1,

        types/1]).

-include("ux.hrl").
-include("ux_string.hrl").
-include("ux_unidata.hrl").
-include("ux_char.hrl").



-define(ASSERT(TEST,TRUE,FALSE), case TEST of 
        true  -> TRUE; 
        false -> FALSE
end).

-define(ASSERT_IN_ARRAY_LAMBDA(TEST), case TEST of 
        true  -> fun lists:member/2; 
        false -> fun not_in_array/2
end).


% Sorry, but -import() is garbage :(

%% Returns Canonical_Combining_Class.
ccc(V) -> ?UNIDATA:ccc(V).
nfc_qc(V) -> ?UNIDATA:nfc_qc(V).
nfd_qc(V) -> ?UNIDATA:nfd_qc(V).
nfkc_qc(V) -> ?UNIDATA:nfkc_qc(V).
nfkd_qc(V) -> ?UNIDATA:nfkd_qc(V).
is_compat(V) -> ?UNIDATA:is_compat(V).
comp(V1, V2) -> ?UNIDATA:comp(V1, V2).
comp('skip_check') -> ?UNIDATA:comp('skip_check').
decomp(V) -> ?UNIDATA:decomp(V).











































%%
%% ==String functions based on the UNIDATA==
%%

%% @doc Returns various "character types" which can be used 
%%      as a default categorization in implementations.
%%      Types:
%%      http://www.ksu.ru/eng/departments/ktk/test/perl/lib/unicode/UCDFF301.html#General%20Category
%% @end
%% ux_char:type(_) -> false.
types(Str) -> 
    Fun = ux_char:type(skip_check),
    lists:map(Fun, Str).



%% @doc Returns a new string which is made from the chars of Str 
%%      which are not a type from Types list.
%% @end
-spec delete_types([char_type()], string()) -> string() | none().

delete_types(Types, Str) -> 
    Fun = ux_char:type(skip_check),
    lists:filter(fun(El) -> 
            not lists:member(Fun(El), Types) 
        end, Str).

%% @doc Stops delete_type/2 after Limit deleted chars. If Limit &lt; 0, then
%%      stops after -Limit skipped chars.
%% @end
-spec delete_types([char_type()], string(), integer()) -> string() | none().

delete_types(Types, Str, Limit) when Limit > 0 ->
    lists:reverse(get_types(Types, Str, Limit, [], true, 
        fun not_in_array/2, 0, -1));
delete_types(Types, Str, Limit) when Limit < 0 ->
    lists:reverse(get_types(Types, Str, Limit, [], true, 
        fun not_in_array/2, 1,  0)).

%% @doc Returns a new string which is made from the chars of Str 
%%      which are a type from Types list.
% @end
-spec filter_types([char_type()], string()) -> string() | none().

filter_types(Types, Str) -> 
    Fun = ux_char:type(skip_check),
    lists:filter(fun(El) -> 
            lists:member(Fun(El), Types) 
        end, Str).

%% @doc Stops after -Limit skipped chars.
%% @end
-spec filter_types([char_type()], string(), integer()) -> string() | none().

filter_types(Types, Str, Limit) when Limit > 0 ->
    lists:reverse(
        get_types(Types, Str, Limit, [], true, 
            fun lists:member/2, -1, 0));
filter_types(Types, Str, Limit) when Limit < 0 ->
    lists:reverse(
        get_types(Types, Str, Limit, [], true, 
            fun lists:member/2,  0, 1)).

%% @doc If Len&lt;0, then gets first Len chars of type, which is in Types
%%      If Len&gt;0, then gets first -Len chars of type, which is NOT in Types
%% @end
-spec first_types([char_type()], string(), integer()) -> string() | none().
first_types(Types, Str, Len) -> 
    lists:reverse(
        get_types(Types, Str, Len, [], false, 
            ?ASSERT_IN_ARRAY_LAMBDA(Len>0), 
            ?ASSERT(Len>0, -1, 1), 0)).

%% @doc If Len&lt;0, then gets last Len chars of type, which is in Types
%%      If Len&gt;0, then gets last -Len chars of type, which is NOT in Types
%% @end
-spec last_types([char_type()], string(), integer()) -> string() | none().
last_types(Types, Str, Len) -> 
    get_types(Types, lists:reverse(Str), Len, [], false, 
        ?ASSERT_IN_ARRAY_LAMBDA(Len>0), 
        ?ASSERT(Len>0, -1, 1), 0).
        
%% @private
%% @doc Return list of chars, for which Fun(CharType) return true.
%%      If Len = 0, then return a part of modified string concatinated with
%%      a tail of this string.
%%      If Fun(Char) return true then Len = Len + TrueStep else Len = Len +
%%      FalseStep.
%%      A returned list is reversed.
%% @end
get_types(_Types, [] = _Str, _ = _Len, Result, _, _, _, _) -> Result;
get_types(_,  _, 0, Result, false, _, _, _) -> Result;
get_types(_,  Tail, 0, Result, true, _, _, _) -> 
    lists:reverse(Tail)++Result;
get_types(Types, [Char|Tail], 
    Len, % Stop after Len chars
    Result, % Result array
    RetTail, % Concat tail with Result or not
    Fun, % Check function
    TrueStep, % Len+TrueStep, if Fun return true
    FalseStep) -> 
    case Fun(ux_char:type(Char), Types) of
    true  -> get_types(Types, Tail, Len+TrueStep, [Char|Result], 
                RetTail, Fun, TrueStep, FalseStep);
    false -> get_types(Types, Tail, Len+FalseStep, Result, 
                RetTail, Fun, TrueStep, FalseStep)
    end.

%% @doc Returns a new list of strings which are parts of Str splited 
%%      by separator chars of a type from Types list.
%% @end
-spec explode_types([char_type()], string()) -> string().

explode_types(Types, Str) -> 
    explode_reverse(explode_types_cycle(Types, Str, [], [])).

%% @private
explode_types_cycle(_Types, [], [], Res) -> Res;
explode_types_cycle(_Types, [], [_|_] = Buf, Res) -> [Buf|Res];
explode_types_cycle(Types, [Char|Str], Buf, Res) -> 
    case lists:member(ux_char:type(Char), Types) of
    true  -> explode_types_cycle(Types, Str, [], [Buf|Res]);
    false -> explode_types_cycle(Types, Str, [Char|Buf], Res)
    end.

%% @doc Returns a new list of strings which are parts of Str splited 
%%      by separator chars of a type from Types list. Parts can not be
%%      empty.
%% @end 
-spec split_types([char_type()], string()) -> string().

split_types(Types, Str) -> delete_empty(explode_types(Types, Str)).

%% @doc Deletes all empty lists from List.
%%      Example:
%%      `delete_empty([ [], "test", [1] ]) -> ["test", [1]].'
%% @end
-spec delete_empty([T]) -> [T].

delete_empty([])        -> [];
delete_empty([[]|List]) -> delete_empty(List);
delete_empty([El|List]) -> [El|delete_empty(List)].

%% @doc Converts something to string (list).
-spec to_string(string() | atom() | integer()) -> string().

to_string(Str) when is_list(Str) -> Str;
to_string(Str) when is_atom(Str) -> erlang:atom_to_list(Str);
to_string(Str) when is_integer(Str) -> [Str].

split(P1, P2)     -> delete_empty(explode(P1, P2)).
split(P1, P2, P3) -> delete_empty(explode(P1, P2, P3)).

%% @doc Counts how many identical chars in the string.
%%      Returns a dict.
%%      Example:
%%      ```
%% >dict:to_list(ux_string:freq("FFDF")).
%% [{70,3},{68,1}]'''
%%
%% @end
-spec freq(string()) -> dict(). 

freq(Str) -> do_freq(Str, dict:new()).

%% @private
do_freq([Char|Str], Dict) -> do_freq(Str, dict:update_counter(Char, 1, Dict));
do_freq([], Dict)         -> Dict.































%%
%% ==PHP-style functions==
%%

%% @doc Splits the string by delimeters.
-spec explode([string()], string()) -> [string()];
        (char(), string()) -> [string()];
        (string(), string()) -> [string()].

explode([Delimeter], [_|_] = Str) when is_integer(Delimeter) -> 
    explode_simple(Delimeter, lists:reverse(Str), [], []);
explode(Delimeter, [_|_] =  Str) when is_integer(Delimeter) -> 
    explode_simple(Delimeter, lists:reverse(Str), [], []);
explode([_|_] = Delimeter, [_|_] = Str) -> 
    case explode_cycle(Delimeter, Str, [], []) of
    false -> [Str];
    Res -> explode_reverse(Res)
    end;
explode([], _) -> false;
explode(_, []) -> [].



-spec explode([string()], string(), integer()) -> string();
        (char(), string(), integer()) -> [string()];
        (string(), string(), integer()) -> [string()].

explode(Delimeter, [_|_] = Str, Limit) when is_integer(Delimeter) ->
    explode([Delimeter], [_|_] = Str, Limit); 
explode([_|_] = Delimeter, [_|_] = Str, Limit) when Limit > 0 -> 
    explode_reverse(explode_cycle_pos(Delimeter, Str, [], [], Limit));
explode([_|_] = Delimeter, [_|_] = Str, Limit) when Limit < 0 -> 
    case explode_cycle(Delimeter, Str, [], []) of
    false -> [];
    Res -> explode_reverse(lists:nthtail(-Limit, Res))
    end;
explode([_|_] = Delimeter, [_|_] = Str, 0) -> explode(Delimeter, Str);
explode([], _, _) -> false;
explode(_, [], _) -> [].





%% @private
explode_reverse(Res) -> lists:map(fun lists:reverse/1, lists:reverse(Res)). 

%% @doc Simple and fast realization.
%%      Delimeter is one char.
%%      Str is reversed string.
%% @end
%% @private
explode_simple(Delimeter, [H|T], Buf, Res) when H == Delimeter ->
    explode_simple(Delimeter, T, [     ], [Buf|Res]);
explode_simple(Delimeter, [H|T], Buf,     Res) ->
    explode_simple(Delimeter, T, [H|Buf], Res);
explode_simple(_        , [   ], [     ], Res) -> Res;
explode_simple(_        , [   ], Buf,     Res) -> [Buf | Res].

%% @doc This function puts a part of the string before the delimeter in Buf, 
%%      if the delimeter is a substring of Str, then return Buf.
%%      Buf is a reversed list of reversed parts of the string.
%%      Return false, if Delimeter is not a part of Str.
%% @end
%% @private
explode_cycle(_, [], _,   [])     -> false;
explode_cycle(_, [], Buf, Result) -> [Buf | Result];
explode_cycle(Delimeter, Str, Buf, Result) ->
    case explode_check(Delimeter, Str) of
    false -> [C|Tail] = Str, 
        explode_cycle(Delimeter, Tail, [C|Buf], Result);
    Tail -> explode_cycle(Delimeter, Tail, [], [Buf | Result])
    end.

%% @private
explode_cycle_pos(_, [], Buf, Result, _) -> [Buf|Result];
explode_cycle_pos(_, [_|_] = Str, _, Result, 1) -> [lists:reverse(Str)|Result];
explode_cycle_pos(Delimeter, [_|_] = Str, Buf, Result, Limit) ->
    case explode_check(Delimeter, Str) of
    false -> [C|Tail] = Str, 
        explode_cycle_pos(Delimeter, Tail, [C|Buf], Result, 
            Limit);
    Tail  -> explode_cycle_pos(Delimeter, Tail, [], [Buf|Result], 
            Limit-1)
    end.

%% @doc This function get the delimeter and the part of the string.
%%      If (Str = Delimeter + Tail), return a Tail, else return 'false'.
%% @end
%% @private
explode_check([], Tail) ->
    Tail;
explode_check([Delimeter], Str) when is_list(Delimeter) ->
    explode_check(Delimeter, Str);    
explode_check([Delimeter|DelArr], Str) when is_list(Delimeter) ->
    case explode_check(Delimeter, Str) of
    false  -> explode_check(DelArr, Str);
    Result -> Result 
    end;
explode_check([DelHead|DelTail], [Head|Tail]) when (DelHead == Head) ->
    explode_check(DelTail, Tail);
explode_check(_, _) ->
    false.

%% @doc Converts characters of a string to a lowercase format.
-spec to_lower(string()) -> string().

to_lower(Str) ->
    Fun = ux_char:to_lower(skip_check),
    lists:map(Fun, Str).

%% @doc Converts characters of a string to a uppercase format.
-spec to_upper(string()) -> string().

to_upper(Str) ->
    Fun = ux_char:to_upper(skip_check),
    lists:map(Fun, Str).

%% @doc Encodes html special chars.
-spec html_special_chars(string()) -> string().

html_special_chars(Str) -> hsc(Str).

%% @see ux_string:htmlspecialchars/1
%% @private
-spec hsc(string()) -> string().

hsc(Str) -> hsc(Str, []).

%% @private
hsc([      ], Buf) -> lists:reverse(Buf);
hsc([$" | T], Buf) -> hsc(T, lists:reverse("&quot;", Buf));
hsc([$' | T], Buf) -> hsc(T, lists:reverse("&#39;", Buf));
hsc([$& | T], Buf) -> hsc(T, lists:reverse("&amp;", Buf));
hsc([$< | T], Buf) -> hsc(T, lists:reverse("&lt;", Buf));
hsc([$> | T], Buf) -> hsc(T, lists:reverse("&gt;", Buf));
hsc([H  | T], Buf) -> hsc(T, [H|Buf]).

%% @doc Deletes tags from the string.
%%
%%      Example: 
%%   ```> ux_string:strip_tags("<b>some string</b>").
%%      "some string"
%%      > ux_string:strip_tags("<h1>Head</h1><p>and paragraf</p>", ["h1"]).        
%%      "<h1>Head</h1>and paragraf"
%%      ux_string:strip_tags("<h1>Head</h1><p><!-- and paragraf --></p>", ["!--"]).
%%      "Head<!-- and paragraf -->"
%%      ux_string:st("a<br />b", [], " ").
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




%% @see ux_string:strip_tags/1
%% @private
st(Str) -> st_cycle(Str, [], 0, []).
%% @see ux_string:strip_tags/2
%% @private
st(Str, []) -> st(Str); 
st(Str, [$<|Allowed]) -> st(Str, tags_to_list(Allowed));
st(Str, Allowed) -> st(Str, Allowed, []). 
%% @see ux_string:strip_tags/3
%% @private
st(Str, [], []) -> st(Str); 
st(Str, [$<|Allowed], Alt) -> st(Str, tags_to_list(Allowed), Alt);
st(Str, [], Alt) -> st_cycle(Str, [], 0, lists:reverse(Alt)); 
st(Str, Allowed, Alt) -> 
    Fun = ux_char:to_lower(skip_check),
    st_cycle_with_allowed(Str, [],
        lists:map(fun lists:reverse/1,
            lists:map(Fun,
                lists:map(fun to_string/1, Allowed))), 
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


%% @private
not_in_array(X,Y) -> not lists:member(X,Y).


































%%
%% ==UNICODE NORMALIZATION FORMS==
%%
%% Unicode Standard Annex #15
%% http://unicode.org/reports/tr15/
%%

-spec is_nf(fun(), list(), integer(), atom(), fun()) -> yes | no | maybe.
is_nf(CCC, [Head|Tail], LastCC, Result, CheckFun) -> 
    case CCC(Head) of
    CC when (LastCC > CC) and (CC =/= 0) -> no;
    CC -> 
        case CheckFun(Head) of
        n -> no;
        m -> is_nf(CCC, Tail, CC, maybe,  CheckFun);
        y -> is_nf(CCC, Tail, CC, Result, CheckFun)
        end
    end;
is_nf(_CCC, [], _, Result, _) -> Result.



%% Detecting Normalization Forms
%% http://unicode.org/reports/tr15/#Detecting_Normalization_Forms
-spec is_nfc(list()) -> yes | no | maybe.

is_nfc(Str) when is_list(Str) -> 
    CCC = ccc('skip_check'),
    QC = nfc_qc('skip_check'),
    is_nf(CCC, Str, 0, yes, QC).


-spec is_nfd(list()) -> yes | no | maybe.

is_nfd(Str) when is_list(Str) -> 
    CCC = ccc('skip_check'),
    QC = nfd_qc('skip_check'),
    is_nf(CCC, Str, 0, yes, QC).


-spec is_nfkc(list()) -> yes | no | maybe.

is_nfkc(Str) when is_list(Str) ->
    CCC = ccc('skip_check'),
    QC = nfkc_qc('skip_check'),
    is_nf(CCC, Str, 0, yes, QC).


-spec is_nfkd(list()) -> yes | no | maybe.

is_nfkd(Str) when is_list(Str) ->
    CCC = ccc('skip_check'),
    QC = nfkd_qc('skip_check'),
    is_nf(CCC, Str, 0, yes, QC).





-spec to_nfc(list()) -> list().

to_nfc([])   -> [];
to_nfc(Str)  -> 
    case is_nfc(Str) of
    yes -> Str;
    _   -> get_composition(to_nfd(Str))
    end.


-spec to_nfkc(list()) -> list().

to_nfkc([])  -> [];
to_nfkc([_|_] = Str) -> get_composition(
        normalize(get_recursive_decomposition(false, Str))).


-spec to_nfd(list()) -> list().

to_nfd([])   -> [];
to_nfd([_|_] = Str) -> 
    normalize(get_recursive_decomposition(true,  Str)).


-spec to_nfkd(list()) -> list().

to_nfkd([])  -> [];
to_nfkd([_|_] = Str) -> 
    normalize(get_recursive_decomposition(false, Str)).




%% @doc internal_decompose(Str)
%% Canonical  If true bit is on in this byte, then selects the recursive 
%%            canonical decomposition, otherwise selects
%%            the recursive compatibility and canonical decomposition.
%% @end
%% @private
-spec get_recursive_decomposition(atom() | function(), list()) -> list().
get_recursive_decomposition(true, Str) -> 
    Canonical = is_compat(skip_check),
    Decomp = decomp(skip_check),
    get_recursive_decomposition(Decomp, Canonical, Str, []);

get_recursive_decomposition(false, Str) -> 
    Canonical = fun(_X) -> false end, % always false
    Decomp = decomp(skip_check),
    get_recursive_decomposition(Decomp, Canonical, Str, []);

get_recursive_decomposition(Canonical, Str) 
    when is_function(Canonical) -> 
    Decomp = decomp(skip_check),
    get_recursive_decomposition(Decomp, Canonical, Str, []).




% Skip ASCII
%% @private
-spec get_recursive_decomposition(fun(), fun(), list(), list()) -> list().
get_recursive_decomposition(Decomp, Canonical, [Char|Tail], Result) 
    when Char < 128 -> % Cannot be decomposed 
    get_recursive_decomposition(Decomp, Canonical, Tail,
    [Char|Result]);

%% @doc Decompose one char of hangul.
get_recursive_decomposition(Decomp, Canonical, [Char|Tail], Result)
    when (Char >= ?HANGUL_SBASE) and (Char =< ?HANGUL_SLAST) ->
    SIndex = Char - ?HANGUL_SBASE,
    L = ?HANGUL_LBASE + (SIndex div ?HANGUL_NCOUNT),
    V = ?HANGUL_VBASE + (SIndex rem ?HANGUL_NCOUNT) div ?HANGUL_TCOUNT,
    T = ?HANGUL_TBASE + (SIndex rem ?HANGUL_TCOUNT),
    get_recursive_decomposition(Decomp, Canonical, Tail,
        case T of
        ?HANGUL_TBASE -> [V|[L|Result]];
        _ -> [T|[V|[L|Result]]]
        end);

get_recursive_decomposition(Decomp, Canonical, [Char|Tail], Result) ->
    case Decomp(Char) of
    []  -> get_recursive_decomposition(Decomp, Canonical, Tail,
            [Char|Result]);
    Dec -> 
        case Canonical(Char) of % not is_compat = singleton
        true  -> 
            get_recursive_decomposition(Decomp, Canonical, Tail, 
                [Char|Result]);
        false -> 
            get_recursive_decomposition(Decomp, Canonical, Tail, 
                get_recursive_decomposition(Decomp, Canonical,
                    Dec, Result))
        end
    end;
get_recursive_decomposition(_, _, [], Result) -> Result.








%% @doc Normalize NFD or NFKD.
normalize(Str) -> 
    CCC = ccc('skip_check'),
    normalize1(CCC, Str, [], []).


%% @private
normalize1(_CCC, [], [ ], Result) -> 
    Result;

normalize1(CCC, [], [_|_]=Buf, Result) -> 
    normalize2(lists:reverse(Buf), Result);

normalize1(CCC, [Char|Tail], Buf, Result) ->
    Class = CCC(Char),
    if
        (Class == 0) and (Buf == []) -> 
            normalize1(CCC, Tail, [], [Char | Result]);
        (Class == 0) -> 
            normalize1(CCC, Tail, [], 
                [Char | normalize2(lists:reverse(Buf), Result)]);
        true -> normalize1(CCC, Tail, [{Class, Char} | Buf], Result)
    end.



%% @doc Append chars from Buf to Result in a right order.
%% @private
normalize2([], Result)  -> Result;
normalize2(Buf, Result) ->
    case normalize3(Buf, false, 0) of
    false -> Result;
    {_, Char} = Value -> normalize2(Buf -- [Value], [Char|Result])
    end.

%% @doc Return char from Buf with max ccc.
%% @private
normalize3([{CharClass, _} = Value | Tail], _, MaxClass) 
    when CharClass > MaxClass -> 
    normalize3(Tail, Value, CharClass);
normalize3([_|Tail], Value, MaxClass) -> 
    normalize3(Tail, Value, MaxClass);
normalize3([], Value, _) -> Value.




-define(COMP_CHAR_CLASS(Char),
       (case CCC(Char) of
            0 -> 0;
            _ -> 256
        end)).

%% @doc Internal Composition Function.
%% @private
get_composition([Char|Tail]) -> 
    CCC = ccc('skip_check'),
    COMP = comp('skip_check'),

    lists:reverse(
        get_composition(CCC, COMP, Tail, Char, 
            ?COMP_CHAR_CLASS(Char), [], [])
    ).


%% @doc Compose hangul characters.
%% 1. check to see if two current characters are L and V
%% 2. check to see if two current characters are LV and T
%% @end
%% @private
get_composition(CCC, COMP, [VChar |Tail], LChar, 0, [], Result) 
    when ?CHAR_IS_HANGUL_L(LChar)
     and ?CHAR_IS_HANGUL_V(VChar)
    ->
    LIndex = LChar - ?HANGUL_LBASE,
    VIndex = VChar - ?HANGUL_VBASE,
    LVChar = ?HANGUL_SBASE + ?HANGUL_TCOUNT  
           * (LIndex * ?HANGUL_VCOUNT + VIndex),

    case Tail of
    [TChar|Tail2] when ?CHAR_IS_HANGUL_T(TChar) ->
        TIndex = TChar - ?HANGUL_TBASE,
        LVTChar = LVChar + TIndex,
        Result3 = [LVTChar|Result],
        case Tail2 of
        [Char|Tail3] ->
            get_composition(CCC, COMP, Tail3, Char, 
                ?COMP_CHAR_CLASS(Char), [], Result3);
        [] -> Result3
        end;
    [Char|Tail2] ->
        get_composition(CCC, COMP, Tail2, Char, 
            ?COMP_CHAR_CLASS(Char), [], [LVChar|Result]);
    [] -> [LVChar|Result]
    end;

get_composition(CCC, COMP, [Char | Tail], LChar, 0, [], Result) 
    when ?CHAR_IS_HANGUL_L(LChar) ->
    get_composition(CCC, COMP, Tail, Char, 
        ?COMP_CHAR_CLASS(Char), [], [LChar|Result]);
                    
get_composition(CCC, COMP, [Char|Tail], LastChar, _, Mods, Result) 
    when Char < 128 ->
    NewResult = comp_append([LastChar|Result], Mods),
    get_composition(CCC, COMP, Tail, Char, 0, [], NewResult);

get_composition(CCC, COMP, [Char|Tail], LastChar, LastClass, Mods, Result) ->
    CharClass = ccc(Char),
    Comp = COMP(LastChar, Char),
    if
        (Comp =/= false) 
        and ((LastClass < CharClass) or (LastClass == 0)) ->
            get_composition(CCC, COMP, 
                Tail, Comp, LastClass, Mods, Result);

        (CharClass == 0) -> 
            NewResult = comp_append([LastChar|Result], Mods),
            get_composition(CCC, COMP, 
                Tail, Char, CharClass, [], NewResult);

        true -> 
            NewMods = [Char|Mods],
            get_composition(CCC, COMP, 
                Tail, LastChar, CharClass, NewMods, Result)
    end;

get_composition(_CCC, _Comp, [], Char, _LastClass, [], Result) ->
    [Char|Result];

get_composition(_CCC, _Comp, [], Char, _LastClass, Mods, Result) ->
    comp_append([Char|Result], Mods).





%% @doc Mods ++ Result.
%% @private
comp_append(Result, []) -> Result;
comp_append(Result, [_|_] = Mods) -> comp_append1(Result, lists:reverse(Mods)).

comp_append1(Result, [H|T]) -> comp_append1([H|Result], T);
comp_append1(Result, [   ]) -> Result.


%% @doc Convert everything from utf-8 into an NCR (Numeric Character Reference).
to_ncr(Str) -> to_ncr(lists:reverse(Str), []).

% FIXME: tail recursion
%% @private
to_ncr([Char|Tail], Res) -> to_ncr(Tail, ux_char:to_ncr(Char) ++ Res);
to_ncr([         ], Res) -> Res.

%% @doc Split unicode string into
%% [graphemes](http://en.wikipedia.org/wiki/Grapheme).
%% Based on
%% [UAX29: UNICODE TEXT SEGMENTATION]
%% (http://www.unicode.org/reports/tr29/#Grapheme_Cluster_Boundaries).
%%
%% It is important to recognize that what the user thinks of as 
%% a "character"—a basic unit of a writing system for a language—may 
%% not be just a single Unicode code point. Instead, that basic unit 
%% may be made up of multiple Unicode code points. 
%% To avoid ambiguity  with the computer use of the term character, 
%% this is called a user-perceived character. 
%% For example, “G” + acute-accent is a user-perceived character: 
%% users think of it as a single character, yet is actually represented 
%% by two Unicode code points. These user-perceived characters are 
%% approximated by what is called a grapheme cluster, which can be
%% determined programmatically.
%% @end
to_graphemes(Str) ->
    explode_reverse(to_graphemes_raw(Str)).

to_graphemes_raw(S) ->
    [H|T] = ux_gb:split('extended', S),
    Buf = [H],
    Res = [],
    
    to_graphemes_raw(T, Buf, Res).
    
%% @doc Returns not reversed result.
%% @private
-spec to_graphemes_raw(list(), string(), [string()]) ->
    [string()].
to_graphemes_raw(['x',H|T], Buf, Res) ->
    NewBuf = [H|Buf],
    to_graphemes_raw(T, NewBuf, Res);
to_graphemes_raw([H|T], Buf, Res) ->
    NewBuf = [H],
    NewRes = [Buf|Res],
    to_graphemes_raw(T, NewBuf, NewRes);
to_graphemes_raw([], Buf, Res) ->
    [Buf|Res].
    
    
    

%% @doc Compute count of graphemes in the string.
length(S) -> 
    BS = ux_gb:split('extended', S),
    do_length(BS, 0).

do_length(['x',H|T], Len) ->
    do_length(T, Len);
do_length([H|T], Len) ->
    do_length(T, Len + 1);
do_length([], Len) ->
    Len.

%% @doc Return Len chars from the beginning of the string.
first(Str, Len) ->
    lists:flatten(
        lists:sublist(to_graphemes(Str), Len)).

%% @doc Return Len chars from the beginning of the string.
last(Str, Len) ->
    lists:flatten(
        explode_reverse(
            lists:sublist(
                to_graphemes_raw(Str), Len))).

%% @doc Reverses the string graphemes.
reverse(Str) ->
    reverse_flatten(
        lists:reverse(to_graphemes_raw(Str)), 
        [], []).

%% [[1,2,3],[4,5,6]] => [6,5,4,3,2,1].
reverse_flatten([[_|_]=H|T], [], Res) ->
    reverse_flatten(T, H, Res);
reverse_flatten(T, [HH|TT],  Res) ->
    reverse_flatten(T, TT, [HH|Res]);
reverse_flatten(_, [], Res) ->
    Res.

































%%
%% Words
%%

extract_words(S) ->
    ux_wb:words(S).






%%
%% Script
%%

script(S) ->
    F = ux_char:script('skip_check'),
    do_script(F, S, dict:new()).
    
%% @private
do_script(F, [Char|Str], Dict) -> 
    Script  = F(Char),
    NewDict = dict:update_counter(Script, 1, Dict),
    do_script(F, Str, NewDict);
do_script(_F, [], Dict) -> 
    L = dict:to_list(Dict),
    max(L).

max(L) -> do_max(L, 0, false).

do_max([{S,N}|T], Max, _OldS)
    when N>Max
       , S=/='Common' ->
    do_max(T, N, S);
do_max([_|T], Max, S) ->
    do_max(T, Max, S);
do_max([], _Max, S) ->
    S.
    





scripts(S) ->
    F = ux_char:script('skip_check'),
    do_scripts(F, S, sets:new()).
    
%% @private
do_scripts(F, [H|T], Acc) -> 
    Script  = F(H),
    NewAcc = sets:add_element(Script, Acc),
    do_scripts(F, T, NewAcc);
do_scripts(_F, [], Dict) -> 
    sets:to_list(Dict).













































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


-endif.

