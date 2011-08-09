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
%%% @see ux
%%% @end
%%% =====================================================================

%%% @doc String functions.


-module(ux_string).
-author('Uvarov Michael <freeakk@gmail.com>').

-export([list_to_latin1/1,
        html_special_chars/1,

        explode/2, explode/3,
        split/2, split/3,

        to_lower/1, to_upper/1, to_string/1,
        strip_tags/1, strip_tags/2, strip_tags/3,
        delete_types/2, delete_types/3, 
        filter_types/2, filter_types/3, 
        explode_types/2, split_types/2,
        first_types/3, last_types/3,


% for utf-8

        freq/1, 
        is_nfc/1, is_nfd/1, is_nfkc/1, is_nfkd/1,
        to_nfc/1, to_nfd/1, to_nfkc/1, to_nfkd/1,
        to_ncr/1,

        to_graphemes/1, reverse/1,
        length/1, len/1,
        first/2, last/2,

        info/1,
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
decomp(V) -> ?UNIDATA:decomp(V).


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
-spec delete_types([char_type()], string()) -> string().

delete_types(Types, Str) -> 
    Fun = ux_char:type(skip_check),
    lists:filter(fun(El) -> 
            not lists:member(Fun(El), Types) 
        end, Str).

%% @doc Stops delete_type/2 after Limit deleted chars. If Limit &lt; 0, then
%%      stops after -Limit skipped chars.
%% @end
-spec delete_types([char_type()], string(), integer()) -> string().

delete_types(Types, Str, Limit) when Limit > 0 ->
    lists:reverse(get_types(Types, Str, Limit, [], true, 
        fun not_in_array/2, 0, -1));
delete_types(Types, Str, Limit) when Limit < 0 ->
    lists:reverse(get_types(Types, Str, Limit, [], true, 
        fun not_in_array/2, 1,  0)).

%% @doc Returns a new string which is made from the chars of Str 
%%      which are a type from Types list.
% @end
-spec filter_types([char_type()], string()) -> string().

filter_types(Types, Str) -> 
    Fun = ux_char:type(skip_check),
    lists:filter(fun(El) -> 
            lists:member(Fun(El), Types) 
        end, Str).

%% @doc Stops after -Limit skipped chars.
%% @end
-spec filter_types([char_type()], string(), integer()) -> string().

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
-spec first_types([char_type()], string(), integer()) -> string().
first_types(Types, Str, Len) -> 
    lists:reverse(
        get_types(Types, Str, Len, [], false, 
            ?ASSERT_IN_ARRAY_LAMBDA(Len>0), 
            ?ASSERT(Len>0, -1, 1), 0)).

%% @doc If Len&lt;0, then gets last Len chars of type, which is in Types
%%      If Len&gt;0, then gets last -Len chars of type, which is NOT in Types
%% @end
-spec last_types([char_type()], string(), integer()) -> string().
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
    case apply(Fun, [ux_char:type(Char), Types]) of
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

%% @doc Splits the string by delimeters.
-spec explode([string()], string()) -> string().
-spec explode([string()], string(), integer()) -> string().

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

%% @doc This function get a delimeter and a part of the string 
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
-spec strip_tags(string, [string() | atom() | char()]) -> string().

strip_tags(Str) -> st(Str, []).

strip_tags(Str, Allowed) -> st(Str, Allowed).
strip_tags(Str, Allowed, Alt) -> st(Str, Allowed, Alt).

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

%% @doc Counts a letter frequency
-spec freq(string()) -> dict(). 

freq(Str) -> freq_1(Str, dict:new()).

%% @private
freq_1([Char|Str], Dict) -> freq_1(Str, dict:update_counter(Char, 1, Dict));
freq_1([], Dict)         -> Dict.

%------------------------------------------------------------------------------
%    %  %    %  %%%%%%
%    %  %%   %  %
%    %  % %  %  %%%%%
%    %  %  % %  %
%    %  %   %%  %
 %%%%   %    %  %

%% UNICODE NORMALIZATION FORMS
%% Unicode Standard Annex #15
%% http://unicode.org/reports/tr15/
-spec is_nf(list(), integer(), atom(), function()) -> yes | no | maybe.
is_nf([Head|Tail], LastCC, Result, CheckFun) -> 
    case ccc(Head) of
    CC when (LastCC > CC) and (CC =/= 0) -> no;
    CC -> 
        case apply(CheckFun, [Head]) of
        n -> no;
        m -> is_nf(Tail, CC, maybe,  CheckFun);
        y -> is_nf(Tail, CC, Result, CheckFun)
        end
    end;
is_nf([], _, Result, _) -> Result.



%% Detecting Normalization Forms
%% http://unicode.org/reports/tr15/#Detecting_Normalization_Forms
-spec is_nfc(list()) -> yes | no | maybe.
-spec is_nfd(list()) -> yes | no | maybe.
-spec is_nfkc(list()) -> yes | no | maybe.
-spec is_nfkd(list()) -> yes | no | maybe.
is_nfc(Str) when is_list(Str) -> is_nf(Str, 0, yes, nfc_qc(skip_check)).
is_nfd(Str) when is_list(Str) -> is_nf(Str, 0, yes, nfd_qc(skip_check)).
is_nfkc(Str) when is_list(Str) -> is_nf(Str, 0, yes, nfkc_qc(skip_check)).
is_nfkd(Str) when is_list(Str) -> is_nf(Str, 0, yes, nfkd_qc(skip_check)).


-spec to_nfc(list()) -> list().
-spec to_nfd(list()) -> list().
-spec to_nfkc(list()) -> list().
-spec to_nfkd(list()) -> list().
to_nfc([])   -> [];
to_nfc(Str)  -> 
    case is_nfc(Str) of
    yes -> Str;
    _   -> get_composition(to_nfd(Str))
    end.
to_nfkc([])  -> [];
to_nfkc([_|_] = Str) -> get_composition(
        normalize(get_recursive_decomposition(false, Str))).
to_nfd([])   -> [];
to_nfd([_|_] = Str) -> normalize(get_recursive_decomposition(true,  Str)).
to_nfkd([])  -> [];
to_nfkd([_|_] = Str) -> normalize(get_recursive_decomposition(false, Str)).


-spec list_to_latin1(list()) -> list().
list_to_latin1(Str) ->
    lists:reverse(list_to_latin1(Str, [])).

-spec list_to_latin1(list(), list()) -> list().
list_to_latin1([Char|Str], Res) ->
    list_to_latin1(Str, char_to_list(Char, [], Res));
list_to_latin1([], Res) -> Res.

% magic
% Char>255
-spec char_to_list(integer(), list(), list()) -> list().
char_to_list(Char, Buf, Res) ->
    case Char bsr 8 of
    0   ->  
        case Buf of
        [] -> [Char|Res];
        _  -> lists:reverse(Buf)++[Char|Res]
        end;
    Div -> 
        Rem = Char band 2#11111111,
        char_to_list(Div, [Rem|Buf], Res)
    end.

%% @doc internal_decompose(Str)
%% Canonical  If true bit is on in this byte, then selects the recursive 
%%            canonical decomposition, otherwise selects
%%            the recursive compatibility and canonical decomposition.
%% @end
%% @private
-spec get_recursive_decomposition(atom() | function(), list()) -> list().
get_recursive_decomposition(true, Str) -> 
    get_recursive_decomposition(is_compat(skip_check), Str, []);
get_recursive_decomposition(false, Str) -> 
    get_recursive_decomposition(fun ux_utils:is_always_false/1, Str, []);
get_recursive_decomposition(Canonical, Str) when is_function(Canonical) -> 
    get_recursive_decomposition(Canonical, Str, []).

% Skip ASCII
%% @private
-spec get_recursive_decomposition(function(), list(), list()) -> list().
get_recursive_decomposition(Canonical, [Char|Tail], Result) 
    when Char < 128 -> % Cannot be decomposed 
    get_recursive_decomposition(Canonical, Tail,
    [Char|Result]);

%% @doc Decompose one char of hangul.
get_recursive_decomposition(Canonical, [Char|Tail], Result)
    when (Char >= ?HANGUL_SBASE) and (Char =< ?HANGUL_SLAST) ->
    SIndex = Char - ?HANGUL_SBASE,
    L = ?HANGUL_LBASE + (SIndex div ?HANGUL_NCOUNT),
    V = ?HANGUL_VBASE + (SIndex rem ?HANGUL_NCOUNT) div ?HANGUL_TCOUNT,
    T = ?HANGUL_TBASE + (SIndex rem ?HANGUL_TCOUNT),
    get_recursive_decomposition(Canonical, Tail,
        case T of
        ?HANGUL_TBASE -> [V|[L|Result]];
        _ -> [T|[V|[L|Result]]]
        end);

get_recursive_decomposition(Canonical, [Char|Tail], Result) ->
    case decomp(Char) of
    []  -> get_recursive_decomposition(Canonical, Tail,
            [Char|Result]);
    Dec -> 
        case Canonical(Char) of % not is_compat = singleton
        true  -> 
            get_recursive_decomposition(Canonical, Tail, 
                [Char|Result]);
        false -> 
            get_recursive_decomposition(Canonical, Tail, 
                get_recursive_decomposition(Canonical,
                    Dec, Result))
        end
    end;
get_recursive_decomposition(_, [], Result) -> Result.

%% @doc Normalize NFD or NFKD.
normalize(Str)              -> normalize1(Str, [], []).
%% @private
normalize1([], [ ], Result) -> Result;
normalize1([], Buf, Result) -> normalize2(lists:reverse(Buf), Result);
normalize1([Char|Tail], Buf, Result) ->
    Class = ccc(Char),
    if
        (Class == 0) and (Buf == []) -> 
            normalize1(Tail, [], [Char | Result]);
        (Class == 0) -> 
            normalize1(Tail, [], 
                [Char | normalize2(lists:reverse(Buf), Result)]);
        true -> normalize1(Tail, [{Class, Char} | Buf], Result)
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
       (case ccc(Char) of
            0 -> 0;
            _ -> 256
        end)).

%% @doc Internal Composition Function.
%% @private
get_composition([Char|Tail]) -> 
    lists:reverse(
        get_composition(Tail, Char, 
            ?COMP_CHAR_CLASS(Char), [], [])
    ).


%% @doc Compose hangul characters.
%% 1. check to see if two current characters are L and V
%% 2. check to see if two current characters are LV and T
%% @end
%% @private
get_composition([VChar |Tail], LChar, 0, [], Result) 
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
            get_composition(Tail3, Char, 
                ?COMP_CHAR_CLASS(Char), [], Result3);
        [] -> Result3
        end;
    [Char|Tail2] ->
        get_composition(Tail2, Char, 
            ?COMP_CHAR_CLASS(Char), [], [LVChar|Result]);
    [] -> [LVChar|Result]
    end;

get_composition([Char | Tail], LChar, 0, [], Result) 
    when ?CHAR_IS_HANGUL_L(LChar) ->
    get_composition(Tail, Char, 
        ?COMP_CHAR_CLASS(Char), [], [LChar|Result]);
                    
get_composition([Char|Tail], LastChar, _, Mods, Result) when Char < 128 ->
    get_composition(Tail, Char, 0, [], comp_append([LastChar|Result], Mods));

get_composition([Char|Tail], LastChar, LastClass, Mods, Result) ->
    CharClass = ccc(Char),
    Comp = comp(LastChar, Char),
    if
        (Comp =/= false) 
        and ((LastClass < CharClass) or (LastClass == 0)) ->
            get_composition(Tail, Comp, LastClass, Mods, Result);
        (CharClass == 0) -> 
            get_composition(Tail, Char, CharClass, [], 
                comp_append([LastChar|Result], Mods));
        true -> get_composition(Tail, LastChar, CharClass, [Char|Mods], Result)
    end;
get_composition([], Char, _LastClass, [], Result) ->
    [Char|Result];
get_composition([], Char, _LastClass, Mods, Result) ->
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
%% [graphemes](http://en.wikipedia.org/wiki/Grapheme)
%% @end
to_graphemes(Str) ->
    explode_reverse(to_graphemes_raw(Str, [], [])).

%% @doc Returns not reversed result.
%% @private
to_graphemes_raw([H|T], Buf, Res) ->
    case {ccc(H), Buf} of
    {0, []} -> to_graphemes_raw(T, [H], Res);
    {0, _ } -> to_graphemes_raw(T, [H], [Buf|Res]);
    _       -> to_graphemes_raw(T, [H|Buf], Res)
    end;
to_graphemes_raw([   ], [ ], Res) -> Res;
to_graphemes_raw([   ], Buf, Res) -> [Buf|Res].

%% @doc Compute count of graphemes in the string.
length(Str) -> len_graphemes(Str, 0).
len(Str) -> len_graphemes(Str, 0).

len_graphemes([H|T], Len) ->
    case ccc(H) of
    0 -> len_graphemes(T, Len + 1);
    _ -> len_graphemes(T, Len)
    end;
len_graphemes([   ], Len) -> Len.

%% @doc Return Len chars from the beginning of the string.
first(Str, Len) ->
    lists:flatten(
        lists:sublist(to_graphemes(Str), Len)).

%% @doc Return Len chars from the beginning of the string.
last(Str, Len) ->
    lists:flatten(
        explode_reverse(
            lists:sublist(
                to_graphemes_raw(Str, [], []), Len))).

%% @doc Reverses string graphemes.
reverse(Str) ->
    reverse_flatten(
        lists:reverse(to_graphemes_raw(Str, [], [])), 
        [], []).

reverse_flatten([H|T], [], Res) ->
    reverse_flatten(T, H, Res);
reverse_flatten(T, [HH|TT],  Res) ->
    reverse_flatten(T, TT, [HH|Res]);
reverse_flatten(_, [], Res) ->
    Res.

%------------------------------------------------------------------------------
  %%%   %     % %%%%%%% %%%%%%%
   %    %%    % %       %     %
   %    % %   % %       %     %
   %    %  %  % %%%%%   %     %
   %    %   % % %       %     %
   %    %    %% %       %     %
  %%%   %     % %       %%%%%%%

%% Collect information about string.
-spec info(Str::string()) -> #unistr_info {}.

%% @doc Return information about a string.
info(Rec = #unistr_info {}) ->
    info1([
        fun info_comment/1,
        fun info_nfd/1,
        fun info_nfc/1,
        fun info_ducet_simple/1,
        fun info_ccc/1,
        fun info_col_sort_array_non_ignorable/1,
        fun info_col_sort_array_blanked/1,
        fun info_col_sort_array_shifted/1,
        fun info_col_sort_array_shift_trimmed/1,
        fun info_char_block/1
        ], Rec);
info([_|_] = Str) -> 
    info(#unistr_info{str=Str});
info(Ch) when is_integer(Ch) -> 
    info(#unistr_info{str=[Ch]}).

%% @private
info1([F|Tail], Rec) ->
    NewRec = apply(F, [Rec]),
    info1(Tail, NewRec);
info1([], Rec) ->
    Rec.

%% @private
info_comment(Obj = #unistr_info{str=Str}) ->
    Obj#unistr_info{
        comment=lists:map(fun ux_char:comment/1, Str)
    }.

%% @private
info_ducet_simple(Obj = #unistr_info{str=Str}) ->
    Obj#unistr_info{ 
        ducet=lists:map(fun ux_col:ducet/1, [[Ch] || Ch <- Str])
    }.

%% @private
info_ccc(Obj = #unistr_info{str=Str}) ->
    Obj#unistr_info{
        ccc=lists:map(fun ccc/1, [Ch || Ch <- Str])
    }.

%% @private
info_nfd(Obj = #unistr_info{str=Str}) ->
    Obj#unistr_info{nfd=to_nfd(Str)}.

%% @private
info_nfc(Obj = #unistr_info{str=Str}) ->
    Obj#unistr_info{nfc=to_nfc(Str)}.

%% @private
info_col_sort_array_non_ignorable(Obj = #unistr_info{str=Str}) ->
    Obj#unistr_info{ 
        col_sort_array_non_ignorable=ux_col:sort_array_non_ignorable(Str)
    }.

%% @private
info_col_sort_array_blanked(Obj = #unistr_info{str=Str}) ->
    Obj#unistr_info{ 
        col_sort_array_blanked=ux_col:sort_array_blanked(Str)
    }.

%% @private
info_col_sort_array_shifted(Obj = #unistr_info{str=Str}) ->
    Obj#unistr_info{ 
        col_sort_array_shifted=ux_col:sort_array_shifted(Str)
    }.

%% @private
info_col_sort_array_shift_trimmed(Obj = #unistr_info{str=Str}) ->
    Obj#unistr_info{ 
        col_sort_array_shift_trimmed=ux_col:sort_array_shift_trimmed(Str)
    }.

%% @private
info_char_block(Obj = #unistr_info{str=Str}) ->
    Obj#unistr_info{ 
        blocks=lists:map(fun ux_char:block/1, [Ch || Ch <- Str])
    }.

% String Info End.

%------------------------------------------------------------------------------
  %%%%%  %%%%%%   %%%%    %%%%%   %%%%
    %    %       %          %    %
    %    %%%%%    %%%%      %     %%%%
    %    %            %     %         %
    %    %       %    %     %    %    %
    %    %%%%%%   %%%%      %     %%%%


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


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

    % empty delimeter
    ,?_assertEqual(M:F("", "test"), false)
    % limit >0
    ,?_assertEqual(M:F("|", "one|two|three|four", 2), ["one", "two|three|four"])

    % limit <0
    ,?_assertEqual(M:F("|", "one|two|three|four", -1), ["one", "two", "three"])
    ,?_assertEqual(M:F("-", "one|two|three|four", -1), [])
    ,?_assertEqual(M:F("-", "one|two|three|four"), ["one|two|three|four"])
    ].

html_special_chars_test_() ->
    F = 'html_special_chars',
    M = 'ux_string',
    [?_assertEqual(M:F("ddf2#$\""), "ddf2#$&quot;")
    ,?_assertEqual(M:F("test1 & test2"), "test1 &amp; test2")
    ].

to_lower_test_() ->
    M = 'ux_string',
    F = 'to_lower',
    [?_assertEqual(M:F("small BIG"), "small big")
    ,?_assertEqual(M:F(    "You want your freedom?"), 
                "you want your freedom?")
    % Russian text
    ,?_assertEqual(M:F(    [1069,1056,1051,1040,1053,1043]), 
                [1101,1088,1083,1072,1085,1075])
    ].

to_upper_test_() ->
    M = 'ux_string',
    F = 'to_upper',
    [?_assertEqual(M:F("small BIG"), "SMALL BIG")
    ,?_assertEqual(M:F(    "I'm making a note here: HUGE SUCCESS."), 
                "I'M MAKING A NOTE HERE: HUGE SUCCESS.")
    ,?_assertEqual(M:F(    [1101,1088,1083,1072,1085,1075]),
                [1069,1056,1051,1040,1053,1043])
    ].

strip_tags_test_() ->
    M = 'ux_string',
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
    ].

tags_to_list_test_() ->
    F = fun tags_to_list/1,
    [?_assertEqual(F("<a><b>"), ["b", "a"])
    ,?_assertEqual(F("<span>"), ["span"])
    ,?_assertEqual(F("<b><span>"), ["span", "b"])
    ,?_assertEqual(F("<i>"), ["i"])
    ].

delete_types_test_() ->
    M = 'ux_string',
    F = 'delete_types',
    [?_assertEqual(M:F([ll, lu], "Tom Cat!"), " !")
    ,?_assertEqual(M:F([ll],     "Tom Cat!"), "T C!")
    ,?_assertEqual(M:F([po],     "Tom Cat!"), "Tom Cat")
    ,{"Skip 2 chars (A,B).",
        ?_assertEqual(M:F([ll], "AaBbCc44ff", -2), "ABbCc44ff")}
    ,{"Delete only 2 chars (A,B).",
        ?_assertEqual(M:F([ll], "AaBbCc44ff",  2), "ABCc44ff")}
    ,?_assertEqual(M:F([ll], "AaBbCc44ffdsBAF",  4), "ABC44fdsBAF")
    ,?_assertEqual(M:F([ll], "AaBbCc44ffdsBAF", -4), "ABC44ffdsBAF")
    ].

filter_types_test_() ->
    M = 'ux_string',
    F = 'filter_types',
    [?_assertEqual(M:F([ll, lu], "Tom Cat!"), "TomCat")
    ,?_assertEqual(M:F([ll],     "Tom Cat!"), "omat")
    ,?_assertEqual(M:F([po],     "Tom Cat!"), "!")
    ,?_assertEqual(M:F([ll], "AaBbCc44ffds",  3), "abc44ffds")
    ,?_assertEqual(M:F([ll], "AaBbCc44ffds",  4), "abcffds")
    ,?_assertEqual(M:F([ll], "AaBbCc44ffds", -2), "abCc44ffds")
    ,?_assertEqual(M:F([ll], "AaBbCc44ffds", -4), "abc4ffds")
    ].

types_test_() ->
    M = 'ux_string',
    F = 'types',
    [?_assertEqual(M:F("Tom Cat!"), [lu,ll,ll,zs,lu,ll,ll,po])
    %,?_assertEqual(M:F(), )
    ].

last_types_test_() ->
    M = 'ux_string',
    F = 'last_types',
    [?_assertEqual(M:F([ll], "AavbfFDsdfffd9s9999", -5), "99999")
    ,?_assertEqual(M:F([ll], "AavbfFDsdfffd9s9999", -6), "D99999")
    ,?_assertEqual(M:F([ll], "AavbfFDsdfffd9s9999", -7), "FD99999")
    ,?_assertEqual(M:F([ll], "AavbfFDsdfffd9s9999", -8), "AFD99999")
    ].

first_types_test_() ->
    M = 'ux_string',
    F = 'first_types',
    [?_assertEqual(M:F([ll], "AavbfFDsdfffds", 4), "avbf")
    ,?_assertEqual(M:F([ll], "AavbfFDsdfffds", 5), "avbfs")
    ].

to_graphemes_test_() ->
    M = 'ux_string',
    F = 'to_graphemes',
    [{"Simple example", 
        ?_assertEqual(M:F("Octocat!"), ["O","c","t","o","c","a","t","!"])},
     {"U+1EE5 LATIN SMALL LETTER U WITH DOT BELOW, U+031B COMBINING HORN, a, b",
        ?_assertEqual(M:F([16#1EE5, 16#031B, $a, $b]), [[7909,795],"a","b"])}
    ].

first_test_() ->
    M = 'ux_string',
    F = 'first',
    [?_assertEqual(M:F("Octocat!", 4), "Octo")
    ].
last_test_() ->
    M = 'ux_string',
    F = 'last',
    [?_assertEqual(M:F("Octocat!", 4), "cat!")
    ].

-ifdef(SLOW_TESTS).
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
nfc_test(_InFd, 0, StrNum) -> 
    io:format(user, "Only ~w strings were tested. Exit.~n", [StrNum]),
    ok;
nfc_test(InFd, Max, StrNum) ->
    % Show message
    case StrNum rem 1000 of
    0 -> io:format(user, "~n~w strings were tested. ", [StrNum]);
    _ -> next
    end,

    NFC  = fun 'ux_string':to_nfc/1,
    NFD  = fun 'ux_string':to_nfd/1,
    NFKC = fun 'ux_string':to_nfkc/1,
    NFKD = fun 'ux_string':to_nfkd/1,

    case file:read_line(InFd) of
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
            % start body
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
            % end body
        _ -> next
        catch error:_ -> next
        after 
            nfc_test(InFd, Max - 1, StrNum + 1)
        end
    end.

nfc_prof(Count) ->
    {ok, InFd} = file:open(?UNIDATA:get_unidata_dir() 
        ++ "NormalizationTest.txt", [read]),
    io:setopts(InFd,[{encoding,utf8}]),
    nfc_test(InFd, Count, 0),
    file:close(InFd),
    ok.

nfc_test_() ->
    {timeout, 600, 
        {"Normalization Conformance Test", 
            fun() -> 
                nfc_prof(1000000),
                io:format(user, "~n", []) end}}.


-endif. % SLOW_TESTS
-endif. % TEST
