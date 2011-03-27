%% uxstring library 
%%
%% @package  ux
%% @author   Uvarov Michael <freeakk@gmail.com>
%% @license  http://www.fsf.org/copyleft/lgpl.html LGPL
%%
%% @copyright 2010 Uvarov Michael.
%% %CopyrightBegin%
%%  Copyright 2010 Uvarov Michael  
%%
%%  See the enclosed file COPYING for license information (LGPL). If you
%%  did not receive this file, see http://www.fsf.org/copyleft/lgpl.html
%% %CopyrightEnd%


-module(uxstring).
-author('Uvarov Michael <freeakk@gmail.com>').

-include("uxstring.hrl").

-export([list_to_latin1/1]).
-export([char_comment/1]).
-export([htmlspecialchars/1, hsc/1]). % hsc is short name

-export([explode/2, explode/3]).
-export([split/2, split/3]).

-export([to_lower/1, to_upper/1]).
-export([st/1, strip_tags/1]).
-export([st/2, strip_tags/2]).
-export([st/3, strip_tags/3]).
-export([to_string/1]).
-export([delete_types/2, delete_types/3, filter_types/2, filter_types/3, explode_types/2, split_types/2]).
-export([first_types/3, last_types/3]).

% for tests
-export([tags_to_list/1]).
-export([delete_empty/1]).

% for utf-8
-export([char_to_lower/1, char_to_upper/1]).
-export([is_lower/1, is_upper/1]).
-export([is_letter/1, is_number/1, is_decimal/1, is_separator/1, is_pm/1, is_punctuation_mark/1]).

-export([freq/1, freq_dict/1]).
-export([ccc/1]).
-export([is_nfc/1, is_nfd/1, is_nfkc/1, is_nfkd/1]).
-export([to_nfc/1, to_nfd/1, to_nfkc/1, to_nfkd/1]).
-export([to_ncr/1]).

-export([is_comp_excl/1]).
-export([is_hangul/1, is_acsii/1]).

-export([to_graphemes/1, reverse/1]).
-export([length/1, len/1]).
-export([hex_to_int/1]).

-export([ducet/1]).
-export([col_non_ignorable/2]).
-export([col_extract/2]).

%% @doc Returns various "character types" which can be used 
%%      as a default categorization in implementations.
%%      Types:
%%      http://www.ksu.ru/eng/departments/ktk/test/perl/lib/unicode/UCDFF301.html#General%20Category
%% @end
-export([char_type/1, char_types/1]).

-define(ASSERT(TEST,TRUE,FALSE), case TEST of 
	true  -> TRUE; 
	false -> FALSE
end).

-define(ASSERT_IN_ARRAY_LAMBDA(TEST), case TEST of 
	true  -> fun lists:member/2; 
	false -> fun not_in_array/2
end).

%% Defines Hangul constants
-define(HANGUL_SBASE,  16#AC00).
-define(HANGUL_LBASE,  16#1100).
-define(HANGUL_VBASE,  16#1161).
-define(HANGUL_TBASE,  16#11A7).
-define(HANGUL_LCOUNT, 19).
-define(HANGUL_VCOUNT, 21).
-define(HANGUL_TCOUNT, 28).
-define(HANGUL_NCOUNT, 588).
-define(HANGUL_SCOUNT, 11172).



-include("string/char_to_upper.hrl").
%char_to_upper(C) -> C.
-include("string/char_to_lower.hrl").
%char_to_lower(C) -> C.

-include("string/is_upper.hrl").
%% @doc Returns true, if is C is uppercase. 
-spec is_upper(C::char()) -> boolean().
%is_upper(_) -> false.
-include("string/is_lower.hrl").
%% @doc Returns true, if is C is lowercase.
-spec is_lower(C::char()) -> boolean().
%is_lower(_) -> false.

-include("string/char_comment.hrl").
-include("string/char_type.hrl").
%% @doc Returns a char type.
-spec char_type(C::char()) -> atom().
%char_type(_) -> other.
char_types(Str)	-> lists:map({?MODULE, char_type}, Str).

-include("string/freq_dict.hrl").
-include("string/ccc.hrl").

%% From http://www.unicode.org/Public/UNIDATA/DerivedNormalizationProps.txt
-include("string/nfc_qc.hrl").
-include("string/nfd_qc.hrl").
-include("string/nfkc_qc.hrl").
-include("string/nfkd_qc.hrl").

%% From http://www.unicode.org/Public/UNIDATA/CompositionExclusions.txt
-include("string/is_comp_excl.hrl").
-include("string/is_compat.hrl").
-include("string/decomp.hrl").
-include("string/comp.hrl").

% http://unicode.org/reports/tr10/
-include("string/ducet.hrl").

%freq_dict(_) -> 0.

%% @doc Returns true, if C is a letter.
-spec is_letter(C::char()) -> boolean().

is_letter(C) -> case erlang:atom_to_list(char_type(C)) of 
			[$l,_] -> true;
			_      -> false
		end.	

%% @doc Returns true, if is C is a number.
-spec is_number(C::char()) -> boolean().

is_number(C) -> case erlang:atom_to_list(char_type(C)) of 
			[$n,_] -> true;
			_      -> false
		end.	

%% @doc Return true, if is C is a separator.
-spec is_separator(C::char()) -> boolean().

is_separator(C) -> case erlang:atom_to_list(char_type(C)) of 
			[$z,_] -> true;
			_      -> false
		end.	

%% @see uxstring:is_punctiation_mark/1
-spec is_pm(C::char()) -> boolean().

is_pm(C) -> is_punctuation_mark(C).

%% @doc Returns true, if is C is a punctiation mark.
-spec is_punctuation_mark(C::char()) -> boolean().

is_punctuation_mark(C) -> case erlang:atom_to_list(char_type(C)) of 
			[$p,_] -> true;
			_      -> false
		end.	

%% @doc Return true, if C is a decimal number.
-spec is_decimal(C::char()) -> boolean().

is_decimal(C) -> char_type(C) == nd.

%% @doc Returns a new string which is made from the chars of Str 
%%      which are not a type from Types list.
%% @end
-spec delete_types([atom()], string()) -> string().

delete_types(Types, Str) -> 
	lists:filter(fun(El) -> 
		not lists:member(char_type(El), Types) 
	end, Str).

%% @doc Stops delete_type/2 after Limit deleted chars. If Limit < 0, then
%%      stops after -Limit skipped chars.
%% @end
-spec delete_types([atom()], string(), integer()) -> string().

delete_types(Types, Str, Limit) when Limit > 0 ->
	lists:reverse(get_types(Types, Str, Limit, [], true, 
				fun not_in_array/2, 0, -1));
delete_types(Types, Str, Limit) when Limit < 0 ->
	lists:reverse(get_types(Types, Str, Limit, [], true, 
				fun not_in_array/2, 1,  0)).

%% @doc Returns a new string which is made from the chars of Str 
%%      which are a type from Types list.
% @end
-spec filter_types([atom()], string()) -> string().

filter_types(Types, Str) -> 
	lists:filter(fun(El) -> 
		lists:member(char_type(El), Types) 
	end, Str).

%% @doc Stops filter_type/2 after Limit extracted chars. If Limit < 0, then
%%      stops after -Limit skipped chars.
%% @end
-spec filter_types([atom()], string(), integer()) -> string().

filter_types(Types, Str, Limit) when Limit > 0 ->
	lists:reverse(get_types(Types, Str, Limit, [], true, 
					fun lists:member/2, -1, 0));
filter_types(Types, Str, Limit) when Limit < 0 ->
	lists:reverse(get_types(Types, Str, Limit, [], true, 
					fun lists:member/2,  0, 1)).

%% @doc If Len>0, then gets first Len chars of type, which is in Types
%%      If Len<0, then gets first -Len chars of type, which is NOT in Types
%% @end
-spec first_types([atom()], string(), integer()) -> string().
first_types(Types, Str, Len) -> 
	lists:reverse(get_types(Types, Str, Len, [], false, 
		?ASSERT_IN_ARRAY_LAMBDA(Len>0), ?ASSERT(Len>0, -1, 1), 0)).

%% @doc If Len>0, then gets last Len chars of type, which is in Types
%%      If Len<0, then gets last -Len chars of type, which is NOT in Types
%% @end
-spec last_types([atom()], string(), integer()) -> string().
last_types(Types, Str, Len) -> 
	get_types(Types, lists:reverse(Str), Len, [], false, 
		?ASSERT_IN_ARRAY_LAMBDA(Len>0), ?ASSERT(Len>0, -1, 1), 0).
	
get_types(_, [], _, Result, _, _, _, _) -> Result;
get_types(_,  _, 0, Result, false, _, _, _) -> Result;
get_types(_,  Tail, 0, Result, true, _, _, _) -> 
	lists:reverse(Tail)++Result;
get_types(Types, [Char|Tail], 
	Len, % Strop after Len chars
	Result, % Result array
	RetTail, % Concat tail with Result or not
	Fun, % Check function
	TrueStep, % Len+TrueStep, if Fun return true
	FalseStep) -> 
	case apply(Fun, [char_type(Char), Types]) of
		true  -> get_types(Types, Tail, Len+TrueStep, [Char|Result], 
					RetTail, Fun, TrueStep, FalseStep);
		false -> get_types(Types, Tail, Len+FalseStep, Result, 
					RetTail, Fun, TrueStep, FalseStep)
	end.

%% @doc Returns a new list of strings which are parts of Str splited 
%%      by separator chars of a type from Types list.
%% @end
-spec explode_types([atom()], string()) -> string().

explode_types(Types, Str) -> 
	explode_reverse(explode_types_cycle(Types, Str, [], [])).

explode_types_cycle(_, [], [], Res) -> Res;
explode_types_cycle(_, [], Buf, Res) -> [Buf|Res];
explode_types_cycle(Types, [Char|Str], Buf, Res) -> 
	case lists:member(char_type(Char), Types) of
		true  -> explode_types_cycle(Types, Str, [], [Buf|Res]);
		false -> explode_types_cycle(Types, Str, [Char|Buf], Res)
	end.

%% @doc Returns a new list of strings which are parts of Str splited 
%%      by separator chars of a type from Types list. Parts can not be
%%      empty.
%% @end 
-spec split_types([atom()], string()) -> string().

split_types(Types, Str) -> delete_empty(explode_types(Types, Str)).

%% @doc Deletes all empty lists from List.
%%      Example:
%%      delete_empty([ [], "test", [1] ]) -> ["test", [1]].
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

explode([], _) -> false;
explode(_, []) -> [];
explode([Delimeter], Str) when is_integer(Delimeter) -> 
    explode_simple(Delimeter, lists:reverse(Str), [], []);
explode(Delimeter,   Str) when is_integer(Delimeter) -> 
    explode_simple(Delimeter, lists:reverse(Str), [], []);
explode(Delimeter, Str) -> 
	case explode_cycle(Delimeter, Str, [], []) of
		false -> [Str];
		Res -> explode_reverse(Res)
	end.

explode([], _, _) -> false;
explode(Delimeter, Str, Limit) when is_integer(Delimeter) ->
    explode([Delimeter], Str, Limit); 
explode(Delimeter, Str, Limit) when Limit > 0 -> 
	explode_reverse(explode_cycle_pos(Delimeter, Str, [], [], Limit));
explode(Delimeter, Str, Limit) when Limit < 0 -> 
	case explode_cycle(Delimeter, Str, [], []) of
		false -> [];
		Res -> explode_reverse(lists:nthtail(-Limit, Res))
	end;
explode(Delimeter, Str, _) -> explode(Delimeter, Str).

explode_reverse(Res) -> lists:map({lists, reverse}, lists:reverse(Res)). 

%% @doc Simple and fast realization.
%%      Delimeter is one char.
%%      Str is reversed string.
%% @end
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
explode_cycle(_, [], _,   [])     -> false;
explode_cycle(_, [], Buf, Result) -> [Buf | Result];
explode_cycle(Delimeter, Str, Buf, Result) ->
	case explode_check(Delimeter, Str) of
		false -> [C|Tail] = Str, 
			explode_cycle(Delimeter, Tail, [C|Buf], Result);
		Tail -> explode_cycle(Delimeter, Tail, [], [Buf | Result])
	end.

explode_cycle_pos(_, [], Buf, Result, _) -> [Buf|Result];
explode_cycle_pos(_, Str, _, Result, 1) -> [lists:reverse(Str)|Result];
explode_cycle_pos(Delimeter, Str, Buf, Result, Limit) ->
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
	lists:map({?MODULE, char_to_lower}, Str).

%% @doc Converts characters of a string to a uppercase format.
-spec to_upper(string()) -> string().

to_upper(Str) ->
	lists:map({?MODULE, char_to_upper}, Str).

%% @doc Encodes html special chars.
-spec htmlspecialchars(string()) -> string().

htmlspecialchars([]) -> [];
htmlspecialchars(Str) -> hsc(Str).

%% @see uxstring:htmlspecialchars/1
-spec hsc(string()) -> string().

hsc([ ]) -> [];
hsc(Str) -> hsc(lists:reverse(Str), []).

hsc([      ], Buf) -> Buf;
hsc([$" | T], Buf) -> hsc(T, lists:append("&quot;", Buf));
hsc([$' | T], Buf) -> hsc(T, lists:append("&#39;", Buf));
hsc([$& | T], Buf) -> hsc(T, lists:append("&amp;", Buf));
hsc([$< | T], Buf) -> hsc(T, lists:append("&lt;", Buf));
hsc([$> | T], Buf) -> hsc(T, lists:append("&gt;", Buf));
hsc([H  | T], Buf) -> hsc(T, [H|Buf]).

%% @doc Deletes tags from the string.
%%
%%      Example: 
%%      > uxstring:strip_tags("<b>some string</b>").
%%      "some string"
%%      > uxstring:strip_tags("<h1>Head</h1><p>and paragraf</p>", ["h1"]).	
%%      "<h1>Head</h1>and paragraf"
%%      uxstring:strip_tags("<h1>Head</h1><p><!-- and paragraf --></p>", ["!--"]).
%%      "Head<!-- and paragraf -->"
%%      uxstring:st("a<br />b", [], " ").
%%      "a b"
%% @end
-spec strip_tags(string()) -> string().
-spec strip_tags(string, [string() | atom() | char()]) -> string().

strip_tags(Str) -> st(Str, []).

strip_tags(Str, Allowed) -> st(Str, Allowed).
strip_tags(Str, Allowed, Alt) -> st(Str, Allowed, Alt).

%% @see uxstring:strip_tags/1
st(Str) -> st_cycle(Str, [], 0, []).
%% @see uxstring:strip_tags/2
st(Str, []) -> st(Str); 
st(Str, [$<|Allowed]) -> st(Str, tags_to_list(Allowed));
st(Str, Allowed) -> st(Str, Allowed, []). 
%% @see uxstring:strip_tags/3
st(Str, [], []) -> st(Str); 
st(Str, [$<|Allowed], Alt) -> st(Str, tags_to_list(Allowed), Alt);
st(Str, [], Alt) -> st_cycle(Str, [], 0, lists:reverse(Alt)); 
st(Str, Allowed, Alt) -> 
	st_cycle_with_allowed(Str, [],
			lists:map({lists, reverse},
		    	lists:map({string, to_lower},
		        	lists:map({?MODULE, to_string}, Allowed))), 
			lists:reverse(Alt)).

%% @doc Drops all tags from the string.
%%      Cnt is a count of not closed <
%%      If we found <, then Cnt++
%%      If we found >, then Cnt--
%% @end
st_cycle([$< | Tail], Buf, Cnt, Alt) -> st_cycle(Tail,        Buf, Cnt + 1, Alt);
st_cycle([$> | Tail], Buf, 1,   Alt) -> st_cycle(Tail, Alt ++ Buf, 0,       Alt);
st_cycle([$> | Tail], Buf, 0,   Alt) -> st_cycle(Tail,        Buf, 0,       Alt);
st_cycle([$> | Tail], Buf, Cnt, Alt) -> st_cycle(Tail,        Buf, Cnt - 1, Alt);
st_cycle([H  | Tail], Buf, 0,   Alt) -> st_cycle(Tail, [H | Buf] , 0,       Alt);
st_cycle([_  | Tail], Buf, Cnt, Alt) -> st_cycle(Tail,        Buf, Cnt,     Alt);
st_cycle([         ], Buf, _,   _  ) -> lists:reverse(Buf).

%% @doc Is used by st_cycle_with_allowed
st_get_tag    ([$> | T],       Buf , Tag, _, 1) ->
	{Tag, [$> | Buf], T};
st_get_tag    ([$> | T],       Buf , Tag    , _    , Cnt    ) ->
	st_get_tag(T       ,       Buf , Tag    , false, Cnt - 1);
st_get_tag    ([$< | T],       Buf , Tag    , false, Cnt    ) ->
	st_get_tag(T       ,       Buf , Tag    , false, Cnt + 1);
st_get_tag    ([$  | T],       Buf , Tag    , _    , Cnt    ) ->
	st_get_tag(T       , [$  | Buf], Tag    , false, Cnt    );
st_get_tag    ([$/ | T],       Buf , Tag    , true , Cnt    ) ->
	st_get_tag(T       , [$/ | Buf], Tag    , true , Cnt    );
st_get_tag    ([H  | T],       Buf , Tag    , true , Cnt    ) ->
	st_get_tag(T       , [H  | Buf], [H|Tag], true , Cnt    );
% TODO: control atributes (onclick, for example. xss fix!)
st_get_tag    ([H  | T],       Buf , Tag    , false, Cnt    ) ->
	st_get_tag(T       , [H  | Buf], Tag    , false, Cnt    );
st_get_tag    ([      ], _         , _      , _,     _) -> false; 
st_get_tag    (_       , [        ], _      , _,     _) -> false. 

%% @doc Drops tags, but saves tags in the Allowed list.
st_cycle_with_allowed([$< | T], Res, Allowed, Alt) ->
	case st_get_tag(T, [$<], [], true, 1) of 
		{Tag, SubStr, Tail} -> 
			case lists:member(string:to_lower(Tag), Allowed) of 
				true  -> st_cycle_with_allowed(Tail, 
					SubStr ++ Res, Allowed, Alt); % Allowed tag
				false -> st_cycle_with_allowed(Tail,
					Alt    ++ Res, Allowed, Alt)  % Alt is replacement
			end;
        _ -> lists:reverse(Res) % deletes unclosed string 
    end;
st_cycle_with_allowed([$> | T], Res, Allowed, Alt) ->
    st_cycle_with_allowed(T, Res, Allowed, Alt);
st_cycle_with_allowed([Ch | T], Res, Allowed, Alt) ->
    st_cycle_with_allowed(T, [Ch | Res], Allowed, Alt);
st_cycle_with_allowed([      ], Res, _, _) -> lists:reverse(Res).

%% @doc Convert string of tags to list
%%      Example:
%%      > tags_to_list("<a><b>").
%%      ["a", "b"]
%% @end
tags_to_list(Str) -> tags_to_list(Str, [], []).

tags_to_list([$< | Str], Res, Buf) -> tags_to_list(Str, Res, Buf);
tags_to_list([$/ | Str], Res, Buf) -> tags_to_list(Str, Res, Buf);
tags_to_list([$> | Str], Res, Buf) -> tags_to_list(Str, [lists:reverse(Buf)|Res], []);
tags_to_list([Ch | Str], Res, Buf) -> tags_to_list(Str, Res, [Ch|Buf]);
tags_to_list([        ], Res, _  ) -> Res. 


%% @doc Extract a tag from the beginning of the string..
%%      In format:
%%      {String,
%%      Lower Case String, 
%%      Buffer for a substring,
%%      Tag - accumulates a tag name, 
%%      Capture chars of a tag name or not, 
%%      a number of unclosed tags}
%% @end

not_in_array(X,Y) -> not lists:member(X,Y).

%% @doc Counts a letter frequency
-spec freq(string()) -> dict(). 

freq(Str) -> freq_1(Str, dict:new()).

freq_1([Char|Str], Dict) -> freq_1(Str, dict:update_counter(Char, 1, Dict));
freq_1([], Dict)         -> Dict.


%% NORMALIZATION
%% http://unicode.org/reports/tr15/
is_nf([Head|Tail], LastCC, Result, CheckFun) -> 
    case ccc(Head) of
        CC when (LastCC > CC) and (CC =/= 0) -> no;
        CC ->   case apply(CheckFun, [Head]) of
                    n -> no;
                    m -> is_nf(Tail, CC, maybe,  CheckFun);
                    y -> is_nf(Tail, CC, Result, CheckFun)
                end
    end;
is_nf([], _, Result, _) -> Result.



%% Detecting Normalization Forms
%% http://unicode.org/reports/tr15/#Detecting_Normalization_Forms
is_nfc(Str)  -> is_nf(Str, 0, yes, fun nfc_qc/1).
is_nfd(Str)  -> is_nf(Str, 0, yes, fun nfd_qc/1).
is_nfkc(Str) -> is_nf(Str, 0, yes, fun nfkc_qc/1).
is_nfkd(Str) -> is_nf(Str, 0, yes, fun nfkd_qc/1).

to_nfc([])   -> [];
to_nfc(Str)  -> case is_nfc(Str) of
    yes -> Str;
    _   -> get_composition(to_nfd(Str))
end.
to_nfkc([])  -> [];
to_nfkc(Str) -> get_composition(get_recursive_decomposition(false, Str)).
to_nfd([])   -> [];
to_nfd(Str)  -> get_recursive_decomposition(true,  Str).
to_nfkd([])  -> [];
to_nfkd(Str) -> get_recursive_decomposition(false, Str).


is_acsii(Char) when (Char>=0) and (Char=<16#7F) 
    -> true;
is_acsii(_) 
    -> false.

list_to_latin1(Str) ->
    lists:reverse(list_to_latin1(Str, [])).

list_to_latin1([Char|Str], Res) ->
    list_to_latin1(Str, char_to_list(Char, [], Res));
list_to_latin1([],         Res) -> Res.

% magic
% Char>255
char_to_list(Char, Buf, Res) ->
    case Char bsr 8 of
        0   ->  case Buf of
                    [] -> [Char|Res];
                    _  -> lists:reverse(Buf)++[Char|Res]
                end;
        Div ->  Rem = Char band 2#11111111,
                char_to_list(Div, [Rem|Buf], Res)
    end.

% internal_decompose(Str)
% Canonical  If true bit is on in this byte, then selects the recursive 
%            canonical decomposition, otherwise selects
%            the recursive compatibility and canonical decomposition.
get_recursive_decomposition(Canonical, Str) -> 
    normalize(
            get_recursive_decomposition(Canonical, Str, [])).

get_recursive_decomposition(Canonical, [Char|Tail], Result) ->
    IsHangul = is_hangul_precomposed(Char),
    if
      IsHangul   % Other algorithm
      ->  get_recursive_decomposition(Canonical, Tail,
               hangul_decomposition(Char, Result));

      Char < 128 % Cannot be decomposed 
      ->  get_recursive_decomposition(Canonical, Tail,
               [Char|Result]);

      true       % Try decomposed
      -> case decomp(Char) of
            []  -> get_recursive_decomposition(Canonical, Tail,
                                                [Char|Result]);
            Dec -> case Canonical and is_compat(Char) of % not is_compat = singleton
                    true    -> get_recursive_decomposition(Canonical,
                            Tail,  [Char|Result]);
                        false   -> get_recursive_decomposition(Canonical,
                            Tail,  get_recursive_decomposition(Canonical,
                            Dec, Result))
                   end
         end
    end;
get_recursive_decomposition(_, [], Result) -> Result.

% Normalize NFD or NFKD
normalize(Str)              -> normalize1(Str, [], []).
normalize1([], [ ], Result) -> Result;
normalize1([], Buf, Result) -> normalize2(lists:reverse(Buf), Result);
normalize1([Char|Tail], Buf, Result) ->
    Class = ccc(Char),
    if
        (Class == 0) and 
        (Buf == [])  -> normalize1(Tail, [], [Char | Result]);

        (Class == 0) -> normalize1(Tail, [], 
            [Char | normalize2(lists:reverse(Buf), Result)]);

        true -> normalize1(Tail, [{Class, Char} | Buf], Result)
    end.

% Append chars from Buf to Result in a right order.
normalize2([], Result)  -> Result;
normalize2(Buf, Result) ->
    case normalize3(Buf, false, 0) of
        false             -> Result;
        {_, Char} = Value -> normalize2(Buf -- [Value], [Char|Result])
    end.

% Return char from Buf with max ccc
normalize3([{CharClass, _} = Value | Tail], _, MaxClass) 
    when CharClass > MaxClass 
 -> normalize3(Tail, Value, CharClass);
normalize3([_|Tail], Value, MaxClass) 
 -> normalize3(Tail, Value, MaxClass);
normalize3([], Value, _) -> Value.


%% Internal Composition Function
get_composition([Char|Tail]) -> 
    CharClass = ccc(Char),
    hangul_composition(
            get_composition(Tail, Char, 
                case CharClass of
                    0 -> 0;
                    _ -> 256
                end, [], [])).

get_composition([Char|Tail], LastChar, _, Mods, Result) when Char < 128 ->
    get_composition(Tail, Char, 0, [], Mods++[LastChar|Result]);
get_composition([Char|Tail], LastChar, LastClass, Mods, Result) ->
    CharClass = ccc(Char),
    Comp = comp(LastChar, Char),
    if
        (Comp =/= false) 
        and ((LastClass < CharClass)
          or (LastClass == 0)) ->
            get_composition(Tail, Comp, LastClass, Mods, Result);
        (CharClass == 0) -> get_composition(Tail, Char, CharClass, [], 
            Mods ++ [LastChar|Result]);
        true -> get_composition(Tail, LastChar, CharClass, [Char|Mods], Result)
    end;
get_composition([], Char, _, [],   Result) ->
    [Char|Result];
get_composition([], Char, _, Mods, Result) ->
    Mods ++ [Char|Result].

% http://unicode.org/reports/tr15/#Hangul
is_hangul(Char) when ((Char>=16#1100) and (Char=<16#11FF)) % Hangul Jamo
                  or ((Char>=16#A960) and (Char=<16#A97C)) % Hangul Jamo Extended-A
                  or ((Char>=16#D7B0) and (Char=<16#D7C6)) % Hangul Jamo Extended-B
                  or ((Char>=16#D7CB) and (Char=<16#D7FB)) % Hangul Jamo Extended-B
                  or ((Char>=16#3131) and (Char=<16#318E)) % Hangul Compatibility Jamo 
                  or  (Char==17#302E) or  (Char==16#302F)  % Tone marks (used in Middle Korean) 
                  or ((Char>=16#AC00) and (Char=<16#D7A3)) % 11,172 precomposed Hangul syllables
                  or ((Char>=16#3200) and (Char=<16#321E)) % For parenthesised 
                  or ((Char>=16#3260) and (Char=<16#327E)) % and circled 
                  or ((Char>=16#FFDC) and (Char=<16#FFA0)) % For halfwidth 
                  -> true;
is_hangul(_)      -> false.

is_hangul_precomposed(Char) 
    when ((Char>=16#AC00) and (Char=<16#D7A3)) 
        % 11,172 precomposed Hangul syllables
                         -> true;
is_hangul_precomposed(_) -> false.

% Decompose one char of hangul
hangul_decomposition(Char, Result) ->
    SIndex = Char - ?HANGUL_SBASE,
    case (SIndex < 0) or (SIndex >= ?HANGUL_SCOUNT) of
        true -> [Char|Result]; % skip
        false -> 
            L = ?HANGUL_LBASE + (SIndex div ?HANGUL_NCOUNT),
            V = ?HANGUL_VBASE + (SIndex rem ?HANGUL_NCOUNT) div ?HANGUL_TCOUNT,
            T = ?HANGUL_TBASE + (SIndex rem ?HANGUL_TCOUNT),
            
            case T of
                ?HANGUL_TBASE -> [V|[L|Result]];
                _ -> [T|[V|[L|Result]]]
            end
    end.


% Compose hangul characters
% Example:
% hangul_composition(lists:reverse(Str));
hangul_composition([VChar|Tail]) ->
    hangul_composition(Tail, VChar, []);
hangul_composition([]) -> [].

% used in hangul_composition/3
hangul_composition([VChar|Tail], Result) ->
    hangul_composition(Tail, VChar, Result);
hangul_composition([], Result) -> Result.

% String is reversed
hangul_composition([LChar|Tail], VChar, Result) ->
   % 1. check to see if two current characters are L and V
   LIndex = LChar - ?HANGUL_LBASE,
    if 
      (0 =< LIndex) and (LIndex < ?HANGUL_LCOUNT) ->
        VIndex = VChar - ?HANGUL_VBASE,
        if
          (0 =< VIndex) and (VIndex < ?HANGUL_VCOUNT) ->
                 
            LVChar = ?HANGUL_SBASE + ?HANGUL_TCOUNT  
                   * (LIndex * ?HANGUL_VCOUNT + VIndex),
                 
                    % 2. check to see if two current characters are LV and T
                    case Result of % Try get last appended char
                       [] -> hangul_composition(Tail, [LVChar]);             % is first, LV
                       [TChar|Result2] ->
                           TIndex = TChar - ?HANGUL_TBASE,
                           if
                             (0 < TIndex) and (TIndex < ?HANGUL_TCOUNT) ->
                                LVTChar = LVChar + TIndex,
                                hangul_composition(Tail, [LVTChar|Result2]); % is LV&T
                             true ->
                               hangul_composition(Tail, [LVChar|Result])     % is LV
                           end
                    end;
          true -> hangul_composition(Tail, LChar, [VChar|Result])            % skip
        end;
      true -> hangul_composition(Tail, LChar, [VChar|Result])                % skip
     end;
hangul_composition([], Char, Result) ->
    [Char|Result].

%% Convert everything from utf-8 into an NCR (Numeric Character Reference)
to_ncr(Str) -> to_ncr(lists:reverse(Str), []).

to_ncr([Char|Tail], Res) -> to_ncr(Tail, char_to_ncr(Char) ++ Res);
to_ncr([         ], Res) -> Res.

-spec char_to_ncr(char()) -> string().
char_to_ncr(Char) when Char =< 16#7F 
% one-byte character
    -> [Char];
char_to_ncr(Char) when Char =< 16#C2
% non-utf8 character or not a start byte
    -> [];
char_to_ncr(Char) 
    -> lists:flatten(io_lib:format("&#~p;", [Char])).   

%% Split unicode string on graphemes http://en.wikipedia.org/wiki/Grapheme
to_graphemes(Str) ->
    explode_reverse(to_graphemes_raw(Str, [], [])).

%% Returns not reversed result.
to_graphemes_raw([H|T], Buf, Res) ->
    case {ccc(H), Buf} of
        {0, []} -> to_graphemes_raw(T, [H], Res);
        {0, _ } -> to_graphemes_raw(T, [H], [Buf|Res]);
        _       -> to_graphemes_raw(T, [H|Buf], Res)
    end;
to_graphemes_raw([   ], [ ], Res) -> Res;
to_graphemes_raw([   ], Buf, Res) -> [Buf | Res].

%% Compute count of graphemes in the string
length(Str) ->
    len_graphemes(Str, 0).

len(Str) ->
    len_graphemes(Str, 0).

len_graphemes([H|T], Len) ->
    case ccc(H) of
        0 -> len_graphemes(T, Len + 1);
        _ -> len_graphemes(T, Len)
    end;
len_graphemes([   ], Len) -> Len.

%% Reverses string graphemes 
reverse(Str) ->
    reverse_flatten(
        lists:reverse(to_graphemes_raw(Str, [], [])), [], []).

reverse_flatten(    [H|T], [],       Res) ->
    reverse_flatten(T,     H,        Res);
reverse_flatten(    T,     [HH|TT],  Res) ->
    reverse_flatten(T,     TT,       [HH|Res]);
reverse_flatten(    _,     [],       Res) ->
                                     Res.
hex_to_int(Code) ->
    {ok, [Int], []} = io_lib:fread("~16u", Code),
    Int.




% UNICODE COLLATION ALGORITHM
% see Unicode Technical Standard #10
-spec col_non_ignorable(string(), string()) -> less | greater | equal.

% Levels: http://unicode.org/reports/tr10/#Multi-Level%20Comparison
% L1 Base characters
% L2 Accents
% L3 Case
% L4 Punctuation

col_non_ignorable(S1, S2) -> 
    col_compare  (S1, S2, 
        fun ducet/1, 
        fun col_bin_to_list/1).

% TableFun = fun uxstring:ducet/1
%% TableFun returns value from DUCET table
%% ComparatorFun http://unicode.org/reports/tr10/#Variable%20Weighting
col_compare (String1, String2, TableFun, ComparatorFun) ->
    col_compare1(to_nfd(String1), 
                 to_nfd(String2), 
                 [], % Buf 1, contains ducet(Char)
                 [], % Buf 2
                 false, % CompValue 1
                 [], % Accumulator for String 1 - saves values for next levels comparation
                 [], % Accumulator for String 2
                 TableFun, ComparatorFun).

% S2.1 Find the longest initial substring S at each point that has a match in the table.
% S2.1.1 If there are any non-starters following S, process each non-starter C.
% S2.1.2 If C is not blocked from S, find if S + C has a match in the table.
col_extract([     ], _       ) -> % No Any Char
    {[], []};
col_extract([CP|[]], TableFun) -> % Last Char
    {apply(TableFun, [[CP]]), []};
col_extract([CP | Tail], TableFun) ->
    col_extract1(Tail, TableFun, [CP], ccc(CP), [], false).

% There is only one char which was compared.
col_extract1([        ],       TableFun, CPList, _   , Skipped, false ) ->
    {apply(TableFun, [CPList]), lists:reverse(Skipped)};
% ... One or more chars
col_extract1([        ],       TableFun, CPList, _   , Skipped, OldVal) ->
    {OldVal, lists:reverse(Skipped)};
% OldVal = apply(TableFun, [CPList])
col_extract1([CP2|Tail] = Str, TableFun, CPList, Ccc1, Skipped, OldVal) ->
    Ccc2 = ccc(CP2),
    if
        % S2.1.2 If C is not blocked from S, find if S + C has a match in the table.
        (Ccc1<Ccc2) or (Ccc1 == 0) ->
            NewCPList = CPList ++ [CP2],
            % Search in callocation table
            % FIXME: [108,1425,183,97] lower [108,1,903,97] 
            case apply(TableFun, [NewCPList]) of
                % skip char CP2
                [<<0:72>>] -> col_extract1(Tail, TableFun, CPList,    Ccc2, [CP2|Skipped], OldVal);
                % append char CP2
                Bin        -> col_extract1(Tail, TableFun, NewCPList, Ccc2, Skipped, Bin)
            end;
        % Note: A non-starter in a string is called blocked if there is another 
        %       non-starter of the same canonical combining class or zero between 
        %       it and the last character of canonical combining class 0.
        true and (OldVal =/= false) -> {OldVal, lists:reverse(Skipped) ++ Str};
        true and (OldVal ==  false) -> {apply(TableFun, [CPList]), lists:reverse(Skipped) ++ Str}
    end.
    

%%% Compares on L1, collects data for {L2,L3,L4} comparations.
%% Extract chars from the strings.
%
%% ComparatorFun    S2.3 Process collation elements according to the variable-weight setting,
%%                  as described in Section 3.6.2, Variable Weighting.
col_compare1([_|_] = Str1, StrTail2, [], Buf2, CV1, Acc1, Acc2, TableFun, ComparatorFun) ->
    {Buf1, StrTail1} = col_extract(Str1, TableFun), % [<<Flag,L1,L2,...>>, ..]
    col_compare1(StrTail1, StrTail2, Buf1, Buf2, CV1, Acc1, Acc2, TableFun, ComparatorFun);

col_compare1(StrTail1, [_|_] = Str2, Buf1, [], CV1, Acc1, Acc2, TableFun, ComparatorFun) ->
    {Buf2, StrTail2} = col_extract(Str2, TableFun), % [<<Flag,L1,L2,...>>, ..]
    col_compare1(StrTail1, StrTail2, Buf1, Buf2, CV1, Acc1, Acc2, TableFun, ComparatorFun);
    
%% Extracts a non-ignorable L1 from the Str1.
col_compare1(StrTail1, StrTail2, [CV1Raw|Buf1], Buf2, false, Acc1, Acc2, TableFun, ComparatorFun) ->
    case apply(ComparatorFun, [CV1Raw]) of
        [0    | Acc] -> % Find other W1L1
            col_compare1(StrTail1, StrTail2, Buf1, Buf2, false, [Acc|Acc1], Acc2, TableFun, ComparatorFun);
        [_    | _  ] when StrTail2 == [] -> % String 2 was ended :(
            greater;    % Return result: S1 greater S2 on L1 
        [W1L1 | Acc] -> % W1L1 was found. Try find W2L1.
            col_compare1(StrTail1, StrTail2, Buf1, Buf2, W1L1,  [Acc|Acc1], Acc2, TableFun, ComparatorFun)
    end;

%% Extracts a non-ignorable L1 from the Str2.
%% Compares L1 values.
col_compare1(StrTail1, StrTail2, Buf1, [CV2Raw|Buf2], W1L1, Acc1, Acc2, TableFun, ComparatorFun) ->
    case apply(ComparatorFun, [CV2Raw]) of
        [0    | Acc] -> % Find other W2L1
            col_compare1(StrTail1, StrTail2, Buf1, Buf2, W1L1,  Acc1, [Acc|Acc2], TableFun, ComparatorFun);
        [_    | _  ] when W1L1 == true -> 
            lower;   % Sting 1 was ended; string 2 still has a non-ignorable char => string2 greater.
        [W2L1 | _  ] when W1L1 >  W2L1 ->
            greater; % Return result: S1 greater S2 on L1
        [W2L1 | _  ] when W1L1 <  W2L1 ->
            lower;   % Return result: S1 lower S2 on L1
        [W2L1 | Acc] when W1L1 == W2L1 ->
            col_compare1(StrTail1, StrTail2, Buf1, Buf2, false, Acc1, [Acc|Acc2], TableFun, ComparatorFun)
    end;

%% String 2 conrains more codepaints, but we cannot throw them.
col_compare1([], [CP2|StrTail2], [], [],  _,    Acc1, Acc2, TableFun, ComparatorFun) ->
    col_compare1([],  StrTail2,  [], CP2, true, Acc1, Acc2, TableFun, ComparatorFun);

%% String 1 conrains more codepaints, but we cannot throw them.
col_compare1([CP1|StrTail1], [], [],  [], _,     Acc1, Acc2, TableFun, ComparatorFun) ->
    col_compare1(StrTail1,   [], CP1, [], false, Acc1, Acc2, TableFun, ComparatorFun);

%::error:function_clause
%  in function uxstring:col_compare1/9
%  called as col_compare1([],[],[<<1,2,123,0,32,0,2,0,33>>],[],513,[[32,2]],[[124,2],[346,2]],#Fun,#Fun)
col_compare1([], [], _,            [], W1L1, _,    _,    _,        _            ) when W1L1 >  0 ->
    greater;
col_compare1([], [], [W1Raw|Buf1], [], W1L1, Acc1, Acc2, TableFun, ComparatorFun) when W1L1 == 0 ->
    [W1L1New | Acc] = apply(ComparatorFun, [W1Raw]),
    col_compare1([], [], Buf1, [], W1L1New, [Acc|Acc1], Acc2, TableFun, ComparatorFun);

%% L1 was ended :(
%% Now, Funs are not neeaded.
%% Acc1 and Acc2 are reversed.
col_compare1([], [], [], [], false, Acc1, Acc2, _, _) ->
    col_compare2(lists:reverse(Acc1), 
                 lists:reverse(Acc2),
                 false, % W1L{2,3,4} 
                 [], % Other accumulator. Contains L3 and L4.
                 []  % Other acc...
                ).

%%% L2 comparation.
%% Try extract W1LX, but 0 was found => try next weigth in InAcc.
col_compare2([ [] | _], _, _, _, _) ->
    equal;
col_compare2([ [0   |OutAcc] | InAccTail1], InAcc2, false, OutAcc1, OutAcc2) ->
    col_compare2(InAccTail1, InAcc2, false, [OutAcc|OutAcc1], OutAcc2);
%% W1LX was found. => Try found W2LX.
col_compare2([ [W1LX|OutAcc] | InAccTail1], InAcc2, false, OutAcc1, OutAcc2) ->
    col_compare2(InAccTail1, InAcc2, W1LX, [OutAcc|OutAcc1], OutAcc2);

%% Try extract W2LX.
col_compare2(InAcc1, [ [0   |OutAcc] | InAccTail2], W1LX, OutAcc1, OutAcc2) ->
    col_compare2(InAcc1, InAccTail2, W1LX, OutAcc1, [OutAcc|OutAcc2]);
    
col_compare2(InAcc1, [ [W2LX|OutAcc] | InAccTail2], W1LX, OutAcc1, OutAcc2) when W1LX <  W2LX ->
    lower;
col_compare2(InAcc1, [ [W2LX|OutAcc] | InAccTail2], W1LX, OutAcc1, OutAcc2) when W1LX >  W2LX ->
    greater;
col_compare2(InAcc1, [ [W2LX|OutAcc] | InAccTail2], W1LX, OutAcc1, OutAcc2) when W1LX == W2LX ->
    col_compare2(InAcc1, InAccTail2, false, OutAcc1, [OutAcc|OutAcc2]);

% Try extract from Str1, which is empty.
col_compare2([], [W2LX|_  ], false, _, _) when W2LX>0 ->
    lower;
col_compare2([], [W2LX|Acc], false, Acc1, Acc2) when W2LX == 0 ->
    col_compare2([], Acc, false, Acc1, [Acc|Acc2]);

% Str2 was ended.
col_compare2(_, [], W1LX, _, _) when W1LX =/= false ->
    greater;

% Try compare on next composition level. 
col_compare2([], [], W1LX, [_|_] = OutAcc1, [_|_] = OutAcc2) when W1LX == false ->
    col_compare2(lists:reverse(OutAcc1), lists:reverse(OutAcc2), false, [], []);

% End compares
col_compare2([], [], W1LX, [], []) when W1LX == false ->
    equal. % on all levels


% Convert binary from DUCET to list [L1, L2, L3, L4]
col_bin_to_list(<<_:8, L1:16, L2:16, L3:16, L4:16>>) ->
    [L1, L2, L3];
col_bin_to_list(<<_:8, L1:16, L2:16, L3:16, L4:24>>) ->
    [L1, L2, L3].

%% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %% %%
%%
%% Tests
%%
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).

-define(NFTESTDATA, ?UNIDATA_DIRECTORY ++ "NormalizationTest.txt"). 

explode_test_() ->
	M = 'uxstring',
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

htmlspecialchars_test_() -> htmlspecialchars_test_X('htmlspecialchars').
hsc_test_() -> htmlspecialchars_test_X('hsc').

htmlspecialchars_test_X(F) ->
	M = 'uxstring',
	[?_assertEqual(M:F("ddf2#$\""), "ddf2#$&quot;")
	,?_assertEqual(M:F("test1 & test2"), "test1 &amp; test2")
	].
to_lower_test_() ->
	M = 'uxstring',
	F = 'to_lower',
	[?_assertEqual(M:F("small BIG"), "small big")
	,?_assertEqual(M:F(	[1069,1056,1051,1040,1053,1043]), 
				[1101,1088,1083,1072,1085,1075])
	].
to_upper_test_() ->
	M = 'uxstring',
	F = 'to_upper',
	[?_assertEqual(M:F("small BIG"), "SMALL BIG")
	,?_assertEqual(M:F(	[1101,1088,1083,1072,1085,1075]),
				[1069,1056,1051,1040,1053,1043])
	].

strip_tags_test_() ->
	strip_tags_test_X('strip_tags').
st_test_() ->
	strip_tags_test_X('st').

strip_tags_test_X(F) ->
	M = 'uxstring',
	[?_assertEqual(M:F("<b>a</b>"), "a")
	,?_assertEqual(M:F("<b>a b c</b>"), "a b c")
% Check a long tag
	,?_assertEqual(M:F("<H1>A B C</H1>"), "A B C")
	,?_assertEqual(M:F("a<img src='i.img' />b"), "ab")
% Check allowed tags
	,?_assertEqual(M:F("<b>a b c</b>", ["b"]), "<b>a b c</b>")
	,?_assertEqual(M:F("<B>a b c</B>", ["b"]), "<B>a b c</B>")
	,?_assertEqual(M:F("<code>a b c</code>", ["b"]), "a b c")
	,?_assertEqual(M:F("<code>a b c</code>", ["b", "code"]), "<code>a b c</code>")
	,?_assertEqual(M:F("<span>a b c</span>", ["b", "span"]), "<span>a b c</span>")
% Check a tag with an attribute
	,?_assertEqual(M:F("a<img src='i.gif' />b", ["b"]), "ab")
	,?_assertEqual(M:F("a<img src='i.gif' />b", ["img"]), "a<img src='i.gif' />b")
	,?_assertEqual(M:F("a<br/>b", ["br"]), "a<br/>b")
% Check an atom in the list allowed tags 
	,?_assertEqual(M:F("a<br/>b", [br]), "a<br/>b")
	,?_assertEqual(M:F("a<br/><b>b</b>", [br]), "a<br/>b")
% Check a replacement argument
	,?_assertEqual(M:F("<b>a b c</b>", [], " "), " a b c ")
	,?_assertEqual(M:F("<b>a b c</b>", [], "tag"), "taga b ctag")
	,?_assertEqual(M:F("<b>a b c</b>", [test], "tag"), "taga b ctag")
% PHP style
	,?_assertEqual(M:F("<b>a b c</b>", "<b>"), "<b>a b c</b>")
	,?_assertEqual(M:F("<span>a b c</span>", "<b><span>"), "<span>a b c</span>")
	,?_assertEqual(M:F("<a><b>test<a", "a"), "<a>test")
	].
tags_to_list_test_() ->
	M = 'uxstring',
	F = 'tags_to_list',
	[?_assertEqual(M:F("<a><b>"), ["b", "a"])
	,?_assertEqual(M:F("<span>"), ["span"])
	,?_assertEqual(M:F("<b><span>"), ["span", "b"])
	,?_assertEqual(M:F("<i>"), ["i"])
	].
delete_types_test_() ->
	M = 'uxstring',
	F = 'delete_types',
	[?_assertEqual(M:F([ll, lu], "Tom Cat!"), " !")
	,?_assertEqual(M:F([ll],     "Tom Cat!"), "T C!")
	,?_assertEqual(M:F([po],     "Tom Cat!"), "Tom Cat")
	,?_assertEqual(M:F([ll], "AaBbCc44ff", -2), "ABbCc44ff") %skip 2 (A,B)
	,?_assertEqual(M:F([ll], "AaBbCc44ff",  2), "ABCc44ff") %del 2 (a,b)
	,?_assertEqual(M:F([ll], "AaBbCc44ffdsBAF",  4), "ABC44fdsBAF")
	,?_assertEqual(M:F([ll], "AaBbCc44ffdsBAF", -4), "ABC44ffdsBAF")
	].
filter_types_test_() ->
	M = 'uxstring',
	F = 'filter_types',
	[?_assertEqual(M:F([ll, lu], "Tom Cat!"), "TomCat")
	,?_assertEqual(M:F([ll],     "Tom Cat!"), "omat")
	,?_assertEqual(M:F([po],     "Tom Cat!"), "!")
	,?_assertEqual(M:F([ll], "AaBbCc44ffds",  3), "abc44ffds")
	,?_assertEqual(M:F([ll], "AaBbCc44ffds",  4), "abcffds")
	,?_assertEqual(M:F([ll], "AaBbCc44ffds", -2), "abCc44ffds")
	,?_assertEqual(M:F([ll], "AaBbCc44ffds", -4), "abc4ffds")
	].
char_types_test_() ->
	M = 'uxstring',
	F = 'char_types',
	[?_assertEqual(M:F("Tom Cat!"), [lu,ll,ll,zs,lu,ll,ll,po])
	%,?_assertEqual(M:F(), )
	].
last_types_test_() ->
	M = 'uxstring',
	F = 'last_types',
	[?_assertEqual(M:F([ll], "AavbfFDsdfffd9s9999", -5), "99999")
	,?_assertEqual(M:F([ll], "AavbfFDsdfffd9s9999", -6), "D99999")
	,?_assertEqual(M:F([ll], "AavbfFDsdfffd9s9999", -7), "FD99999")
	,?_assertEqual(M:F([ll], "AavbfFDsdfffd9s9999", -8), "AFD99999")
	].
first_types_test_() ->
	M = 'uxstring',
	F = 'first_types',
	[?_assertEqual(M:F([ll], "AavbfFDsdfffds", 4), "avbf")
	,?_assertEqual(M:F([ll], "AavbfFDsdfffds", 5), "avbfs")
	].

%    NFC
%      c2 ==  NFC(c1) ==  NFC(c2) ==  NFC(c3)
%      c4 ==  NFC(c4) ==  NFC(c5)
%
%    NFD
%      c3 ==  NFD(c1) ==  NFD(c2) ==  NFD(c3)
%      c5 ==  NFD(c4) ==  NFD(c5)
%
%    NFKC
%      c4 == NFKC(c1) == NFKC(c2) == NFKC(c3) == NFKC(c4) == NFKC(c5)
%
%    NFKD
%      c5 == NFKD(c1) == NFKD(c2) == NFKD(c3) == NFKD(c4) == NFKD(c5)

nfc_test(_, 0) -> max;
nfc_test(InFd, Max) ->
    NFC  = fun 'uxstring':to_nfc/1,
    NFD  = fun 'uxstring':to_nfd/1,
    NFKC = fun 'uxstring':to_nfkc/1,
    NFKD = fun 'uxstring':to_nfkd/1,

    case file:read_line(InFd) of
        eof -> ok;
        {ok, Data} -> 
            try
              [LineWithoutComment|_] = uxstring:explode("#", Data),
              lists:map(fun (Str) -> % Convert string from file to list of integers 
                            lists:map(fun hex_to_int/1, string:tokens(Str, " ")) 
                        end,
                        uxstring:explode(";", LineWithoutComment))
            of 
                Row when length(Row) == 6 ->
                   % start body
                   C1 = lists:nth(1, Row),
                   C2 = lists:nth(2, Row),
                   C3 = lists:nth(3, Row),
                   C4 = lists:nth(4, Row),
                   C5 = lists:nth(5, Row),
                   % {Result from function, From, To}
                   %NFD
                   ?assertEqual({Max,C3, C1, C3}, {Max,NFD(C1), C1, C3}),
                   ?assertEqual({C3, C2, C3}, {NFD(C2), C2, C3}),
                   ?assertEqual({C3, C3, C3}, {NFD(C3), C3, C3}),
                   ?assertEqual({C5, C4, C5}, {NFD(C4), C4, C5}),
                   ?assertEqual({C5, C5, C5}, {NFD(C5), C5, C5}),
                   %NFC
                   ?assertEqual({Max, C2, C1, C2}, {Max, NFC(C1), C1, C2}),
                   ?assertEqual({C2, C2, C2}, {NFC(C2), C2, C2}),
                   ?assertEqual({C2, C3, C2}, {NFC(C3), C3, C2}),
                   ?assertEqual({C4, C4, C4}, {NFC(C4), C4, C4}),
                   ?assertEqual({C4, C5, C4}, {NFC(C5), C5, C4}),
                   %NFKC
                   ?assertEqual({C4, C1}, {NFKC(C1), C1}),
                   ?assertEqual({C4, C2}, {NFKC(C2), C2}),
                   ?assertEqual({C4, C3}, {NFKC(C3), C3}),
                   ?assertEqual({C4, C4}, {NFKC(C4), C4}),
                   ?assertEqual({C4, C5}, {NFKC(C5), C5}),
                   %NFCD
                   ?assertEqual({C5, C1}, {NFKD(C1), C1}),
                   ?assertEqual({C5, C2}, {NFKD(C2), C2}),
                   ?assertEqual({C5, C3}, {NFKD(C3), C3}),
                   ?assertEqual({C5, C4}, {NFKD(C4), C4}),
                   ?assertEqual({C5, C5}, {NFKD(C5), C5})
                   % end body

            catch error:_ -> next
            after 
                nfc_test(InFd, Max - 1)
            end
    end.

nfc_prof(Count) ->
    {ok, InFd} = file:open(?NFTESTDATA, [read]),
    io:setopts(InFd,[{encoding,utf8}]),
    nfc_test(InFd, Count),
    ok.

nfc_test_() ->
    {timeout, 600, fun() -> nfc_prof(100) end}.

% Collation Test
calloc_test(_,    _, _,      0)   -> max;
calloc_test(InFd, F, false,  Max) ->
    OldVal = calloc_test_read(InFd),
    calloc_test(InFd, F, OldVal,  Max);
calloc_test(InFd, F, OldVal, Max) ->
    case calloc_test_read(InFd) of
        Val when is_list(Val) -> 
            case F(Val, OldVal) of
                lower -> io:format(user, "Error: ~w ~w ~w ~n", [Val, lower, OldVal]),
                         calloc_test(InFd, F, Val, Max - 1);
                _     -> calloc_test(InFd, F, Val, Max - 1)
            end;
        _ -> ok
    end.

%% Read line from a testdata file (see CollationTest.html)
%% Return list of codepaints
calloc_test_read(InFd) ->
    case file:read_line(InFd) of
        eof -> ok;
        {ok, Data} -> 
            try
                [Value|_] = uxstring:split(["#", ";", "\n"], Data), 
                %% Converts "0009 0021" to [16#0009, 16#0021]
                lists:map(fun uxstring:hex_to_int/1, string:tokens(Value, " "))
            of Res -> Res
            catch                   
                error:_Reason -> calloc_test_read(InFd)
            end
    end.

calloc_prof(File, Fun, Count) ->
    {ok, InFd} = file:open(File, [read]),
            io:setopts(InFd,[{encoding,utf8}]),
            calloc_test(InFd, Fun, false, Count),
            ok.

calloc_test_() ->
    {timeout, 600, fun() -> 
        calloc_prof(?COLLATION_TEST_DATA_DIRECTORY 
                        ++ "CollationTest_NON_IGNORABLE_SHORT.txt", 
                    fun col_non_ignorable/2, 
                    10000000) end}.


-endif.
