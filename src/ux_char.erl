% vim: set filetype=erlang shiftwidth=4 tabstop=4 expandtab tw=80:
%%% User Extentions for Erlang 
%%%
%%% @package  ux_char
%%% @author   Uvarov Michael <freeakk@gmail.com>
%%% @license  http://www.fsf.org/copyleft/lgpl.html LGPL
%%%
%%% @copyright 2010 Uvarov Michael.
%%% %CopyrightBegin%
%%%  Copyright 2010 Uvarov Michael  
%%%
%%%  See the enclosed file COPYING for license information (LGPL). If you
%%%  did not receive this file, see http://www.fsf.org/copyleft/lgpl.html
%%% %CopyrightEnd%


-module(ux_char).
-author('Uvarov Michael <freeakk@gmail.com>').

-export([comment/1, type/1, block/1,
        to_lower/1, to_upper/1, to_ncr/1,
        is_lower/1, is_upper/1, 
        is_letter/1, is_number/1, is_decimal/1,  
        is_separator/1, is_punctuation_mark/1, 
        is_hangul/1, is_acsii/1, 
        is_cjk_compatibility_ideograph/1, 
        is_cjk_unified_ideograph/1, 
        is_unified_ideograph/1, 
        is_hangul_precomposed/1 
        ]).
-include("ux_char.hrl").

to_lower(V) -> ux_unidata:char_to_lower(V).
to_upper(V) -> ux_unidata:char_to_upper(V).
is_lower(V) -> ux_unidata:is_lower(V).
is_upper(V) -> ux_unidata:is_upper(V).

comment(V) -> ux_unidata:char_comment(V).
type(V) -> ux_unidata:char_type(V).

is_acsii(Char) when (Char>=0) and (Char=<16#7F) -> true;
is_acsii(_) -> false.

%% @doc Returns true, if C is a letter.
-spec is_letter(C::char()) -> boolean().

is_letter(C) ->
    case erlang:atom_to_list(type(C)) of
    [$l,_] -> true;
    _      -> false
    end.

%% @doc Return true, if C is a decimal number.
-spec is_decimal(C::char()) -> boolean().

is_decimal(C) -> type(C) == nd.


%% @doc Returns true, if is C is a number.
-spec is_number(C::char()) -> boolean().

is_number(C) ->
    case erlang:atom_to_list(type(C)) of
    [$n,_] -> true;
    _      -> false
    end.

%% @doc Return true, if is C is a separator.
-spec is_separator(C::char()) -> boolean().

is_separator(C) ->
    case erlang:atom_to_list(type(C)) of
    [$z,_] -> true;
    _      -> false
    end.

%% @doc Returns true, if is C is a punctiation mark.
-spec is_punctuation_mark(C::char()) -> boolean().

is_punctuation_mark(C) ->
    case erlang:atom_to_list(type(C)) of
    [$p,_] -> true;
    _      -> false
    end.

-spec to_ncr(char()) -> string().
to_ncr(Char) when Char =< 16#7F ->
    [Char]; % one-byte character
to_ncr(Char) when Char =< 16#C2 ->
    [];     % non-utf8 character or not a start byte
to_ncr(Char) ->
    lists:flatten(io_lib:format("&#~p;", [Char])).


%% http://unicode.org/reports/tr15/#Hangul
is_hangul(Char) when
     ((Char>=16#1100) and (Char=<16#11FF)) % Hangul Jamo
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
is_hangul(_) -> false.

is_hangul_precomposed(Char)
    when ((Char>=16#AC00) and (Char=<16#D7A3))
        % 11,172 precomposed Hangul syllables
                         -> true;
is_hangul_precomposed(_) -> false.

is_cjk_compatibility_ideograph(Ch) when
    ?CHAR_IS_CJK_COMPATIBILITY_IDEOGRAPH(Ch) -> true;
is_cjk_compatibility_ideograph(_) -> false.

is_cjk_unified_ideograph(Ch) when
    ?CHAR_IS_CJK_UNIFIED_IDEOGRAPH(Ch) -> true;
is_cjk_unified_ideograph(_) -> false.

is_unified_ideograph(Ch) when
    ?CHAR_IS_UNIFIED_IDEOGRAPH(Ch) -> true;
is_unified_ideograph(_) -> false.

block(V) -> ux_unidata:char_block(V).

