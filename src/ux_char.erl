% vim: set filetype=erlang shiftwidth=4 tabstop=4 expandtab tw=80:
%%%
%%% @copyright 2010-2011 Michael Uvarov
%%% @author Michael Uvarov <arcusfelis@gmail.com>
%%%
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
%%% =====================================================================

%%% =====================================================================
%%%   Copyright 2011 Uvarov Michael 
%%%
%%%   Licensed under the Apache License, Version 2.0 (the "License");
%%%   you may not use this file except in compliance with the License.
%%%   You may obtain a copy of the License at
%%%
%%%       http://www.apache.org/licenses/LICENSE-2.0
%%%
%%%   Unless required by applicable law or agreed to in writing, software
%%%   distributed under the License is distributed on an "AS IS" BASIS,
%%%   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%%   See the License for the specific language governing permissions and
%%%   limitations under the License.
%%%
%%% $Id$
%%% =====================================================================


%%% @doc Character functions.


-module(ux_char).
-author('Uvarov Michael <arcusfelis@gmail.com>').

-export([comment/1, type/1, block/1, script/1,
        to_lower/1, to_upper/1, to_ncr/1,
        is_lower/1, is_upper/1, 
        is_letter/1, is_number/1, is_decimal/1, is_mark/1, 
        is_separator/1, is_punctuation_mark/1, 
        is_hangul/1, is_acsii/1, 
        is_cjk_compatibility_ideograph/1, 
        is_cjk_unified_ideograph/1, 
        is_unified_ideograph/1, 
        is_hangul_precomposed/1 
        ]).
-include("ux.hrl").

-type char_type() :: ux_types:char_type().


-spec to_lower(char()) -> char(); 
        (skip_check) -> fun().

to_lower(V) -> 
    ?UNIDATA:char_to_lower(V).


-spec to_upper(char()) -> char(); 
        (skip_check) -> fun().

to_upper(V) -> 
    ?UNIDATA:char_to_upper(V).


-spec is_lower(char()) -> boolean();
        (skip_check) -> fun().

is_lower(V) -> 
    ?UNIDATA:is_lower(V).


-spec is_upper(char()) -> boolean(); 
        (skip_check) -> fun().

is_upper(V) -> 
    ?UNIDATA:is_upper(V).


-spec comment(char()) -> binary();
        (skip_check) -> fun().

comment(V) -> 
    ?UNIDATA:char_comment(V).


-spec type(char()) -> char_type(); 
        (skip_check) -> fun().

type(V) -> 
    ?UNIDATA:char_type(V).


-spec is_acsii(char()) -> boolean().
is_acsii(Char) when (Char>=0) and (Char=<16#7F) -> true;
is_acsii(_) -> false.

%% @doc Returns true, if C is a letter.
-spec is_letter(C::char()) -> boolean().

is_letter(C) ->
    case erlang:atom_to_list(type(C)) of
    [$L,_] -> true;
    _      -> false
    end.

is_mark(C) ->
    case erlang:atom_to_list(type(C)) of
    [$M,_] -> true;
    _      -> false
    end.

%% @doc Return true, if C is a decimal number.
-spec is_decimal(C::char()) -> boolean().

is_decimal(C) -> type(C) == 'Nd'.


%% @doc Returns true, if is C is a number.
-spec is_number(C::char()) -> boolean().

is_number(C) ->
    case erlang:atom_to_list(type(C)) of
    [$N,_] -> true;
    _      -> false
    end.

%% @doc Return true, if is C is a separator.
-spec is_separator(C::char()) -> boolean().

is_separator(C) ->
    case erlang:atom_to_list(type(C)) of
    [$Z,_] -> true;
    _      -> false
    end.

%% @doc Returns true, if is C is a punctiation mark.
-spec is_punctuation_mark(C::char()) -> boolean().

is_punctuation_mark(C) ->
    case erlang:atom_to_list(type(C)) of
    [$P,_] -> true;
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
    ?CHAR_IS_HANGUL(Char)
             -> true;
is_hangul(_) -> false.

is_hangul_precomposed(Char)
    when Char>=16#AC00, Char=<16#D7A3
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

-spec block(char) -> atom();
        (skip_check) -> fun().
block(V) -> ?UNIDATA:char_block(V).

-spec script(char) -> atom();
        (skip_check) -> fun().
script(V) -> ?UNIDATA:char_script(V).
