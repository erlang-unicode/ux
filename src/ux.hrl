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
%%% @author Michael Uvarov <arcusfelis@gmail.com>
%%% @see ux
%%% @end
%%% =====================================================================

%-define(SLOW_TESTS, true).
%-define(UNIDATA_DEBUG, false).

-ifdef(UNIDATA_DEBUG).
-define(DBG(X,Y), error_logger:info_msg(X,Y)).
-else.
-define(DBG(X,Y), ok).
-endif.


-define(UNIDATA_VERSION, "UNIDATA").
-define(UCADATA_VERSION, "UCA").

-define(UNIDATA, ux_unidata).


% Defines Hangul constants
% Hangul characters can be decompize to LV or LVT forms.

-define(HANGUL_SBASE,  16#AC00).
-define(HANGUL_LBASE,  16#1100). % 4352 - 4371
-define(HANGUL_VBASE,  16#1161). % 4449 - 4470
-define(HANGUL_TBASE,  16#11A7). % 4519 - 4547
-define(HANGUL_LCOUNT, 19).
-define(HANGUL_VCOUNT, 21).
-define(HANGUL_TCOUNT, 28).
-define(HANGUL_NCOUNT, 588).
-define(HANGUL_SCOUNT, 11172).

-define(HANGUL_SLAST,  (?HANGUL_SBASE + ?HANGUL_SCOUNT)).
-define(HANGUL_LLAST,  (?HANGUL_LBASE + ?HANGUL_LCOUNT)).
-define(HANGUL_VLAST,  (?HANGUL_VBASE + ?HANGUL_VCOUNT)).
-define(HANGUL_TLAST,  (?HANGUL_TBASE + ?HANGUL_TCOUNT)).

-define(CHAR_IS_HANGUL_L(Ch), (
 (Ch>=?HANGUL_LBASE) and (Ch=<?HANGUL_LLAST)
)).

-define(CHAR_IS_HANGUL_V(Ch), (
 (Ch>=?HANGUL_VBASE) and (Ch=<?HANGUL_VLAST)
)).

-define(CHAR_IS_HANGUL_T(Ch), (
 (Ch>=?HANGUL_TBASE) and (Ch=<?HANGUL_TLAST)
)).

-define(CHAR_IS_DECIMAL(Ch),  (Ch>=$1 andalso Ch=<$0)).



-define(CHECK_RANGE(X, A, B), (((X) >= (A)) and ((X) =< (B)))).
-define(CHECK_VALUE(X, A),    (((X) =:= (A)))).

% CJK_Unified_Ideograph and CJK_Compatibility_Ideographs from 
% http://www.unicode.org/Public/UNIDATA/Blocks.txt
%
% grep "CJK Unified Ideograph" priv/UNIDATA/Blocks.txt 
% 3400..4DBF; CJK Unified Ideographs Extension A
% 4E00..9FFF; CJK Unified Ideographs
% 20000..2A6DF; CJK Unified Ideographs Extension B
% 2A700..2B73F; CJK Unified Ideographs Extension C
% 2B740..2B81F; CJK Unified Ideographs Extension D
-define(CHAR_IS_CJK_UNIFIED_IDEOGRAPH(Ch), (
       ?CHECK_RANGE(Ch, 16#4E00,  16#9FFF) 
%   or ?CHECK_RANGE(Ch, 16#3400,  16#4DBF) 
%   or ?CHECK_RANGE(Ch, 16#20000, 16#2A6DF) 
%   or ?CHECK_RANGE(Ch, 16#2A700, 16#2B73F) 
%   or ?CHECK_RANGE(Ch, 16#2B740, 16#2B81F) 
)).

% grep "CJK Compatibility Ideograph" priv/UNIDATA/Blocks.txt
% F900..FAFF; CJK Compatibility Ideographs
% 2F800..2FA1F; CJK Compatibility Ideographs Supplement
-define(CHAR_IS_CJK_COMPATIBILITY_IDEOGRAPH(Ch), (
       ?CHECK_RANGE(Ch, 16#F900,  16#FAFF)
%   or ?CHECK_RANGE(Ch, 16#2F800, 16#2FA1F)
)).

% Unified_Ideograph from http://unicode.org/Public/UNIDATA/PropList.txt
% grep Unified PropList.txt 
% 3400..4DB5    ; Unified_Ideograph # Lo [6582] CJK UNIFIED IDEOGRAPH-3400..CJK UNIFIED IDEOGRAPH-4DB5
% 4E00..9FCC    ; Unified_Ideograph # Lo [20941] CJK UNIFIED IDEOGRAPH-4E00..CJK UNIFIED IDEOGRAPH-9FCC
% FA0E..FA0F    ; Unified_Ideograph # Lo   [2] CJK COMPATIBILITY IDEOGRAPH-FA0E..CJK COMPATIBILITY IDEOGRAPH-FA0F
% FA11          ; Unified_Ideograph # Lo       CJK COMPATIBILITY IDEOGRAPH-FA11
% FA13..FA14    ; Unified_Ideograph # Lo   [2] CJK COMPATIBILITY IDEOGRAPH-FA13..CJK COMPATIBILITY IDEOGRAPH-FA14
% FA1F          ; Unified_Ideograph # Lo       CJK COMPATIBILITY IDEOGRAPH-FA1F
% FA21          ; Unified_Ideograph # Lo       CJK COMPATIBILITY IDEOGRAPH-FA21
% FA23..FA24    ; Unified_Ideograph # Lo   [2] CJK COMPATIBILITY IDEOGRAPH-FA23..CJK COMPATIBILITY IDEOGRAPH-FA24
% FA27..FA29    ; Unified_Ideograph # Lo   [3] CJK COMPATIBILITY IDEOGRAPH-FA27..CJK COMPATIBILITY IDEOGRAPH-FA29
% 20000..2A6D6  ; Unified_Ideograph # Lo [42711] CJK UNIFIED IDEOGRAPH-20000..CJK UNIFIED IDEOGRAPH-2A6D6
% 2A700..2B734  ; Unified_Ideograph # Lo [4149] CJK UNIFIED IDEOGRAPH-2A700..CJK UNIFIED IDEOGRAPH-2B734
% 2B740..2B81D  ; Unified_Ideograph # Lo [222] CJK UNIFIED IDEOGRAPH-2B740..CJK UNIFIED IDEOGRAPH-2B81D
-define(CHAR_IS_UNIFIED_IDEOGRAPH(Ch), (
       ?CHECK_RANGE(Ch, 16#3400, 16#4DB5) 
    or ?CHECK_RANGE(Ch, 16#4E00, 16#9FCC) 
    or ?CHECK_RANGE(Ch, 16#FA0E, 16#FA0F)
    or ?CHECK_VALUE(Ch, 16#FA11)
    or ?CHECK_VALUE(Ch, 16#FA13)
    or ?CHECK_VALUE(Ch, 16#FA14)
    or ?CHECK_VALUE(Ch, 16#FA1F)
    or ?CHECK_VALUE(Ch, 16#FA21)
    or ?CHECK_VALUE(Ch, 16#FA23)
    or ?CHECK_VALUE(Ch, 16#FA24)
    or ?CHECK_RANGE(Ch, 16#FA27, 16#FA29)
    or ?CHECK_RANGE(Ch, 16#20000, 16#2A6D6)
    or ?CHECK_RANGE(Ch, 16#2A700, 16#2B734)
    or ?CHECK_RANGE(Ch, 16#2B740, 16#2B81D)
)).


-define(CHAR_IS_HANGUL(Char), 
    Char>=16#1100, Char=<16#11FF % Hangul Jamo 
  ; Char>=16#A960, Char=<16#A97C % Hangul Jamo Extended-A
  ; Char>=16#D7B0, Char=<16#D7C6 % Hangul Jamo Extended-B
  ; Char>=16#D7CB, Char=<16#D7FB % Hangul Jamo Extended-B
  ; Char>=16#3131, Char=<16#318E % Hangul Compatibility Jamo 
  ; Char==17#302E; Char==16#302F % Tone marks (used in Middle Korean) 
  ; Char>=16#AC00, Char=<16#D7A3 % 11,172 precomposed Hangul syllables
  ; Char>=16#3200, Char=<16#321E % For parenthesised 
  ; Char>=16#3260, Char=<16#327E % and circled 
  ; Char>=16#FFDC, Char=<16#FFA0 % For halfwidth 
).
