%%% User Extentions for Erlang 
%%%
%%% @package  ux_string
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

-define(HANGUL_SLAST,  ?HANGUL_SBASE + ?HANGUL_SCOUNT).
-define(HANGUL_LLAST,  ?HANGUL_LBASE + ?HANGUL_LCOUNT).
-define(HANGUL_VLAST,  ?HANGUL_VBASE + ?HANGUL_VCOUNT).
-define(HANGUL_TLAST,  ?HANGUL_TBASE + ?HANGUL_TCOUNT).

-define(CHAR_IS_HANGUL_L(Ch), (
 (Ch>=?HANGUL_LBASE) and (Ch=<?HANGUL_LLAST)
)).

-define(CHAR_IS_HANGUL_V(Ch), (
 (Ch>=?HANGUL_VBASE) and (Ch=<?HANGUL_VLAST)
)).

-define(CHAR_IS_HANGUL_T(Ch), (
 (Ch>=?HANGUL_TBASE) and (Ch=<?HANGUL_TLAST)
)).

% Records
-record(unistr_info, {
        str,            % Input string
        comment,        % Array of char's comments
        nfd,            % Normal Form D
        nfc,            % Normal Form C
        ducet,          % Weight's from ducet for each symbol
        ccc,            % Canonical Combining Classes 
        col_sort_array_non_ignorable, % Collation sort array
        col_sort_array_blanked, % Collation sort array
        col_sort_array_shifted, % Collation sort array
        col_sort_array_shift_trimmed, % Collation sort array
        blocks          % Blocks from Unicode Character Database
        }).
