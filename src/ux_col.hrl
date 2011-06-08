% vim: set filetype=erlang shiftwidth=4 tabstop=4 expandtab tw=80:
%%% User Extentions for Erlang 
%%%
%%% @package  ux_col
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

% Hangul & UCA
-define(COL_HANGUL_LBASE,  12337). % 12337 - 12356
-define(COL_HANGUL_VBASE,  12463). % 12463 - 12484
-define(COL_HANGUL_TBASE,  12533). % 12533 - 12584

-define(COL_HANGUL_LLAST,  ?COL_HANGUL_LBASE + ?HANGUL_LCOUNT).
-define(COL_HANGUL_VLAST,  ?COL_HANGUL_VBASE + ?HANGUL_VCOUNT).
-define(COL_HANGUL_TLAST,  ?COL_HANGUL_TBASE + ?HANGUL_TCOUNT).

% TERMINATOR < T <  V < L
-define(COL_HANGUL_TERMINATOR, 13000). % 12337 - 68

% Weight on level 1 (L1) is L1 of Hangul jamo L.
-define(IS_L1_OF_HANGUL_L(W), (
 (W>=?COL_HANGUL_LBASE) and (W=<?COL_HANGUL_LLAST)
)).

% Weight on level 1 (L1) is L1 of Hangul jamo V.
-define(IS_L1_OF_HANGUL_V(W), (
 (W>=?COL_HANGUL_VBASE) and (W=<?COL_HANGUL_VLAST)
)).

% Weight on level 1 (L1) is L1 of Hangul jamo T.
-define(IS_L1_OF_HANGUL_T(W), (
 (W>=?COL_HANGUL_TBASE) and (W=<?COL_HANGUL_TLAST)
)).


-define(COL_DECIMAL_START, 5529). % L1 of 0
-define(COL_DECIMAL_END, 5538). % L1 of 9
-define(IS_L1_OF_DECIMAL(W), (
 (W>=?COL_DECIMAL_START) and (W=<?COL_DECIMAL_END)
)).
-define(COL_WEIGHT_TO_DECIMAL(W), (
 W - ?COL_DECIMAL_START 
)).

% Records
-record(uca_options, {
        hangul_terminator = ?COL_HANGUL_TERMINATOR,
        natural_sort = true,
        strength = 4,
        alternate = shifted,
        case_sensitive = false, % move L3 to L1
        case_first = lower, % off | upper | lower
        sort_key_format = binary % binary | list | uncompressed (list)
        }).
