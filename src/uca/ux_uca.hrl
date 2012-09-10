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

% TERMINATOR < T < V < L
-define(COL_HANGUL_TERMINATOR, 6139). 

% Records
-record(uca_options, {
    % Generator options
    hangul_terminator = ?COL_HANGUL_TERMINATOR :: ux_uca:uca_weight(),
    natural_sort = true :: boolean(),
    strength = 4 :: ux_uca:uca_strength(),
    alternate = shifted :: ux_uca:uca_alternate(),
    case_sensitive = false, % move L3 to L1 if true
    case_first = lower :: ux_uca:uca_case_first(), 
    backwards = false :: boolean(),

    % Other options
    sort_key_format = binary :: ux_uca:uca_sort_key_format()
    }).











%%
%% Some constants
%%



% Hangul & UCA
-define(COL_HANGUL_LBASE, 12603). 
-define(COL_HANGUL_VBASE, 12729). 
-define(COL_HANGUL_TBASE, 12799). % 31FF

-define(COL_HANGUL_LLAST, (?COL_HANGUL_LBASE + ?HANGUL_LCOUNT)).
-define(COL_HANGUL_VLAST, (?COL_HANGUL_VBASE + ?HANGUL_VCOUNT)).
-define(COL_HANGUL_TLAST, 12850).

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


-define(COL_DECIMAL_START, 5578). % L1 of 0
-define(COL_DECIMAL_END,   (?COL_DECIMAL_START+9)). % L1 of 9
-define(IS_L1_OF_DECIMAL(W), (
 (W>=?COL_DECIMAL_START) and (W=<?COL_DECIMAL_END)
)).
-define(COL_WEIGHT_TO_DECIMAL(W), (
 W - ?COL_DECIMAL_START
)).


