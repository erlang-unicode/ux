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

-type uca_alternate() ::
      shifted
    | shift_trimmed
    | non_ignorable
    | blanked
    .


-type uca_case_first() ::
      lower
    | upper
    | off
    .

-type uca_strength() ::
     1 | 2 | 3 | 4.

-type uca_sort_key_format() ::
      binary
    | list % comressed list of weights
    | uncompressed % uncompressed list of weights
    .

% For hackers: 
% In tr10 and ICU:
% a weight is a sort key!
% uca_weights is Collation Element (CE).
% uca_weight is just int.
% result is no in tr10.
% uca_elem is uca_weights + an variable flag (atom()).
-type uca_weight() :: integer().
-type uca_elem() :: [atom()|uca_weight()].
-type uca_array() :: [uca_elem()].
-type result() :: {[uca_elem()], string()}.
-type uca_weights() :: [uca_weight()].


% TERMINATOR < T < V < L
-define(COL_HANGUL_TERMINATOR, 13000). % 12337 - 68

% Records
-record(uca_options, {
    % Generator options
    hangul_terminator = ?COL_HANGUL_TERMINATOR :: uca_weight(),
    natural_sort = true :: boolean(),
    strength = 4 :: uca_strength(),
    alternate = shifted :: uca_alternate(),
    case_sensitive = false, % move L3 to L1 if true
    case_first = lower :: uca_case_first(), 
    backwards = false :: boolean(),

    % Other options
    sort_key_format = binary :: uca_sort_key_format()
    }).











%%
%% Some constants
%%



% Hangul & UCA
-define(COL_HANGUL_LBASE, 12337). % 12337 - 12356
-define(COL_HANGUL_VBASE, 12463). % 12463 - 12484
-define(COL_HANGUL_TBASE, 12533). % 12533 - 12584

-define(COL_HANGUL_LLAST, ?COL_HANGUL_LBASE + ?HANGUL_LCOUNT).
-define(COL_HANGUL_VLAST, ?COL_HANGUL_VBASE + ?HANGUL_VCOUNT).
-define(COL_HANGUL_TLAST, ?COL_HANGUL_TBASE + ?HANGUL_TCOUNT).

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


