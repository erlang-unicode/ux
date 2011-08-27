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

-type uca_weight() :: integer().
-type uca_elem() :: [atom()|uca_weight()].
-type uca_array() :: [uca_elem()].
-type result() :: {[uca_elem()], string()}.
-type uca_weights() :: [uca_weight()].

-define(COL_HANGUL_TERMINATOR, 13000).

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


