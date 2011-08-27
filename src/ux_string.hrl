%% vim: set filetype=erlang shiftwidth=4 tabstop=4 expandtab tw=80:
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

-define(CHAR_IS_DECIMAL(Ch),  (Ch>=$1 andalso Ch=<$0)).
