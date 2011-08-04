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

%%% @doc Functions for extraction UNIDATA.
%%% @private


-module(ux_unidata).
-author('Uvarov Michael <freeakk@gmail.com>').
-export([get_source_file/1,
        get_unidata_dir/0, get_ucadata_dir/0]).
-export([char_to_upper/1, char_to_lower/1, is_upper/1, is_lower/1,
        char_comment/1, char_type/1, ccc/1, 
        nfc_qc/1, nfd_qc/1, nfkc_qc/1, nfkd_qc/1, 
        is_comp_excl/1, is_compat/1, decomp/1, comp/2,
        ducet_r/1, char_block/1]).
-include("ux_unidata.hrl").

-spec get_source_file(Parser::atom()) -> string().
get_source_file(allkeys) ->
    code:priv_dir(ux) ++ "/UNIDATA/allkeys.txt";
get_source_file(blocks) ->
    code:priv_dir(ux) ++ "/UNIDATA/Blocks.txt";
get_source_file(comp_exclusions) ->
    code:priv_dir(ux) ++ "/UNIDATA/CompositionExclusions.txt";
get_source_file(norm_props) ->
    code:priv_dir(ux) ++ "/UNIDATA/DerivedNormalizationProps.txt";
get_source_file(unidata) ->
    code:priv_dir(ux) ++ "/UNIDATA/UnicodeData.txt".

-spec is_upper(C::char()) -> boolean().
-spec is_lower(C::char()) -> boolean().
-spec char_type(C::char()) -> atom().
-spec char_comment(C::char()) -> binary().
-spec ccc(C::char()) -> integer().
-spec nfc_qc(C::char()) -> y | n | m.
-spec nfd_qc(C::char()) -> y | n | m.
-spec nfkc_qc(C::char()) -> y | n | m.
-spec nfkd_qc(C::char()) -> y | n | m.
-spec is_compat(C::char()) -> true | false.
-spec is_comp_excl(C::char()) -> true | false.
-spec ducet_r(list()) -> list() | atom().
-spec comp(integer(), integer()) -> integer() | false.
-spec decomp(integer()) -> list().

func(Parser, Type, Value) -> 
    F = ux_unidata_filelist:get_source(Parser, Type),
    F(Value).
char_to_upper(C) -> func(unidata, to_upper, C).
char_to_lower(C) -> func(unidata, to_lower, C).
is_upper(C) -> func(unidata, is_upper, C).
is_lower(C) -> func(unidata, is_lower, C).
char_type(C) -> func(unidata, type, C).
char_comment(C) -> func(unidata, comment, C).
ccc(C) -> func(unidata, ccc, C).
nfc_qc(C) -> func(norm_props, nfc_qc, C).
nfd_qc(C) -> func(norm_props, nfd_qc, C).
nfkc_qc(C) -> func(norm_props, nfkc_qc, C).
nfkd_qc(C) -> func(norm_props, nfkd_qc, C).
is_compat(C) -> func(unidata, is_compat, C).
is_comp_excl(C) -> func(comp_exclusions, is_exclusion, C).
ducet_r(L) -> func(allkeys, ducet_r, L).
comp(C1, C2) -> func(unidata, comp, {C1, C2}).
decomp(C) -> func(unidata, decomp, C).
char_block(C) -> func(blocks, block, C).


priv_dir() ->
    case code:priv_dir(ux) of
        [_|_] = Res -> Res;
        _ -> "../priv"
    end.
get_unidata_dir() -> priv_dir() ++ "/" ++ ?UNIDATA_VERSION ++ "/".
get_ucadata_dir() -> priv_dir() ++ "/" ++ ?UCADATA_VERSION ++ "/".

