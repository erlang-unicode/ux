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

%%% @doc UCA.
%%%
%%% == Additional information (and links) ==
%%%
%%% 1. [http://www.open-std.org/jtc1/sc22/wg20/docs/n1037-Hangul%20Collation%20Requirements.htm
%%%     Hangul Collation Requirements]
%%% PS: There is the main source of information.
%%%
%%% 2. [http://code.activestate.com/lists/perl-unicode/2163/ 
%%%     Terminator weight for Hangul]
%%%
%%% 3. [http://blogs.msdn.com/b/michkap/archive/2005/02/25/380266.aspx 
%%%     Theory vs. practice for Korean text collation]
%%% PS: there is no any practice. They do not the UCA :/
%%%
%%% 4. [http://en.wikipedia.org/wiki/Unicode_collation_algorithm Wiki]
%%%
%%% 6. [http://useless-factor.blogspot.com/2007/08/unicode-implementers-guide-part-3.html
%%%     Unicode implementer's guide part 3: Conjoining jamo behavior]
%%%
%%% 7. [http://useless-factor.blogspot.com/2007/10/unicode-implementers-guide-part-5.html
%%%     Unicode implementer's guide part 5: Collation]
%%%
%%% 8. [http://useless-factor.blogspot.com/2008/05/unicode-collation-works-now.html
%%%     Unicode collation works now]
%%% PS: I found it so late. :(
%%%
%%% 9. [http://userguide.icu-project.org/collation/concepts ICU]
%%%
%%% 10. [http://trapexit.org/String_Sorting_%28Natural%29
%%%      String Sorting (Natural) in Erlang Cookbook]
%%% ```
%%% FIXED: Combining character contractions. Apparently, two combining marks can 
%%%        form a contraction. A straight reading of the UCA wouldn't predict 
%%%        this, but not all of the UCA tests pass unless you check for 
%%%        non-adjacent combining marks being in a contraction together, without 
%%%        a noncombining mark to start it off.
%%% '''
%%% @end

-module(ux_col).
-author('Uvarov Michael <freeakk@gmail.com>').

-export([non_ignorable/2,
        blanked/2,
        shifted/2,
        shift_trimmed/2,
        compare/2, compare/3,
        sort_array/1, sort_array/2,
        sort_array_non_ignorable/1,
        sort_array_blanked/1,
        sort_array_shifted/1,
        sort_array_shift_trimmed/1,
        sort_key/1, sort_key/2,
        sort/1, sort/2,
        ducet/1,
        get_options/0, get_options/1, get_options/2
        ]).

-include("ux_string.hrl").
-include("ux_unidata.hrl").
-include("ux_char.hrl").
-include("ux_col.hrl").

% ducet_r(reversed_in) -> non_reversed_key;
ducet_r(V) -> ux_unidata:ducet_r(V).
ccc(V) -> ux_unidata:ccc(V).


%% @doc In:  not reversed string.
%%      Out: not reversed weight list.
%% @end
ducet(A) -> ducet_r(lists:reverse(A)).

get_options() -> #uca_options{ 
        ducet_r_fn = fun ducet_r/1 
    }.

get_options(non_ignorable) ->
    #uca_options { 
        ducet_r_fn = fun ducet_r/1,

        natural_sort = false,
        strength = 3,
        alternate = non_ignorable
    };
get_options(blanked) ->
    #uca_options { 
        ducet_r_fn = fun ducet_r/1,

        natural_sort = false,
        strength = 3,
        alternate = blanked
    };
get_options(shifted) ->
    #uca_options { 
        ducet_r_fn = fun ducet_r/1,

        natural_sort = false,
        strength = 4,
        alternate = shifted
    };
get_options(shift_trimmed) ->
    #uca_options { 
        ducet_r_fn = fun ducet_r/1,

        natural_sort = false,
        strength = 4,
        alternate = shift_trimmed 
    };
get_options([_|_] = Params) ->
    get_options(Params, get_options()).

%% @doc If you want use this library without import *.hrl, you can create 
%% a #uca_options {} record with this function.
%% @end
get_options([{hangul_terminator, Val}|T], Opt = #uca_options{ }) ->
    get_options(T, Opt#uca_options{ hangul_terminator=Val });
get_options([{natural_sort, Val}|T], Opt = #uca_options{ }) ->
    get_options(T, Opt#uca_options{ natural_sort=Val });
get_options([{case_sensitive, Val}|T], Opt = #uca_options{ }) ->
    get_options(T, Opt#uca_options{ case_sensitive=Val });
get_options([{case_first, Val}|T], Opt = #uca_options{ }) ->
    get_options(T, Opt#uca_options{ case_first=Val });
get_options([{strength, Val}|T], Opt = #uca_options{ }) ->
    get_options(T, Opt#uca_options{ strength=Val });
get_options([{alternate, Val}|T], Opt = #uca_options{ }) ->
    get_options(T, Opt#uca_options{ alternate=Val });
get_options([{ducet_r_fn, Val}|T], Opt = #uca_options{ }) 
    when is_function(Val) ->
    get_options(T, Opt#uca_options{ ducet_r_fn=Val });
get_options([{ducet_r, Val}|T], Opt = #uca_options{ }) 
    when is_function(Val) ->
    get_options(T, Opt#uca_options{ 
        ducet_r_fn=fun(A) -> 
            Val(lists:reverse(A))
            end });
get_options([], Opt = #uca_options{ }) ->
    Opt.
    

%     %  %%%%%     %
%     % %     %   % %
%     % %        %   %
%     % %       %     %
%     % %       %%%%%%%
%     % %     % %     %
 %%%%%   %%%%%  %     %


%% ----------------------------------------------------------------------------
%% UNICODE COLLATION ALGORITHM
%% see Unicode Technical Standard #10

% For hangul:
% http://www.open-std.org/Jtc1/sc22/wg20/docs/n1037-Hangul%20Collation%20Requirements.htm
% http://www.unicode.org/reports/tr10/#Hangul_Collation
% http://en.wikipedia.org/wiki/KSX1001
-spec non_ignorable(string(), string()) -> less | greater | equal.

% Levels: http://unicode.org/reports/tr10/#Multi_Level_Comparison
% L1 Base characters
% L2 Accents
% L3 Case
% L4 Punctuation

%% @doc Variable collation elements are not reset to be ignorable, but
%% get the weights explicitly mentioned in the file.
%% ```
%% * SPACE would have the value [.0209.0020.0002]
%% * Capital A would be unchanged, with the value [.06D9.0020.0008]
%% * Ignorables are unchanged.'''
%% @end
non_ignorable(S1, S2) ->
    compare(S1, S2,
        get_options(non_ignorable),
        fun ducet_r/1, % ducet_r(reversed_in) -> non_reversed_key;
        fun non_ignorable_bin_to_list/1).

%% @doc Variable collation elements and any subsequent ignorables 
%% are reset so that their weights at levels one through three are zero. 
%% For example,
%% ```
%% * SPACE would have the value [.0000.0000.0000]
%% * A combining grave accent after a space would have the value [.0000.0000.0000]
%% * Capital A would be unchanged, with the value [.06D9.0020.0008]
%% * A combining grave accent after a Capital A would be unchanged'''
%% @end
blanked(S1, S2) ->
    compare(S1, S2,
        get_options(blanked),
        fun ducet_r/1, % ducet_r(reversed_in) -> non_reversed_key;
        fun blanked_bin_to_list/1).

%% @doc Variable collation elements are reset to zero at levels one through
%% three. In addition, a new fourth-level weight is appended, whose value 
%% depends on the type, as shown in Table 12.
%% Any subsequent primary or secondary ignorables following a variable are reset
%% so that their weights at levels one through four are zero.
%% ```
%% * A combining grave accent after a space would have the value 
%%   [.0000.0000.0000.0000].
%% * A combining grave accent after a Capital A would be unchanged.'''
%% @end
shifted(S1, S2) ->
    compare(S1, S2,
        get_options(shifted),
        fun ducet_r/1, % ducet_r(reversed_in) -> non_reversed_key;
        fun shifted_bin_to_list/1).

%% @doc This option is the same as Shifted, except that all trailing 
%% FFFFs are trimmed from the sort key. 
%% This could be used to emulate POSIX behavior.
%% @end
shift_trimmed(S1, S2) ->
    compare(S1, S2,
        get_options(shift_trimmed),
        fun ducet_r/1, % ducet_r(reversed_in) -> non_reversed_key;
        fun shift_trimmed_bin_to_list/1).

non_ignorable_bin_to_list(Value) ->
        {fun non_ignorable_bin_to_list/1, bin_to_list(Value)}.

%% @doc Convert binary from DUCET to list [L1, L2, L3, L4].
%%      A variable CE is "*" in ducet (1).
%%      A non-varialbe CE is "." in ducet (0).
%% @end
%% @private
bin_to_list(<<_Variable:8, L1:16, L2:16, L3:16, L4:16>>) ->
    [L1, L2, L3, L4];
bin_to_list(<<_Variable:8, L1:16, L2:16, L3:16, L4:24>>) ->
    [L1, L2, L3, L4];
% For hangul
bin_to_list(L1) when is_integer(L1) ->
    [L1, 0,  0,  0].


%% @private
% If it is a tertiary ignorable, then L4 = 0.
shifted_bin_to_list(<<_:8, 0:48, _/binary>>) ->
    {fun shifted_bin_to_list/1, [0, 0, 0, 0]};
% If it is a variable, then L4 = Old L1.
shifted_bin_to_list(<<1:8, L1:16, _/binary>>) ->
    {fun shifted_bin_to_list2/1, [0, 0, 0, L1]};
shifted_bin_to_list(Value) ->
    {fun shifted_bin_to_list/1, set_l4_to_value(Value, 16#FFFF)}.


%% @doc This function is a version of shifted_bin_to_list/1, but its value is
%%      after variable.
%% @end
%% @private
% If it is a ignorable, then L4 = 0.
shifted_bin_to_list2(<<_:8, 0:16, _/binary>>) ->
    {fun shifted_bin_to_list2/1, [0, 0, 0, 0]};
% If it is a variable, then L4 = Old L1.
shifted_bin_to_list2(<<1:8, L1:16, _/binary>>) ->
    {fun shifted_bin_to_list2/1, [0, 0, 0, L1]};
shifted_bin_to_list2(Value) ->
    {fun shifted_bin_to_list/1, set_l4_to_value(Value, 16#FFFF)}.

%% @private
%% Alternate=Shifted, Strength=L3
blanked_bin_to_list(<<1:8, _/binary>>) ->
    {fun blanked_bin_to_list2/1, [0, 0, 0]};
blanked_bin_to_list(Value) ->
    {fun blanked_bin_to_list/1, bin_to_list(Value)}.

%% @private
blanked_bin_to_list2(<<_:8, 0:16, _/binary>>) ->
    {fun blanked_bin_to_list2/1, [0, 0, 0]};
blanked_bin_to_list2(<<1:8, _/binary>>) ->
    {fun blanked_bin_to_list2/1, [0, 0, 0]};
blanked_bin_to_list2(Value) ->
    {fun blanked_bin_to_list/1, bin_to_list(Value)}.


%% @private
% If it is a tertiary ignorable, then L4 = 0.
shift_trimmed_bin_to_list(<<_:8, 0:48, _/binary>>) ->
    {fun shift_trimmed_bin_to_list/1, [0, 0, 0, 0]};
% If it is a variable, then L4 = Old L1.
shift_trimmed_bin_to_list(<<1:8, L1:16, _/binary>>) ->
    {fun shift_trimmed_bin_to_list2/1, [0, 0, 0, L1]};
shift_trimmed_bin_to_list(Value) ->
    {fun shift_trimmed_bin_to_list/1, set_l4_to_value(Value, 0)}.

%% @doc This function is a version of shifted_bin_to_list/1, but its value is
%%      after variable.
%% @end
%% @private
% If it is a ignorable, then L4 = 0.
shift_trimmed_bin_to_list2(<<_:8, 0:16, _/binary>>) ->
    {fun shift_trimmed_bin_to_list2/1, [0, 0, 0, 0]};
% If it is a variable, then L4 = Old L1.
shift_trimmed_bin_to_list2(<<1:8, L1:16, _/binary>>) ->
    {fun shift_trimmed_bin_to_list2/1, [0, 0, 0, L1]};
shift_trimmed_bin_to_list2(Value) ->
    {fun shift_trimmed_bin_to_list/1, set_l4_to_value(Value, 0)}.


%% @private
%% Return: [_, _, _, _]
set_l4_to_value(<<_Variable:8, L1:16, L2:16, L3:16, _:16>>, Val) ->
    [L1, L2, L3, Val];
set_l4_to_value(<<_Variable:8, L1:16, L2:16, L3:16, _:24>>, Val) ->
    [L1, L2, L3, Val];
% For hangul
set_l4_to_value(L1, Val) when is_integer(L1) ->
    [L1, 0,  0, Val].

%% @private
%% Returns: <<Bin>>
set_l1_to_value_bin(<<Variable:8, _L1:16, T/binary>>, Val) ->
    <<Variable, Val:16, T/binary>>.

sort(Lists) ->
    sort(Lists, #uca_options{}).

%% @doc Sort a string list.
%% Example:
%%  ```f().
%%  RawData = ["death", "de luge", "de-luge", "deluge", "de-luge", "de Luge", "de-Luge", "deLuge", "de-Luge", "demark"].
%%  Data = lists:map(fun lists:flatten/1, RawData).
%%  ux_string:sort(Data, non_ignorable).
%%  ux_string:sort(Data, blanked).
%%  ux_string:sort(Data, shifted).
%%  ux_string:sort(Data, shift_trimmed).'''
%% @end
sort(Lists, Alt) when is_atom(Alt) ->
    Params = get_options(Alt),
    Fn = get_comp_fn(Alt),
    lists:map(fun({_SortKey, Str}) -> Str end, 
        lists:keysort(1, 
            sort_map(Lists, Params, Fn, [])));

sort(Lists, #uca_options{alternate=Alt} = Params) ->
    Fn = get_comp_fn(Alt),
    lists:map(fun({_SortKey, Str}) -> Str end, 
        lists:keysort(1, 
            sort_map(Lists, Params, Fn, []))).

%% @doc Lists: an array of strings;
%% Fn:    an map function.
%%
%% This function does:
%% `lists:map(fun(X) -> sort_key(X, Fn) end, Lists).'
%% H is string.
%% Fn is col_function.
%% Params is #uca_options{}
%% @end
%% @private
sort_map([H|T], Params = #uca_options{ducet_r_fn=DucetRFn}, Fn, Res) ->
    sort_map(T, Params, Fn, [
        {sort_array_to_key(
            sort_array(H, Params, DucetRFn, Fn)), H}|Res]);
sort_map([], _Params, _Fn, Res) ->
    lists:reverse(Res).

%% @private
-spec get_sort_fn(atom()) -> function().

get_sort_fn(non_ignorable) ->
    fun sort_array_non_ignorable/1;
get_sort_fn(blanked) ->
    fun sort_array_blanked/1;
get_sort_fn(shifted) ->
    fun sort_array_shifted/1;
get_sort_fn(shift_trimmed) ->
    fun sort_array_shift_trimmed/1.

-spec get_comp_fn(atom()) -> function().

get_comp_fn(non_ignorable) ->
    fun non_ignorable_bin_to_list/1;
get_comp_fn(blanked) ->
    fun blanked_bin_to_list/1;
get_comp_fn(shifted) ->
    fun shifted_bin_to_list/1;
get_comp_fn(shift_trimmed) ->
    fun shift_trimmed_bin_to_list/1.

sort_key(Str) ->
    sort_key(Str, #uca_options{ducet_r_fn=fun ducet_r/1}).

% Return key as binary
sort_key(Str, #uca_options{
        sort_key_format=binary, 
        alternate=Alt, 
        ducet_r_fn=DucetRFn} = Params) ->
    Fn = get_comp_fn(Alt),
    convert_key_to_bin(
        sort_array_to_key(
            sort_array(Str, Params, DucetRFn, Fn)));
% Return key as a list
sort_key(Str, #uca_options{
        sort_key_format=list,
        alternate=Alt, 
        ducet_r_fn=DucetRFn} = Params) ->
    Fn = get_comp_fn(Alt),
    sort_array_to_key(
        sort_array(Str, Params, DucetRFn, Fn));
% Return key as an uncompressed list
sort_key(Str, #uca_options{
        sort_key_format=uncompressed,
        alternate=Alt, 
        ducet_r_fn=DucetRFn} = Params) ->
    Fn = get_comp_fn(Alt),
    sort_array_to_uncompressed_key(
        sort_array(Str, Params, DucetRFn, Fn));
% For testing. Second parameter is a comp_fun.
sort_key(Str, Fn) when is_function(Fn) ->
    Array = apply(Fn, [Str]),
    {_Level, Res} = sort_key1(Array, 1, [], []),
    lists:reverse(Res);
% Pass only comp_fn id.
sort_key(Str, FnName) when is_atom(FnName) ->
    Fn = get_sort_fn(FnName), 
    sort_key(Str, Fn).

%% Convert a sort array to a sort key.
sort_array_to_key(Array) ->
    {Level, Res} = sort_key1(Array, 1, [], []),
    compress_sort_key_r(Res, Level, []).

sort_array_to_uncompressed_key(Array) ->
    {_Level, Res} = sort_key1(Array, 1, [], []),
    lists:reverse(Res).
 

%% @private
%% @param AT is Array Tail.
%% @param Level is max level of key (default 1).
%% @return {MaxLevel, ReversedSortKey}
-spec sort_key1(list(), integer(), list(), list()) -> tuple().

sort_key1([[0]|AT], Level, Acc, Res) ->
    sort_key1(AT, Level, Acc, Res);
sort_key1([[H]|AT], Level, Acc, Res) ->
    sort_key1(AT, Level, Acc, [H|Res]);
sort_key1([[0|T]|AT], Level, Acc, Res) ->
    sort_key1(AT, Level, [T|Acc], Res);
sort_key1([[H|T]|AT], Level, Acc, Res) ->
    sort_key1(AT, Level, [T|Acc], [H|Res]);
sort_key1([[]|AT], Level, Acc, Res) ->
    sort_key1(AT, Level, Acc, Res);
sort_key1([] = _InArray, Level, [_|_] = Acc, Res) ->
    sort_key1(lists:reverse(Acc), Level + 1, [], [0|Res]);
sort_key1([] = _InArray, Level, [] = _Acc, Res) -> {Level, Res}.


-define(COL_LEVEL2_CAPACITY, 500).
-define(COL_LEVEL2_MIN, 1).
-define(COL_LEVEL2_MAX, 450).
-define(COL_LEVEL2_COMMON, 32).
-define(COL_LEVEL2_BOUND, 50).

-define(COL_LEVEL3_CAPACITY, 16#FF).
-define(COL_LEVEL3_MIN, 2).
-define(COL_LEVEL3_MAX, 16#1F).
-define(COL_LEVEL3_COMMON, 2).
-define(COL_LEVEL3_BOUND, 60).

-define(COL_LEVEL4_CAPACITY, 16#FFFFFF).
-define(COL_LEVEL4_MIN, 1).
-define(COL_LEVEL4_MAX, 16#1FFFFF).
-define(COL_LEVEL4_COMMON, 16#FFFF).
-define(COL_LEVEL4_BOUND, 16#100F0).

% Reassign the weights in the collation element table at level n to create
% a gap of size GAP above COMMON. Typically for secondaries or tertiaries 
% this is done after the values have been reduced to a byte range by the 
% above methods. Here is a mapping that moves weights up or down to create 
% a gap in a byte range.
% w -> w + 01 - MIN, for MIN <= w < COMMON
% w -> w + FF - MAX, for COMMON < w <= MAX
-define(REASSIGN_WEIGHT(W, Min, Max, Common, Capacity),
    (if
    ((W) >= (Min)) and ((W) < (Common)) ->
        (W) + 1 - (Min);
    (W > Common) and ((W) =< (Max)) ->
        (W) + Capacity - (Max);
    true -> Common
    end)).
-define(REASSIGN_WEIGHT2(W),
    ?REASSIGN_WEIGHT(W, ?COL_LEVEL2_MIN, ?COL_LEVEL2_MAX, ?COL_LEVEL2_COMMON, 
        ?COL_LEVEL2_CAPACITY)).
-define(REASSIGN_WEIGHT3(W),
    ?REASSIGN_WEIGHT(W, ?COL_LEVEL3_MIN, ?COL_LEVEL3_MAX, ?COL_LEVEL3_COMMON,
        ?COL_LEVEL3_CAPACITY)).
-define(REASSIGN_WEIGHT4(W),
    ?REASSIGN_WEIGHT(W, ?COL_LEVEL4_MIN, ?COL_LEVEL4_MAX, ?COL_LEVEL4_COMMON,
        ?COL_LEVEL4_CAPACITY)).

-define(COL_LEVEL2_MINTOP, 31).
-define(COL_LEVEL2_MAXBOTTOM, (?COL_LEVEL2_CAPACITY - ?COL_LEVEL2_MIN
    - (?COL_LEVEL2_MAX - ?COL_LEVEL2_COMMON + 1))).
-define(COL_LEVEL2_GAP_SIZE, (?COL_LEVEL2_CAPACITY - ?COL_LEVEL2_MAX 
   -  ?COL_LEVEL2_MIN)).

-define(COL_LEVEL3_MINTOP, 1).
-define(COL_LEVEL3_MAXBOTTOM, (?COL_LEVEL3_CAPACITY - ?COL_LEVEL3_MIN
    - (?COL_LEVEL3_MAX - ?COL_LEVEL3_COMMON + 1))).
-define(COL_LEVEL3_GAP_SIZE, (?COL_LEVEL3_CAPACITY - ?COL_LEVEL3_MAX
   -  ?COL_LEVEL3_MIN)).

-define(COL_LEVEL4_MINTOP, 1).
-define(COL_LEVEL4_MAXBOTTOM, (?COL_LEVEL4_CAPACITY - ?COL_LEVEL4_MIN
    - (?COL_LEVEL4_MAX - ?COL_LEVEL4_COMMON + 1))).
-define(COL_LEVEL4_GAP_SIZE, (?COL_LEVEL4_CAPACITY - ?COL_LEVEL4_MAX
   -  ?COL_LEVEL4_MIN)).

%% @doc Get reversed sort key and compress it.
%% @param Key
%% @param Level (1-4). For example: 3 then 2 then 1 (because Key is reversed!)
%% @param Res Compressed key
-spec compress_sort_key_r(list(), integer(), list()) -> list().

compress_sort_key_r([0|T], Level, Res) ->
    compress_sort_key_r(T, Level - 1, [0|Res]);
% Replace H=2 on Level=3
compress_sort_key_r([?COL_LEVEL3_COMMON|T], 3, Res) ->
    compress_sort_key_l3(T, 1, Res);
compress_sort_key_r([H|T], Level = 3, Res) ->
    compress_sort_key_r(T, Level, [?REASSIGN_WEIGHT3(H)|Res]);

compress_sort_key_r([?COL_LEVEL2_COMMON|T], 2, Res) ->
    compress_sort_key_l2(T, 1, Res);
compress_sort_key_r([H|T], Level = 2, Res) ->
    compress_sort_key_r(T, Level, [?REASSIGN_WEIGHT2(H)|Res]);

compress_sort_key_r([?COL_LEVEL4_COMMON|T], 2, Res) ->
    compress_sort_key_l4(T, 1, Res);
compress_sort_key_r([H|T], Level = 4, Res) ->
    compress_sort_key_r(T, Level, [?REASSIGN_WEIGHT4(H)|Res]);

compress_sort_key_r([H|T], Level, Res) ->
    compress_sort_key_r(T, Level, [H|Res]);
compress_sort_key_r([], _Level, Res) -> Res.


%% @doc Read all W from Key.
%% ```If W < COMMON (or there is no W), replace the sequence by a synthetic low
%% weight equal to (MINTOP + m).
%% If W > COMMON, replace the sequence by a synthetic high weight equal to
%% (MAXBOTTOM - m).'''
%%
%% An input key must be reversed!
%% @end
%% @private
-spec compress_sort_key_l3(list(), integer(), list()) -> list().

compress_sort_key_l3([?COL_LEVEL3_COMMON|T], M, Res) ->
    compress_sort_key_l3(T, M + 1, Res);
compress_sort_key_l3(T, M, [W|_] = Res) 
    when (W > ?COL_LEVEL3_MAXBOTTOM) ->
    SynHighWeight = ?COL_LEVEL3_MAXBOTTOM - M,
    if
% If a synthetic high weight would be less than BOUND, use a 
% sequence of high weights of the form (BOUND)..(BOUND)(MAXBOTTOM - 
% remainder).
        SynHighWeight < ?COL_LEVEL3_BOUND ->
            HighGapSize = ?COL_LEVEL3_MAXBOTTOM - ?COL_LEVEL3_BOUND - 1,
            SeqCnt    = M div HighGapSize,
            Remainder = M rem HighGapSize,
            
            compress_sort_key_r(T, 3, 
                compress_seq(SeqCnt, ?COL_LEVEL3_BOUND,
                    [(?COL_LEVEL3_MAXBOTTOM - Remainder)|Res]));
        true -> compress_sort_key_r(T, 3, [SynHighWeight|Res])
    end;
compress_sort_key_l3(T, M, Res) ->
    SynLowWeight = ?COL_LEVEL3_MINTOP + M,
    if
        SynLowWeight < ?COL_LEVEL3_BOUND ->
            compress_sort_key_r(T, 3, [SynLowWeight|Res]);

% If a synthetic low weight would not be less than BOUND, use a sequence 
% of low weights of the form (BOUND-1)..(BOUND-1)(MINTOP + remainder) to 
% express the length of the sequence.
        true ->
            LowGapSize = ?COL_LEVEL3_BOUND - ?COL_LEVEL3_MINTOP - 2,
            SeqCnt    = M div LowGapSize,
            Remainder = M rem LowGapSize,
            
            compress_sort_key_r(T, 3, 
                compress_seq(SeqCnt, ?COL_LEVEL3_BOUND - 1,
                    [(?COL_LEVEL3_MINTOP + Remainder)|Res])) 
    end.
    

%% Read all W from Key.
%% If W < COMMON (or there is no W), replace the sequence by a synthetic low
%% weight equal to (MINTOP + m).
%% If W > COMMON, replace the sequence by a synthetic high weight equal to
%% (MAXBOTTOM - m).
-spec compress_sort_key_l2(list(), integer(), list()) -> list().

compress_sort_key_l2([?COL_LEVEL2_COMMON|T], M, Res) ->
    compress_sort_key_l2(T, M + 1, Res);
compress_sort_key_l2(T, M, [W|_] = Res) 
    when (W > ?COL_LEVEL2_MAXBOTTOM) ->
    SynHighWeight = ?COL_LEVEL2_MAXBOTTOM - M,
    if
% If a synthetic high weight would be less than BOUND, use a 
% sequence of high weights of the form (BOUND)..(BOUND)(MAXBOTTOM - 
% remainder).
        SynHighWeight < ?COL_LEVEL2_BOUND ->
            HighGapSize = ?COL_LEVEL2_MAXBOTTOM - ?COL_LEVEL2_BOUND - 1,
            SeqCnt    = M div HighGapSize,
            Remainder = M rem HighGapSize,
            
            compress_sort_key_r(T, 2, 
                compress_seq(SeqCnt, ?COL_LEVEL2_BOUND,
                    [(?COL_LEVEL2_MAXBOTTOM - Remainder)|Res]));
        true -> compress_sort_key_r(T, 2, [SynHighWeight|Res])
    end;
compress_sort_key_l2(T, M, Res) ->
    SynLowWeight = ?COL_LEVEL2_MINTOP + M,
    if
        SynLowWeight < ?COL_LEVEL2_BOUND ->
            compress_sort_key_r(T, 2, [SynLowWeight|Res]);

% If a synthetic low weight would not be less than BOUND, use a sequence 
% of low weights of the form (BOUND-1)..(BOUND-1)(MINTOP + remainder) to 
% express the length of the sequence.
        true ->
            LowGapSize = ?COL_LEVEL2_BOUND - ?COL_LEVEL2_MINTOP - 2,
            SeqCnt    = M div LowGapSize,
            Remainder = M rem LowGapSize,
            
            compress_sort_key_r(T, 2, 
                compress_seq(SeqCnt, ?COL_LEVEL2_BOUND - 1,
                    [(?COL_LEVEL2_MINTOP + Remainder)|Res])) 
    end.
    

%% Read all W from Key.
%% If W < COMMON (or there is no W), replace the sequence by a synthetic low
%% weight equal to (MINTOP + m).
%% If W > COMMON, replace the sequence by a synthetic high weight equal to
%% (MAXBOTTOM - m).
-spec compress_sort_key_l4(list(), integer(), list()) -> list().

compress_sort_key_l4([?COL_LEVEL4_COMMON|T], M, Res) ->
    compress_sort_key_l4(T, M + 1, Res);
compress_sort_key_l4(T, M, [W|_] = Res) 
    when (W > ?COL_LEVEL4_MAXBOTTOM) ->
    SynHighWeight = ?COL_LEVEL4_MAXBOTTOM - M,
    if
% If a synthetic high weight would be less than BOUND, use a 
% sequence of high weights of the form (BOUND)..(BOUND)(MAXBOTTOM - 
% remainder).
        SynHighWeight < ?COL_LEVEL4_BOUND ->
            HighGapSize = ?COL_LEVEL4_MAXBOTTOM - ?COL_LEVEL4_BOUND - 1,
            SeqCnt    = M div HighGapSize,
            Remainder = M rem HighGapSize,
            
            compress_sort_key_r(T, 4, 
                compress_seq(SeqCnt, ?COL_LEVEL4_BOUND,
                    [(?COL_LEVEL4_MAXBOTTOM - Remainder)|Res]));
        true -> compress_sort_key_r(T, 4, [SynHighWeight|Res])
    end;
compress_sort_key_l4(T, M, Res) ->
    SynLowWeight = ?COL_LEVEL4_MINTOP + M,
    if
        SynLowWeight < ?COL_LEVEL4_BOUND ->
            compress_sort_key_r(T, 4, [SynLowWeight|Res]);

% If a synthetic low weight would not be less than BOUND, use a sequence 
% of low weights of the form (BOUND-1)..(BOUND-1)(MINTOP + remainder) to 
% express the length of the sequence.
        true ->
            LowGapSize = ?COL_LEVEL4_BOUND - ?COL_LEVEL4_MINTOP - 2,
            SeqCnt    = M div LowGapSize,
            Remainder = M rem LowGapSize,
            
            compress_sort_key_r(T, 4, 
                compress_seq(SeqCnt, ?COL_LEVEL4_BOUND - 1,
                    [(?COL_LEVEL4_MINTOP + Remainder)|Res])) 
    end.
    
%% @see compress_sort_key_l3/3
%% @see compress_sort_key_l2/3
%% @doc Add Val to the beginning Cnt times.
%% @private
-spec compress_seq(integer(), integer(), list()) -> list().

compress_seq(1, Val, Res) ->
    [Val|Res];
compress_seq(SeqCnt, Val, Res) when SeqCnt > 1 ->
    compress_seq(SeqCnt - 1, Val, [Val|Res]).

convert_key_to_bin(Key) when is_list(Key) ->
    convert_key_to_bin(Key, 1, []).

%% @doc Key is a list.
%%      Level (default 1).
%% @end
-spec convert_key_to_bin(list(), integer(), list()) -> binary().

convert_key_to_bin([0|T], Level, Res) ->
    convert_key_to_bin(T, Level + 1, [0|[0|Res]]);
convert_key_to_bin([H|T], 2, Res) when H < 255 ->
    convert_key_to_bin(T, 2, [H|Res]);
convert_key_to_bin([H|T], 3, Res) when H < 255 ->
    convert_key_to_bin(T, 3, [H|Res]);
convert_key_to_bin([H|T], 2, Res) when H > 254 ->
    convert_key_to_bin([(H - 255)|T], 2, [255|Res]);
convert_key_to_bin([H|T], 3, Res) when H > 254 ->
    convert_key_to_bin([(H - 255)|T], 3, [255|Res]);
convert_key_to_bin([H|T], Level, Res) when H =< 16#FFFF ->
    convert_key_to_bin(T, Level, [(H rem 256) |[(H bsr 8) |Res]]);
convert_key_to_bin([H|T], Level, Res) 
    when (H > 16#FFFF) and (H =< 16#1FFFFF) ->
    convert_key_to_bin(T, Level, 
        [(H rem 256) 
            |[((H bsr 8) rem 256)
                |[(H bsr 16)|Res]]]);
convert_key_to_bin([], _Level, Res) ->
    erlang:list_to_binary(lists:reverse(Res)).
    

% http://unicode.org/reports/tr10/#Variable_Weighting
sort_array(Str) when is_list(Str) -> 
    sort_array(Str, #uca_options {ducet_r_fn = fun ducet_r/1}, 
        fun ducet_r/1, fun bin_to_bin/1).

sort_array(Str, Params = #uca_options{alternate=Alt, ducet_r_fn=DucetRFn}) 
    when is_list(Str) -> 
    sort_array(Str, Params, DucetRFn, get_comp_fn(Alt)).

sort_array_non_ignorable(Str) when is_list(Str) -> 
    sort_array(Str, get_options(non_ignorable), 
        fun ducet_r/1, fun non_ignorable_bin_to_list/1).

sort_array_blanked(Str) when is_list(Str) -> 
    sort_array(Str, get_options(blanked), 
        fun ducet_r/1, fun blanked_bin_to_list/1).

sort_array_shifted(Str) when is_list(Str) -> 
    sort_array(Str, get_options(shifted), 
        fun ducet_r/1, fun shifted_bin_to_list/1).

sort_array_shift_trimmed(Str) when is_list(Str) -> 
    sort_array(Str, get_options(shift_trimmed), 
        fun ducet_r/1, fun shift_trimmed_bin_to_list/1).

%% @doc This function does nothing. :)
%% @private
bin_to_bin(Val) ->
    { fun bin_to_bin/1, Val }.

-spec compare(list(), list()) -> lower | upper | equal.

compare(String1, String2) when is_list(String1), is_list(String2) ->
    Params = #uca_options{ducet_r_fn=fun ducet_r/1},
    #uca_options{ alternate=Alt } = Params, 
    compare(String1, String2, Params, fun ducet_r/1, get_comp_fn(Alt)).
compare(String1, String2, #uca_options{ 
        alternate=Alt, ducet_r_fn=DucetRFn} = Params) ->
    compare(String1, String2, Params, DucetRFn, get_comp_fn(Alt)).
    
%% @doc Compare 2 strings.
%% TableFun returns value from DUCET table.
%% ComparatorFun http://unicode.org/reports/tr10/#Variable%20Weighting
%% @end
-spec compare(list(), list(), record(), function(), function()) 
    -> lower | upper | equal.
compare(String1, String2, #uca_options{} = Params, TableFun, ComparatorFun) 
    when is_function(TableFun), is_function(ComparatorFun),
    is_list(String1), is_list(String2) ->
    compare1(ux_string:to_nfd(String1), 
        ux_string:to_nfd(String2),
        Params, 
        [], % Buf 1, contains ducet(Char)
        [], % Buf 2
        false, % CompValue 1
        [], % Accumulator for String 1 
            % saves values for next levels comparation
        [], % Accumulator for String 2
        TableFun, % fun ux_string:ducet/1, in chars are REVERSED.
        ComparatorFun, ComparatorFun).

%% @doc MANUAL:
%% S2.1   Find the longest initial substring S at each point 
%%        that has a match in the table.
%% S2.1.1 If there are any non-starters following S, process each non-starter C.
%% S2.1.2 If C is not blocked from S, find if S + C has a match in the table.
%% S2.1.3 If there is a match, replace S by S + C, and remove C.
%%
%% Returns:  {Not reversed list of weight elements, Tail of the string}.
%% @end
%% @private
-spec extract(string(), #uca_options{}, fun()) 
    -> {[[integer(), ...], ...], Tail :: string()}.
extract(Str, #uca_options { 
        hangul_terminator=Term, 
        natural_sort=DecFlag,
        case_sensitive=CaseSenFlag,
        case_first=CaseFirst
    } = _Params, TableFun) ->
    Res = extract0(Str, TableFun),
    {Weights, StrTail} = Res,
    {Weights2, StrTail2} = 
        case mod_weights_proxy(Weights, DecFlag, Term, [], 
            StrTail, TableFun) of
        false -> Res;
        Res1  -> Res1 
        end,

    Weights3 = case CaseFirst of
        off   -> Weights2;
        lower -> Weights2;
        upper -> case_first_hack(Weights2)
    end,

    Weights4 = case CaseSenFlag of
        false -> Weights3;
        true  -> case_sensitive_hack(Weights3)
        end,
    
    {Weights4, StrTail2}.

%% @doc Uppercase to sort before lowercase. Remap L3.
%% @private
case_first_hack(Res) ->
    case_first_hack1(Res, []).

%% @private
case_first_hack1([<<Var:8, L1L2:32, L3:16, L4/binary>> | In], Out) ->
    NewL3 = case_invert(L3),
    case_first_hack1(In, [<<Var:8, L1L2:32, NewL3:16, L4/binary>> | Out]);
case_first_hack1([] = _In, Out) -> lists:reverse(Out).

%% @private
case_invert(L3) when L3 >= 2 andalso L3 =< 6 ->
    L3 + 6;
case_invert(L3) when L3 >= 8 andalso L3 =< 12 ->
    L3 - 6;
case_invert(L3) ->
    L3.


%% @doc Copy L3 before L1.
%% @private
case_sensitive_hack(Res) ->
    case_sensitive_hack1(Res, []).

%% @private
% Skip primary ignorable element. 
% L1 ~ (L1 ++ L3)
case_sensitive_hack1([<<Var:8, L1:16, L2:16, L3:16, L4/binary>> | In], Out) ->
    case_sensitive_hack1(In, [<<Var:8, L1:16, L2:16, 0:16, L4/binary>> | 
        [<<Var:8, L3:16, 0:48>> | Out]]);
case_sensitive_hack1([] = _In, Out) -> lists:reverse(Out).

%% @private
mod_weights_proxy(Weights, DecFlag, Term, Acc, StrTail, TableFun) ->
    case mod_weights(Weights, DecFlag, Term, Acc, StrTail, TableFun) of
    % There is no any hangul jamo L chars in this string 
    % (or other char with a weight of jamo L char)
    {next, {Weights2, Acc2, StrTail2}} -> 
        mod_weights_proxy(Weights2, DecFlag, Term, Acc2, StrTail2, TableFun);
    Res2 -> Res2
    end.
        

% 7.1.5 Hangul Collation
% Interleaving Method
% MANUAL:
% Generate a modified weight table:
% 1. Assign a weight to each precomposed Hangul syllable character, 
%    with a 1-weight gap between each one. 
%    (See Section 6.2, Large Weight Values)
% 2. Give each jamo a 1-byte internal weight. 
%    Also add an internal terminator 1-byte weight (W). 
%    These are assigned so that al W < T <  V < L.
%    These weights are separate from the default weights, and are just used 
%    internally.
% When any string of jamo and/or Hangul syllables is encountered, 
% break it into syllables according to the rules of Section 3.12, 
% Conjoining Jamo Behavior of [Unicode]. 
% Process each syllable separately:
% If a syllable is canonically equivalent to one of the precomposed Hangul 
% syllables, then just assign the weight as above
% If not, then find the greatest syllable that it is greater than; 
% call that the base syllable. Generate a weight sequence corresponding to
% the following gap weight, followed by all the jamo weight bytes, 
% followed by the terminator byte.
%

% L1 as an argument is first hangul jamo L.
% L1 as an part of ?IS_L1_OF_HANGUL_L is first level.
%% @private
% Hack for Hangul.
mod_weights([<<_:8, L1:16, _/binary>> = H | T], 
    _DecFlag, Term, Acc, StrTail, TableFun) 
    when ?IS_L1_OF_HANGUL_L(L1) ->
    hangul2(l, T, [H|Acc], StrTail, TableFun, Term);
% Hack for numbers.
mod_weights([<<_:8, L1:16, _/binary>> = H | T], 
    true = _DecFlag, _Term, Acc, StrTail, TableFun) 
    when ?IS_L1_OF_DECIMAL(L1) ->
    decimal2(?COL_WEIGHT_TO_DECIMAL(L1), T, 
        [set_l1_to_value_bin(H, 1)|Acc], StrTail, TableFun);
mod_weights([H|T], DecFlag, Term, Acc, StrTail, TableFun) ->
    mod_weights(T, DecFlag, Term, [H|Acc], StrTail, TableFun);
mod_weights([], _DecFlag, _Term, _Acc, _StrTail, _TableFun) ->
    false. % L1 is not found. There is no hangul jamo in this string.

decimal2(Dec, [<<_:8, 00:16, _/binary>> = H | T], Acc, StrTail, TableFun) ->
    decimal2(Dec, T, [H|Acc], StrTail, TableFun); % skip an ignorable element.
decimal2(Dec, [<<_:8, L1:16, _/binary>> = H | T], Acc, StrTail, TableFun) 
    when ?IS_L1_OF_DECIMAL(L1) -> % L2 is found. LL*
    decimal2((Dec * 10) + ?COL_WEIGHT_TO_DECIMAL(L1), T, 
        [set_l1_to_value_bin(H, 0)|Acc], StrTail, TableFun); 
decimal2(Dec,  [], Acc, [_|_] = StrTail, TableFun) -> 
    {Weights, StrTail2} = extract0(StrTail, TableFun), % We need more gold.
    decimal2(Dec, Weights, Acc, StrTail2, TableFun);
decimal2(Dec, T, Acc, StrTail, _TableFun) -> % L
    {lists:reverse(lists:reverse(T, decimal_result(Dec, Acc))), StrTail}.


decimal_result(Dec, Res) ->
    case Dec div 16#FFFF of
    0 -> [1|[Dec|Res]];
    Div -> decimal_result(Div, [Dec rem 16#FFFF|[16#FFFF|Res]])
    end.

%% L1 was found. 
%% Mod: l
%% @private
hangul2(Mod, [<<_:8, 0:16, _/binary>> = H|T], Acc, StrTail, TableFun, Term) ->
   % skip an ignorable element.
   hangul2(Mod, T, [H|Acc], StrTail, TableFun, Term); 
hangul2(l,  [<<_:8, L1:16, _/binary>> = H|T], Acc, StrTail, TableFun, Term) 
    when ?IS_L1_OF_HANGUL_L(L1) -> % L2 is found. LL*
    hangul2(ll, T, [H|Acc], StrTail, TableFun, Term); 
hangul2(l,  [<<_:8, L1:16, _/binary>> = H|T], Acc, StrTail, TableFun, Term) 
    when ?IS_L1_OF_HANGUL_V(L1) -> % V1 is found. LV*
    hangul2(lv, T, [H|Acc], StrTail, TableFun, Term); 
hangul2(lv, [<<_:8, L1:16, _/binary>> = H|T], Acc, StrTail, _TableFun, Term) 
    when ?IS_L1_OF_HANGUL_T(L1) -> % T1 is found. LVT
    hangul_result(T, [H|Acc], StrTail, Term); 
hangul2(lv, [<<_:8, L1:16, _/binary>> = H|T], Acc, StrTail, _TableFun, Term) 
    when ?IS_L1_OF_HANGUL_V(L1) -> % V2 is found. LVV
    hangul_result(T, [H|Acc], StrTail, Term); 
hangul2(ll, [<<_:8, L1:16, _/binary>> = H|T], Acc, StrTail, _TableFun, Term) 
    when ?IS_L1_OF_HANGUL_V(L1) -> % V1 is found. LLV
    hangul_result(T, [H|Acc], StrTail, Term); 
%hangul2(lv, T, Acc, StrTail, _TableFun) 
%    % Skip and try to found other L. LVX
%    -> hangul_result(T, Acc, StrTail); 
hangul2(_Mod, [H|T], Acc, StrTail, _TableFun, _Term) ->
    % Skip and try to found other L. LX
    {next, {T, [H|Acc], StrTail}};
hangul2(Mod,  [   ], Acc, [_|_] = StrTail, TableFun, Term) -> 
    {Weights, StrTail2} = extract0(StrTail, TableFun), % We need more gold.
    hangul2(Mod, Weights, Acc, StrTail2, TableFun, Term);
hangul2(_Mod, [   ], _Acc, [] = _StrTail, _TableFun, _Term) -> % L
    false.

%% @private
hangul_result(T, Acc, StrTail, Term) ->
    {lists:reverse(lists:reverse(T, [Term|Acc])), StrTail}.


%% @private
extract0([], _) -> % No Any Char
    {[], []};

% Table 18. Values for Base
% -----------------------------------------------------------------------------
% Range 1: Unified_Ideograph=True AND
% ((Block=CJK_Unified_Ideograph) OR (Block=CJK_Compatibility_Ideographs))
% Base  1: FB40
% Range 2: Unified_Ideograph=True AND NOT
% ((Block=CJK_Unified_Ideograph) OR (Block=CJK_Compatibility_Ideographs))
% Base  2: FB80
% Base  3: FBC0 Any other code point
% Range 3: Ideographic AND NOT Unified_Ideograph
% -----------------------------------------------------------------------------
 extract0([CP|Tail], _)  
    when ?CHAR_IS_UNIFIED_IDEOGRAPH(CP) 
     and (?CHAR_IS_CJK_COMPATIBILITY_IDEOGRAPH(CP) 
       or ?CHAR_IS_CJK_UNIFIED_IDEOGRAPH(CP)) ->
     {implicit_weight(CP, 16#FB40), Tail};
    
 extract0([CP|Tail], _)  
    when ?CHAR_IS_UNIFIED_IDEOGRAPH(CP) 
      and (not (?CHAR_IS_CJK_COMPATIBILITY_IDEOGRAPH(CP) 
             or ?CHAR_IS_CJK_UNIFIED_IDEOGRAPH(CP))) ->
    {implicit_weight(CP, 16#FB80), Tail};

% If TableFun = only_derived then don't use ducet, 
% try only ideographs and hangul characters.
% This function runs when ducet() return 'other'. 
extract0([CP|Tail], { only_derived, TableFun }) ->
    case TableFun([CP]) of
    [_|_] = Value -> % from ducet 
        {Value, Tail};
    _ -> % other, more 
        {implicit_weight(CP, 16#FBC0), Tail}
    end;

% Try extract from ducet.
extract0([CP|[]], TableFun) -> % Last Char
    case TableFun([CP]) of
    [_|_] = Value ->
        {Value, []};
    {set_ignorables_to_0, Value} ->
        {Value, []};
    _ ->
        {[], []}
    end;    
extract0([CP | Tail] = _Str, TableFun) ->
    extract1(Tail, TableFun, [CP], 
    false, % Max ccc among ccces of skipped chars beetween the starter char 
           % and the processed char. If there are no skipped chars, then 
           % Ccc1=false.
    [], false).


% Note: A non-starter in a string is called blocked if there is another 
%       non-starter of the same canonical combining class or zero between 
%       it and the last character of canonical combining class 0.

% There is only one char which was compared.
% TableFun(CPlist) is always return right weight.
%% @private
extract1([], TableFun, [Ch], _, Skipped, false ) ->
    extract0(
        [Ch|lists:reverse(Skipped)], 
        {only_derived, TableFun}); 
% see BUG 7
extract1([], _, _, _, _, more) ->
    more_error;
% ... One or more chars
extract1([], _,  _,  _, Skipped, OldVal) ->
    {OldVal, lists:reverse(Skipped)}; % Return result

% TIP: OldVal = apply(TableFun, [CPList])
extract1([CP2|Tail] = Str, TableFun, CPList, Ccc1, Skipped, OldVal) ->
    Ccc2  = ccc(CP2),
%   W2CP2 = apply(TableFun, [[CP2]]),
    if
        (Ccc1 =/= 0) and  % Ccc1 == 0     => Last skipped char was blocked.
        ((Ccc1 == false)  % Ccc1 == false => There is no skipped chars.
        or (Ccc1 < Ccc2)) % Ccc1 == Ccc2  => Last skipped char was blocked.
            -> % Last skipped char was non-blocked.
            NewCPList = [CP2|CPList],

            % Try extract weight from ducat. There is one place, where we can 
            % extract. We only get old value from ducat in other places.
            Bin = TableFun(NewCPList),
            case Bin of
            % Bin(CPList) == other, but Bin(CPList+NextChar) may be 
            % have a specified collation weight.
            more ->
                case extract1(Tail, TableFun, NewCPList, 
                    Ccc1, Skipped, more) of
                more_error -> % Cannot add any next char.
                    extract1(Tail, TableFun, CPList, Ccc2, 
                        [CP2|Skipped], OldVal); % skip CP2
                MoreRes -> MoreRes
                end;
            % Cannot extract collation element.
            other when (more == OldVal) -> more_error;
            other when (more =/= OldVal) ->
                extract1(Tail, TableFun, CPList, Ccc2, 
                    [CP2|Skipped], OldVal); % skip CP2

            % Append char CP2. Try find more characters.
            % Ccc1 == false, because this is a first step of algorithm.
            % (don't save canonical class of previous character (ccc1))
            [_|_] when (false == Ccc1) -> 
            extract1(Tail, TableFun, NewCPList, false, 
                Skipped, Bin);
            
            % Append char CP2. Try find more characters.
            % Ccc1 =/= false, because this is a second step of algorithm.
            [_|_] when (false =/= Ccc1) -> 
            extract1(Tail, TableFun, NewCPList, Ccc2, 
                Skipped, Bin)
            end; % if

        % Last skipped char was blocked.
        (Ccc1 == Ccc2) and (0 =/= Ccc1) ->
            extract1(Tail, TableFun, CPList, Ccc2, 
                [CP2|Skipped], OldVal); % skip CP2

        OldVal ==  more  -> more_error;
        OldVal ==  false -> % and (CPList == [_]) 
            % http://unicode.org/reports/tr10/#Unassigned_And_Other
            extract0(
                lists:reverse(CPList, 
                    lists:reverse(Skipped, Str)), 
                {only_derived, TableFun}); % Or *timed out*
        OldVal =/= false -> {OldVal, lists:reverse(Skipped, Str)}
    end.



%% @doc 7.1.3 Implicit Weights 
%% The result of this process consists of collation elements that are sorted in
%% code point order, that do not collide with any explicit values in the table,
%% and that can be placed anywhere (for example, at BASE) with respect to the 
%% explicit collation element mappings. By default, implicit mappings are given
%% higher weights than all explicit collation elements.
%% @end
%% @private
implicit_weight(CP, BASE) ->
    AAAA = BASE + (CP bsr 15),
    BBBB = (CP band 16#7FFF) bor 16#8000,
    [<<0:8, AAAA:16, 16#0020:16, 0002:16, 0:16>>, BBBB]. % reversed


%% @doc Compares on L1, collects data for {L2,L3,L4} comparations.
%% Extract chars from the strings.
%%
%% ComparatorFun    S2.3 Process collation elements according to the 
%%                  variable-weight setting, as described in Section 
%%                  3.6.2, Variable Weighting.
%%
%% ```ALGORITHM.
%% 1. Extract weights from Str1 to Buf1.
%% 2. Extract weights from Str2 to Buf2.
%% 3. Extract L1 weight from Buf1 to W1L1.
%% 4. Exctaxt L1 weight from Buf1 and compare with W1L1.
%% 5a. If W1L1 > W2L1 then Str1 greater Str2.
%% 5b. If W1L1 < W2L1 then Str1 lower   Str2.
%% 5c. If W1L1 = W2L1 and strings have non-compared characters then go to a step 1.
%% 6. Run compare2.'''
%% @end
%% @private
-spec compare1(Str1 :: string(), Str1 :: string(), #uca_options{},
        Buf1 :: [binary(), ...], Buf2 :: [binary()], char(), 
        Acc1 :: [[integer(), ...], ...], 
        Acc2 :: [[integer()]], % Acc = [[L2,L3,L4], ...] 
        fun(), fun(), fun()) -> lower | greater | equal.

compare1([_|_] = Str1, StrTail2, Params, [], Buf2, W1L1, Acc1, 
    Acc2, TableFun, ComparatorFun1, ComparatorFun2) ->
    {Buf1,     % [<<Flag,L1,L2,...>>, ..]
     StrTail1} = extract(Str1, Params, TableFun), 
%   io:format("B1: ~w ~n", [Buf1]),
    compare1(StrTail1, StrTail2, Params, Buf1, Buf2, W1L1, Acc1,
        Acc2, TableFun, ComparatorFun1, ComparatorFun2);
compare1(StrTail1, [_|_] = Str2, Params, Buf1, [], W1L1, Acc1, 
    Acc2, TableFun, ComparatorFun1, ComparatorFun2) ->
    {Buf2,     % [<<Flag,L1,L2,...>>, ..]
     StrTail2} = extract(Str2, Params, TableFun), 
%   io:format("B2: ~w ~n", [Buf2]),
    compare1(StrTail1, StrTail2, Params, Buf1, Buf2, W1L1, Acc1, 
        Acc2, TableFun, ComparatorFun1, ComparatorFun2);
    
% Extracts a non-ignorable L1 from the Str1.
compare1(StrTail1, StrTail2, #uca_options{
        strength=S
    } = Params, [CV1Raw|Buf1], Buf2, false, Acc1, 
    Acc2, TableFun, ComparatorFun1, ComparatorFun2) ->
    % This function can reverse argument or set 0.
    {NewFun, Val} = ComparatorFun1(CV1Raw),
    case weight_strength(S, Val) of 
    [0|Acc] -> % Find other W1L1
        compare1(StrTail1, StrTail2, Params, Buf1, Buf2, false, [Acc|Acc1], 
            Acc2, TableFun, NewFun, ComparatorFun2);
    [W1L1|Acc] -> % W1L1 was found. Try find W2L1.
        compare1(StrTail1, StrTail2, Params, Buf1, Buf2, W1L1,  [Acc|Acc1],
            Acc2, TableFun, NewFun, ComparatorFun2)
    end;

compare1(StrTail1, StrTail2, #uca_options{
        strength=S
    } = Params, Buf1, [CV2Raw|Buf2], W1L1, Acc1, 
    Acc2, TableFun, ComparatorFun1, ComparatorFun2) ->
    {NewFun, Val} = ComparatorFun2(CV2Raw),
    case weight_strength(S, Val) of 
    [0|Acc] -> % Find other W2L1
        compare1(StrTail1, StrTail2, Params, Buf1, Buf2, W1L1, Acc1, 
            [Acc|Acc2], TableFun, ComparatorFun1, NewFun);
    [_|_] when W1L1 == true -> 
        lower;   % Sting 1 was ended; 
                 % string 2 still has a non-ignorable char 
                 % => string2 greater.
    [W2L1|_] when W1L1 > W2L1 ->
        greater; % Return result: S1 greater S2 on L1
    [W2L1|_] when W1L1 < W2L1 ->
        lower;   % Return result: S1 lower S2 on L1
    [W2L1|Acc] when W1L1 == W2L1 ->
        compare1(StrTail1, StrTail2, Params, Buf1, Buf2, false, Acc1, 
            [Acc|Acc2], TableFun, ComparatorFun1, NewFun)
    end;

% MANUAL:
% This guarantees that when two strings of unequal length are compared, 
% where the shorter string is a prefix of the longer string, the longer 
% string is always sorted after the shorter in the absence of special
% features like contractions. For example: "abc" < "abcX" where "X" can
% be any character(s).

% String 1 conrains more codepaints, but we cannot throw them.
compare1([CP1|StrTail1], [] = _Str2, Params, [] = _Buf1, [] = _Buf2, _, Acc1, 
    Acc2, TableFun, ComparatorFun1, ComparatorFun2) ->
    compare1(StrTail1, [], Params, CP1, [], false, Acc1, 
        Acc2, TableFun, ComparatorFun1, ComparatorFun2);

% String 2 conrains more codepaints, but we cannot throw them.
compare1([] = _Str1, [CP2|StrTail2], Params, [] = _Buf1, [] = _Buf2, _, Acc1, 
    Acc2, TableFun, ComparatorFun1, ComparatorFun2) ->
    compare1([], StrTail2, Params, [], CP2, true, Acc1, 
        Acc2, TableFun, ComparatorFun1, ComparatorFun2);

compare1(_Str1, [] = _Str2, _Params, _Buf1, [] = _Buf2, W1L1, _Acc1, 
    _Acc2, _TableFun, _ComparatorFun1, _ComparatorFun2) 
    when (W1L1 > 0) and (W1L1 =/= false) ->
    greater;

compare1([] = _Str1, [] = _Str2, #uca_options{
        strength=S
    } = Params, [W1Raw|Buf1], [] = _Buf2, W1L1, Acc1, 
    Acc2, TableFun, ComparatorFun1, ComparatorFun2) when W1L1 == 0 ->
    {NewFun, Val} = ComparatorFun1(W1Raw),
    [W1L1New|Acc] = weight_strength(S, Val),
    compare1([], [], Params, Buf1, [], W1L1New, [Acc|Acc1], 
        Acc2, TableFun, NewFun, ComparatorFun2);

% L1 was ended :(
% Now, Funs are not neeaded.
% Acc1 and Acc2 are reversed.
compare1([] = _Str1, [] = _Str2, _Params, [] = _Buf1, [] = _Buf2, false, Acc1, 
    Acc2, _TableFun, _ComparatorFun1, _ComparatorFun2) -> 
%   io:format(user, "~w ~w ~n", [Acc1, Acc2]),
    compare2(lists:reverse(Acc1), 
        lists:reverse(Acc2),
        false, % W1L{2,3,4} 
        [], % Other accumulator. Contains L3 and L4.
        []  % Other acc...
    ).


%% @doc L2 comparation.
%% Try extract W1LX, but 0 was found => try next weigth in InAcc.
%% @end
%% @private
% skip if higher levels are not defined.
compare2([[]|InAcc1], InAcc2, W1LX, OutAcc1, OutAcc2) ->
    compare2(InAcc1, InAcc2, W1LX, OutAcc1, OutAcc2);
compare2(InAcc1, [[]|InAcc2], W1LX, OutAcc1, OutAcc2) ->
    compare2(InAcc1, InAcc2, W1LX, OutAcc1, OutAcc2);

compare2([[0|OutAcc]|InAccTail1], InAcc2, false, OutAcc1, OutAcc2) ->
    compare2(InAccTail1, InAcc2, false, [OutAcc|OutAcc1], OutAcc2);
% W1LX was found. => Try found W2LX.
compare2([[W1LX|OutAcc]|InAccTail1], InAcc2, false, OutAcc1, OutAcc2) ->
    compare2(InAccTail1, InAcc2, W1LX, [OutAcc|OutAcc1], OutAcc2);

% Try extract W2LX.
compare2(InAcc1, [[0|OutAcc]|InAccTail2], W1LX, OutAcc1, OutAcc2) ->
    compare2(InAcc1, InAccTail2, W1LX, OutAcc1, [OutAcc|OutAcc2]);
    
compare2(_, [[W2LX|_]|_], W1LX, _, _) 
    when W1LX < W2LX -> lower;
compare2(_, [[W2LX|_]|_], W1LX, _, _) 
    when W1LX > W2LX -> greater;
compare2(InAcc1, [[W2LX|OutAcc]|InAccTail2], W1LX, OutAcc1, OutAcc2) 
    when W1LX == W2LX ->
    compare2(InAcc1, InAccTail2, false, OutAcc1, [OutAcc|OutAcc2]);

% Try extract from Str1, which is empty.
compare2([], [W2LX|_], false, _, _) when W2LX>0 ->
    lower;
compare2([], [W2LX|Acc], false, Acc1, Acc2) when W2LX == 0 ->
    compare2([], Acc, false, Acc1, [Acc|Acc2]);

% Str2 was ended.
compare2(_, [], W1LX, _, _) when W1LX =/= false ->
    greater;

% Try compare on next composition level. 
compare2([], [], false, [_|_] = OutAcc1, [_|_] = OutAcc2) ->
    compare2(lists:reverse(OutAcc1), lists:reverse(OutAcc2), false, [], []);

% End compares
compare2([], [], false, [], []) ->
    equal. % on all levels

%% @doc Produce Sort Array
%% http://unicode.org/reports/tr10/#Step_2
%% @end
%% @private
sort_array(Str, Params, TableFun, CompFun) ->
    sort_array1(ux_string:to_nfd(Str), Params, TableFun, CompFun, [], []).

sort_array1([], _Params, _TableFun, _CompFun, [], Res) ->
    lists:reverse(Res);
sort_array1(Str, #uca_options{strength=S} = P, TableFun, CompFun, [H|T], Res) ->
    {NewCompFun, Val} = CompFun(H),
    Val2 = weight_strength(S, Val),
    sort_array1(Str, P, TableFun, NewCompFun, T, [Val2|Res]);
sort_array1([_|_] = Str, P, TableFun, CompFun, [], Res) ->
    {Buf, StrTail} = extract(Str, P, TableFun), 
    sort_array1(StrTail, P, TableFun, CompFun, Buf, 
%       [test|Res]).
        Res).

weight_strength(1, [L1|_Tail]) ->
    [L1];
weight_strength(2, [L1,L2|_Tail]) ->
    [L1, L2];
weight_strength(3, [L1,L2,L3|_Tail]) ->
    [L1, L2, L3];
weight_strength(4, [L1,L2,L3,L4]) ->
    [L1, L2, L3, L4];
weight_strength(_, Val) ->
    Val.

% Collation end


%------------------------------------------------------------------------------
  %%%%%  %%%%%%   %%%%    %%%%%   %%%%
    %    %       %          %    %
    %    %%%%%    %%%%      %     %%%%
    %    %            %     %         %
    %    %       %    %     %    %    %
    %    %%%%%%   %%%%      %     %%%%


-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).

compress_info() ->
    F = fun io_lib:format/2,
    io:format(user, "Information: ~n~s ~n", [lists:flatten([
        F(" ?COL_LEVEL2_MIN = ~w ~n", [?COL_LEVEL2_MIN]),
        F(" ?COL_LEVEL2_MAX = ~w ~n", [?COL_LEVEL2_MAX]),
        F(" ?COL_LEVEL2_GAP_SIZE = ~w ~n", [?COL_LEVEL2_GAP_SIZE]),
        F(" ?COL_LEVEL2_MINTOP = ~w ~n", [?COL_LEVEL2_MINTOP]),
        F(" ?COL_LEVEL2_MAXBOTTOM = ~w ~n", [?COL_LEVEL2_MAXBOTTOM]),

        F(" ?COL_LEVEL3_MIN = ~w ~n", [?COL_LEVEL3_MIN]),
        F(" ?COL_LEVEL3_MAX = ~w ~n", [?COL_LEVEL3_MAX]),
        F(" ?COL_LEVEL3_GAP_SIZE = ~w ~n", [?COL_LEVEL3_GAP_SIZE]),
        F(" ?COL_LEVEL3_MINTOP = ~w ~n", [?COL_LEVEL3_MINTOP]),
        F(" ?COL_LEVEL3_MAXBOTTOM = ~w ~n", [?COL_LEVEL3_MAXBOTTOM])
    ])]).


-define(_assertLower(X,Y), ({
    lists:flatten(
        io_lib:format("~20s < ~20s", lists:map(
            fun(A) ->
                lists:flatten(
                    io_lib:format("~w", [A])) end, 
            [X, Y]))),
    case (X) < (Y) of
    true ->  ?_assertEqual(1,1);
    false -> ?_assert(X < Y)
    end})).

compress_show_info_test_() ->
    {setup, fun compress_info/0, fun ux_utils:noop/1, ?_test(?_assert(true))}.

compress_sort_key_test_() ->
    [{"Check constants for L3", 
        ?_assertEqual(?COL_LEVEL3_MINTOP + ?COL_LEVEL3_GAP_SIZE,
            ?COL_LEVEL3_MAXBOTTOM)},
     {"Check constants for L2", 
        ?_assertEqual(?COL_LEVEL2_MINTOP + ?COL_LEVEL2_GAP_SIZE,
            ?COL_LEVEL2_MAXBOTTOM)}].

compress_sort_key3_test_() ->
    L = lists:duplicate(2000, ?COL_LEVEL3_COMMON),
    FF = fun(A) -> compress_sort_key_r(lists:reverse(A), 3, []) end,
    [?_assertLower(FF([4]), FF([8])) 
    ,?_assertLower(FF([2]), FF([8])) 
    ,?_assertLower(FF([2,2]), FF([2,8])) 
    ,?_assertLower(FF([2,2]), FF([2,2,2])) 
    ,?_assertLower(FF([2,2]), FF([2,16#1F])) 
    ,?_assertLower(FF(L ++ [2]), FF(L ++ [8])) 
    ].

compress_sort_key2_test_() ->
    L = lists:duplicate(2000, ?COL_LEVEL2_COMMON),
    FF = fun(A) -> compress_sort_key_r(lists:reverse(A), 2, []) end,
    [?_assertLower(FF([32]), FF([34])) 
    ,?_assertLower(FF([18]), FF([32])) 
    ,?_assertLower(FF([32,32]), FF([32,34])) 
    ,?_assertLower(FF([32,32]), FF([32,40])) 
    ,?_assertLower(FF([32,32]), FF([32,32,32])) 
    ,?_assertLower(FF([32,32, 32]), FF([32,32,34])) 
    ,?_assertLower(FF([32]), FF([34])) 
    ,?_assertLower(FF(L ++ [18]), FF(L ++ [32])) 
    ,?_assertLower(FF(L ++ [32]), FF(L ++ [34])) 
    ,?_assertLower(FF(L), FF(L ++ [32])) 
    ,?_assertLower(FF(L), FF(L ++ [34])) 
    ].

sort_key_test_() ->
    M = 'ux_col',
    F = 'sort_key',
    FF = fun ux_utils:noop/1,
    [?_assertEqual(M:F([[1,2,3], [4,5,6], [0,7], [8,9]], FF), 
        [1,4,8,0,2,5,7,9,0,3,6])
    ].

append_test_() ->
    F = fun lists:reverse/2,
    [?_assertEqual(F("ABC", "DEF"), "CBADEF")
    ,?_assertEqual(F("123", F("ABC", "DEF")), "321CBADEF")
    ].

shifted_equal_test_() ->
    F = fun shifted/2,
    [?_assertEqual(F([10973,98], [10972,98]), F([10972,98], [10973,98]))
    ].

natural_sort_test_() ->
    [{"Using official test strings from Dave Koelle",
       ?_assertEqual(["10X Radonius",
         "20X Radonius",
         "20X Radonius Prime",
         "30X Radonius",
         "40X Radonius",
         "200X Radonius",
         "1000X Radonius Maximus",
         "Allegia 50 Clasteron",
         "Allegia 51 Clasteron",
         "Allegia 51B Clasteron",
         "Allegia 52 Clasteron",
         "Allegia 60 Clasteron",
         "Allegia 500 Clasteron",
         "Alpha 2",
         "Alpha 2A",
         "Alpha 2A-900",
         "Alpha 2A-8000",
         "Alpha 100",
         "Alpha 200",
         "Callisto Morphamax",
         "Callisto Morphamax 500",
         "Callisto Morphamax 600",
         "Callisto Morphamax 700",
         "Callisto Morphamax 5000",
         "Callisto Morphamax 7000",
         "Callisto Morphamax 7000 SE",
         "Callisto Morphamax 7000 SE2",
         "QRS-60 Intrinsia Machine",
         "QRS-60F Intrinsia Machine",
         "QRS-62 Intrinsia Machine",
         "QRS-62F Intrinsia Machine",
         "Xiph Xlater 5",
         "Xiph Xlater 40",
         "Xiph Xlater 50",
         "Xiph Xlater 58",
         "Xiph Xlater 300",
         "Xiph Xlater 500",
         "Xiph Xlater 2000",
         "Xiph Xlater 5000",
         "Xiph Xlater 10000"],
        sort(["1000X Radonius Maximus",
                "10X Radonius",
                "200X Radonius",
                "20X Radonius",
                "20X Radonius Prime",
                "30X Radonius",
                "40X Radonius",
                "Allegia 50 Clasteron",
                "Allegia 500 Clasteron",
                "Allegia 51 Clasteron",
                "Allegia 51B Clasteron",
                "Allegia 52 Clasteron",
                "Allegia 60 Clasteron",
                "Alpha 100",
                "Alpha 2",
                "Alpha 200",
                "Alpha 2A",
                "Alpha 2A-8000",
                "Alpha 2A-900",
                "Callisto Morphamax",
                "Callisto Morphamax 500",
                "Callisto Morphamax 5000",
                "Callisto Morphamax 600",
                "Callisto Morphamax 700",
                "Callisto Morphamax 7000",
                "Callisto Morphamax 7000 SE",
                "Callisto Morphamax 7000 SE2",
                "QRS-60 Intrinsia Machine",
                "QRS-60F Intrinsia Machine",
                "QRS-62 Intrinsia Machine",
                "QRS-62F Intrinsia Machine",
                "Xiph Xlater 10000",
                "Xiph Xlater 2000",
                "Xiph Xlater 300",
                "Xiph Xlater 40",
                "Xiph Xlater 5",
                "Xiph Xlater 50",
                "Xiph Xlater 500",
                "Xiph Xlater 5000",
                "Xiph Xlater 58"], 
        get_options([{natural_sort, true}, {alternate, non_ignorable}])))}].

%---------------------------------------------------------------
% SLOW TESTS 
%---------------------------------------------------------------

%% Collation Test
%% Parse data files from 
%% http://www.unicode.org/Public/UCA/latest/
%% README: http://www.unicode.org/Public/UCA/latest/CollationTest.html
test(_InFd, _F, _OldVal, StrNum, 0 = _Max, Res) ->
    io:format(user, "Only ~w strings were tested. Exit.~n", [StrNum]),
    Res; % max
% Read first string with data from file.
test(InFd, P, false, StrNum, Max, Res) ->
    OldVal = test_read(InFd, StrNum),
    test(InFd, P, OldVal, StrNum, Max, Res);
test(InFd, Params, {OldFullStr, OldVal, StrNum}, _OldStrNum, Max, Res) ->
    % Add new string.
    case StrNum of
    100 -> io:format(user, "~n", []);
    _   -> boo
    end,
    % Show message
    case StrNum rem 10000 of
    0 -> io:format(user, "~w strings were tested. ~n", [StrNum]);
    _ -> boo
    end,

    case test_read(InFd, StrNum) of
    {FullStr, Val, NewStrNum} = Result when is_list(Val) -> 
        % 1. check compare/3.
        Res2 = case compare(Val, OldVal, Params) of % collation compare
            % error
            lower -> io:format(user, "Error (compare): ~w ~w ~w ~n", 
                    [Val, lower, OldVal]),
                io:format(user,
                    " Data1: ~ts Data2: ~ts",
                    [OldFullStr, FullStr]),
                    error;
            % OK (equal or upper). 
            _ -> Res
            end,

        % 2. check sort_key/2.
        Key1 = sort_key(OldVal, Params),
        Key2 = sort_key(Val, Params),
        Res3 = if
                Key1 > Key2 ->
                io:format(user, "Error (key): ~w ~w ~w ~n", 
                    [Val, lower, OldVal]),
                io:format(user,
                    " Data1: ~ts Data2: ~ts",
                    [OldFullStr, FullStr]),
                io:format(user,
                    " Key1: ~w ~n Key2: ~w~n",
                    [Key1, Key2]),
                
                Arr1 = sort_array(OldVal, Params),
                Arr2 = sort_array(Val, Params),
                io:format(user,
                    " Arr1: ~w ~n Arr2: ~w~n",
                    [Arr1, Arr2]),
                    error;
                true -> Res2
            end,
            
        test(InFd, Params, Result, NewStrNum, Max - 1, Res3);
    _ -> ok
    end.

%% @doc Read line from a testdata file InFd (see CollationTest.html).
%% Return list of codepaints.
%% Used by test/4.
%% @end
%% @private
test_read(InFd, StrNum) ->
    case io:get_line(InFd, "") of
    eof -> ok;
    {error,Mess} -> throw({error, "Error while reading file", Mess});
    Data -> 
        try % parse Data
            [Value|_] = ux_string:split(["#", ";", "\n"], Data), 
            %% Converts "0009 0021" to [16#0009, 16#0021]
            lists:map(fun ux_string:hex_to_int/1, 
                      string:tokens(Value, " "))
        of Res -> {Data, Res, StrNum + 1} % {FullStr, Codepaints}
        catch                   
        error:_Reason -> 
%            io:format(user, "~w: Data=~w ~n", [Reason, Data]),
            test_read(InFd, StrNum + 1)
        end
    end.

prof(File, Params, Count) ->
    {ok, InFd} = file:open(File, [read]),
%    io:setopts(InFd,[{encoding,utf8}]),
    Res = test(InFd, Params, false, 0, Count, ok),
    ?assertEqual(Res, ok).


non_ignorable_test_() ->
    {timeout, 600, 
        fun() -> 
            prof(
               ux_unidata:get_ucadata_dir() ++ "CollationTest/" 
                    % Slow, with comments.
%                  ++ "CollationTest_NON_IGNORABLE.txt", 
                    % Fast version (data from slow version are equal).
                    ++ "CollationTest_NON_IGNORABLE_SHORT.txt", 
                get_options(non_ignorable), 
                1000000) 
        end}.

shifted_test_() ->
    {timeout, 600, 
        fun() -> 
            prof(
               ux_unidata:get_ucadata_dir() ++ "CollationTest/" 
                    % Slow, with comments.
%                   ++ "CollationTest_SHIFTED.txt", 
                    ++ "CollationTest_SHIFTED_SHORT.txt", 
                get_options(shifted), 
                1000000) end}.


nat_prof(Seq) ->
    Lists = [io_lib:format("Abr~w", [X]) || X <- Seq],
    Params = #uca_options{alternate=non_ignorable, natural_sort=true},
    {Time, SortedLists} = timer:tc(?MODULE, sort, [Lists, Params]),
    io:format(user, "~n Sort Time, ~.3gs ", [Time / 1000000]),
    ?_assertEqual(Lists, SortedLists).

natural_sort_long_test_() ->
    {timeout, 600,
        fun() ->
            nat_prof(lists:seq(1, 10000, 1)),
            nat_prof(lists:seq(1, 10000000, 1000)),
            nat_prof(lists:seq(1, 100000000000000, 9999999999)),
            io:format(user, "~n", [])
        end}.
-endif.
