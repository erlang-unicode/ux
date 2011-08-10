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
%%%
%%% @doc UNICODE COLLATION ALGORITHM
%%%      see Unicode Technical Standard #10
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
%%%
%%%   
%%% For hangul collation:
%%% 11. [http://www.open-std.org/Jtc1/sc22/wg20/docs/n1037-Hangul%20Collation%20Requirements.htm
%%%  Hangul Collation Requirements]
%%% 12. [http://www.unicode.org/reports/tr10/#Hangul_Collation
%%%  UTR 10]
%%% 13. [http://en.wikipedia.org/wiki/KSX1001 KSX1001 on Wiki]
%%%
%%%
%%% == Levels == 
%%% http://unicode.org/reports/tr10/#Multi_Level_Comparison
%%%
%%% * L1 Base characters
%%% * L2 Accents
%%% * L3 Case
%%% * L4 Punctuation
%%%
%%% Example using levels:
%%% ```
%%% C = ux_uca_options:get_options([{strength, 3}]).
%%% ux_uca:sort_key(C, "Get L1-L3 weights"). '''
%%%
%%%
%%% == Common configurations ==
%%% 
%%% === Non-ignorable ===
%%% Variable collation elements are not reset to be ignorable, but
%%% get the weights explicitly mentioned in the file.
%%%
%%% * SPACE would have the value [.0209.0020.0002]
%%% * Capital A would be unchanged, with the value [.06D9.0020.0008]
%%% * Ignorables are unchanged.
%%% 
%%% Example:
%%% ```
%%% C = ux_uca_options:get_options(non_ignorable).
%%% ux_uca:sort_key(C, "Non-ignorable collation sort key"). '''
%%% 
%%%
%%% === Blanked ===
%%% Variable collation elements and any subsequent ignorables 
%%% are reset so that their weights at levels one through three are zero. 
%%% For example,
%%%
%%% * SPACE would have the value [.0000.0000.0000]
%%% * A combining grave accent after a space would have the value
%%%   [.0000.0000.0000]
%%% * Capital A would be unchanged, with the value [.06D9.0020.0008]
%%% * A combining grave accent after a Capital A would be unchanged
%%%
%%% Example:
%%% ```
%%% C = ux_uca_options:get_options(non_ignorable).
%%% ux_uca:sort_key(C, "Blanked collation sort key"). '''
%%%
%%%
%%% === Shifted === 
%%% Variable collation elements are reset to zero at levels one through
%%% three. In addition, a new fourth-level weight is appended, whose value 
%%% depends on the type, as shown in Table 12.
%%% Any subsequent primary or secondary ignorables following a variable are reset
%%% so that their weights at levels one through four are zero.
%%% 
%%% * A combining grave accent after a space would have the value 
%%%   [.0000.0000.0000.0000].
%%% * A combining grave accent after a Capital A would be unchanged.
%%% 
%%% Example:
%%% ```
%%% C = ux_uca_options:get_options(shifted).
%%% ux_uca:sort_key(C, "Shifted collation sort key"). '''
%%%
%%%
%%% === Shift-trimmed === 
%%% This option is the same as Shifted, except that all trailing 
%%% FFFFs are trimmed from the sort key. 
%%% This could be used to emulate POSIX behavior.
%%%
%%% Example:
%%% ```
%%% C = ux_uca_options:get_options(shift_trimmed).
%%% ux_uca:sort_key(C, "Shift-trimmed collation sort key"). '''
%%% 
%%%
%%% @end

-module(ux_uca).
-author('Uvarov Michael <freeakk@gmail.com>').

-export([
        compare/2,      % Str1, Str2
        compare/3,      % Opt, Str1, Str2
        sort_array/1,   % Str
        sort_array/2,   % Opt, Str
        sort_key/1,     % Str
        sort_key/2,     % Opt, Str
        sort/1,         % Str
        sort/2          % Opt, [Str]
%       get_options/0, 
%       get_options/1, 
%       get_options/2
        ]).

-include("ux.hrl").
-include("ux_uca.hrl").
-include("ux_uca_common.hrl").

-type uca_compare_result() ::
      lower
    | greater
    | equal
    .

-type uca_generator() :: fun().


-spec compare(string(), string()) -> uca_compare_result().
%% @doc Compare two strings and return: lower, greater or equal.
%% @end
compare(S1, S2) ->
    C = get_options(),
    compare(C, S1, S2).

-spec compare(#uca_options{}, string(), string()) -> uca_compare_result().
compare(C=#uca_options{}, S1, S2) ->
    G1 = generator(C, S1),
    G2 = generator(C, S2),
    do_compare(G1, G2).

-spec do_compare(uca_generator(), uca_generator()) -> uca_compare_result().
do_compare(G1, G2) ->
    case {G1(), G2()} of
    {stop, stop} -> equal;
    {stop, ____} -> lower;
    {____, stop} -> upper;
    {{W1, NewG1}, {W2, NewG2}} 
        when W1 =:= W2 ->
        do_compare(NewG1, NewG2);
    {{W1, _NewG1}, {W2, _NewG2}} 
        when W1 < W2 -> lower;
    {{W1, _NewG1}, {W2, _NewG2}} 
        when W1 > W2 -> upper
    end.
    
%% @doc Convert the unicode string to the
%%      [http://unicode.org/reports/tr10/#Step_2 collation element array]
%% @end
sort_array(S) ->
    C = get_options(),
    sort_array(C, S).

sort_array(C, S) ->
    W = [],
    A = [],
    D = get_ducet(),
    do_sort_array(C, D, S, W, A).

do_sort_array(_C, _D, []=_S, []=_W, A) ->
    lists:reverse(A);
do_sort_array(C, D, S, []=_W, A) ->
    {NewW, NewS} = do_extract(C, S, D),
    do_sort_array(C, D, NewS, NewW, A);
do_sort_array(C, D, S, [WH|WT], A) ->
    do_sort_array(C, D, S, WT, [WH|A]).
    
%% @doc Convert the unicode string to the sort key.
sort_key(S) ->
    C = get_options(),
    sort_key(C, S).
    
%% @param C#sort_key_format{}
%% @param S::string()
sort_key(C=#uca_options{
        sort_key_format='binary', 
        case_sensitive=true}, S) ->
    ux_uca_sort_key_binary_cs:sort_key(C, S);
sort_key(C=#uca_options{sort_key_format=F}, S) ->
    case F of
    'binary' ->
        ux_uca_sort_key_binary:sort_key(C, S);
    'list' ->
        ux_uca_sort_key_list:sort_key(C, S);
    'uncompressed' ->
        ux_uca_sort_key_uncompressed:sort_key(C, S) 
    end.

-spec sort([string()]) -> [string()].
%% @doc Sort a list of strings.
sort(Strings) ->
    C = get_options(),
    sort(C, Strings).

-spec sort(#uca_options{}, [string()]) -> [string()].
%% @doc Sort a list of strings.
sort(C=#uca_options{}, Strings) ->
    
    % Step 1: produce array of sort keys
    F = fun(S) -> 
            Key = sort_key(C, S),
            {Key, S} 
        end,
    Keys = lists:map(F, Strings),

    % Step 2: sort array
    SortedKeys = lists:keysort(1, Keys),

    % Step 3: Return result
    RetFn = fun({_Key, S}) -> S end,
    lists:map(RetFn, SortedKeys).




%%
%% Generator
%% 

-spec generator(#uca_options{}, string()) -> uca_generator().
%generator(#uca_options{}, []) -> stop;
generator(C=#uca_options{}, S) ->
    D = get_ducet(),
    A = ux_uca_alt:get_alternate_function(C, D),
    W = [],
    R = [],

    fun() -> do_generator(C, S, D, A, W, R) end.

%% @param C::uca_options{} Configuration
%% @param S::string()   String
%% @param D::fun()      Ducet_reversed function
%% @param A::fun()      Alternate function 
%% @param W::[int()]    ListOfWeights 
%% @param R::[[int()]]  Remain weights
do_generator(#uca_options{strength=S}, []=_S, _D, _A, []=_W, R) ->
    F = fun() -> do_generator2(S-1, lists:reverse(R), []) end,
    {0, F};
do_generator(C, S, D, A, []=_W, R) ->
    {NewW, NewS} = do_extract(C, S, D),
    do_generator(C, NewS, D, A, NewW, R);
do_generator(C, S, D, A, [WH|WT], R) ->
    {NewA, Ints} = do_alt(A, WH),
    case Ints of
    [0] -> % try extract next
        do_generator(C, S, D, NewA, WT, R);
    [L1] when is_integer(L1) -> 
        F = fun() -> 
                do_generator(C, S, D, NewA, WT, R)
            end,
        {L1, F};
    [0|IT] -> % try extract next
        do_generator(C, S, D, NewA, WT, [IT|R]);
    [L1|IT] when is_integer(L1) -> 
        F = fun() -> 
                do_generator(C, S, D, NewA, WT, [IT|R])
            end,
        {L1, F}
    end.

%% @param S::integer() Strength (not string).
do_generator2(0, _W, _R) ->
    stop;
do_generator2(_S, []=_W, []=_R) ->
    stop;
do_generator2(1, []=_W, _R) ->
    stop;
do_generator2(S, []=_W, R) ->
    F = fun() -> 
            do_generator2(S-1, lists:reverse(R), []) 
        end,
    {0, F};
do_generator2(S, [[0=_WH]|WT], R) ->
    do_generator2(S, WT, R);
do_generator2(S, [[0=_WH|WR]|WT], R) ->
    do_generator2(S, WT, [WR|R]);
do_generator2(S, [[WH]|WT], R) ->
    F = fun() -> do_generator2(S, WT, R) end,
    {WH, F};
do_generator2(S, [[WH|WR]|WT], R) ->
    F = fun() -> do_generator2(S, WT, [WR|R]) end,
    {WH, F}.
    


    


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
split_levels_test_() ->
    F = fun split_levels/1,
    [?_assertEqual(F([[1,2,3],[4,5,6],[7,8,9]]), {[1,4,7], [[2,3],[5,6],[8,9]]})
    ,?_assertEqual(F([[1,2,3],[4],[7,8]]), {[1,4,7], [[2,3],[8]]})
    ].
-endif.
