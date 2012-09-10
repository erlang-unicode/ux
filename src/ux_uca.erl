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
%%% =====================================================================

%%% =====================================================================
%%%   Copyright 2011 Uvarov Michael 
%%%
%%%   Licensed under the Apache License, Version 2.0 (the "License");
%%%   you may not use this file except in compliance with the License.
%%%   You may obtain a copy of the License at
%%%
%%%       http://www.apache.org/licenses/LICENSE-2.0
%%%
%%%   Unless required by applicable law or agreed to in writing, software
%%%   distributed under the License is distributed on an "AS IS" BASIS,
%%%   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%%   See the License for the specific language governing permissions and
%%%   limitations under the License.
%%%
%%% $Id$
%%% =====================================================================
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
-author('Uvarov Michael <arcusfelis@gmail.com>').

-export([
        compare/2,      % Str1, Str2
        compare/3,      % Opt, Str1, Str2
        sort_array/1,   % Str
        sort_array/2,   % Opt, Str
        sort_key/1,     % Str
        sort_key/2,     % Opt, Str
        sort/1,         % Str
        sort/2,         % Opt, [Str]
        search/2,
        search/3,
        search/4
%       get_options/0, 
%       get_options/1, 
%       get_options/2
        ]).

-import(ux_uca_utils, [
    do_alt/2, 
    do_alt/3, 
    do_extract/3, 
    get_ducet/0, 
    get_options/0, 
    split_levels/3, 
    get_reassign_function/2]).


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

-export_type([uca_alternate/0,
    uca_case_first/0,
    uca_strength/0,
    uca_sort_key_format/0,
    uca_weight/0,
    uca_elem/0,
    uca_array/0,
    result/0,
    uca_weights/0]).


-type uca_compare_result() ::
      lower
    | greater
    | equal
    .

-type uca_generator() :: fun().

-include("ux.hrl").
-include("uca/ux_uca.hrl").


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
    
check_const_test_() ->
    Cases = fun(_) ->
        [?_assertEqual(l1("0"), [?COL_DECIMAL_START])
        ,?_assertEqual(l1("9"), [?COL_DECIMAL_END])
        ,?_assertEqual(l1([?HANGUL_LBASE]), [?COL_HANGUL_LBASE])
        ,?_assertEqual(l1([?HANGUL_VBASE]), [?COL_HANGUL_VBASE])
        ,?_assertEqual(l1([?HANGUL_TBASE]), [?COL_HANGUL_TBASE])
        ,?_assertEqual(l1([?HANGUL_TLAST]), [?COL_HANGUL_TLAST])
        ]
        end,
    {timeout, 60, 
        {setup, fun() -> l1("0") end, Cases}}.

l1(Str) -> 
    [L1 || [_, L1|_] <- ux_unidata:ducet(Str)].

-endif.




-spec compare(string(), string()) -> uca_compare_result().
%% @doc Compare two strings and return: lower, greater or equal.
%% @end
compare(S1, S2) ->
    C = get_options(),
    compare(C, S1, S2).

-spec compare(#uca_options{}, string(), string()) -> uca_compare_result().
compare(C=#uca_options{}, S1, S2) ->
    G1 = generator(C, preprocess(S1)),
    G2 = generator(C, preprocess(S2)),
    do_compare(G1, G2).

-spec do_compare(uca_generator(), uca_generator()) -> uca_compare_result().
do_compare(G1, G2) ->
    case {G1(), G2()} of
    {stop, stop} -> equal;
    {stop, ____} -> lower;
    {____, stop} -> greater;

    {{W1, NewG1}, {W2, NewG2}} 
        when W1 =:= W2 ->
        do_compare(NewG1, NewG2);
    {{W1, _NewG1}, {W2, _NewG2}} 
        when W1 < W2 -> lower;
    {{W1, _NewG1}, {W2, _NewG2}} 
        when W1 > W2 -> greater
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
    % Return always all 4 levels, because
    % if strength=3, alternate=shifted, array will be incorrect.
    NewC = ux_uca_options:get_options(C, [{'strength', 4}]),
    do_sort_array(NewC, D, preprocess(S), W, A).

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
    ux_uca_sort_key_binary_cs:sort_key(C, preprocess(S));
sort_key(C=#uca_options{sort_key_format=F}, S) ->
    case F of
    'binary' ->
        ux_uca_sort_key_binary:sort_key(C, preprocess(S));
    'list' ->
        ux_uca_sort_key_list:sort_key(C, preprocess(S));
    'uncompressed' ->
        ux_uca_sort_key_uncompressed:sort_key(C, preprocess(S))
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



weights(S) ->
    C = get_options(),
    weights(C, S).

weights(C=#uca_options{strength=S}, Str) ->
    List = sort_array(C, Str),

    D = get_ducet(),
    A = ux_uca_alt:get_alternate_function(C, D),
    do_weights(A, S, List, []).


%% Apply the alternate function for the list of weights.
do_weights(A, S, [H|T], Acc) ->
    {NewA, Ints} = do_alt(A, H, S),
    NewAcc = case Ints of
        [_|_] -> [Ints|Acc];
        []    -> Acc
        end,
    do_weights(NewA, S, T, NewAcc);
do_weights(A, S, [], Acc) ->
    NewAcc = lists:reverse(Acc),
    {A, NewAcc}.

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


%% ==Normal version:==
%%
%% ```
%% do_generator    L1 -> L1 -> L1 ->
%% reverse         <----------------
%% do_generator2   L2 -> L2 -> L2 ->
%% reverse         <----------------
%% do_generator2   L3 -> L3 -> L3 ->
%% reverse         <----------------
%% do_generator2   L4 -> L4 -> L4 '''


%% ==Backward version:==
%%
%% ```
%% do_generator    L1 -> L1 -> L1 ->
%% do_generator3   L2 <- L2 <- L2 <-
%% do_generator2   L3 -> L3 -> L3 ->
%% reverse         <----------------
%% do_generator2   L4 -> L4 -> L4 '''

-spec do_generator(#uca_options{}, string(), fun(), fun(), uca_array(),
    [[uca_weight()]]) -> {uca_weight(), fun()}|stop.

do_generator(#uca_options{}=C, S, D, A, [WH|WT], R) ->
    {NewA, Ints} = do_alt(A, WH),
    case Ints of
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
        {L1, F};
    _ -> % [] try extract next
        do_generator(C, S, D, NewA, WT, R)
    end;
do_generator(#uca_options{strength=S, backwards=false}, []=_S, _D, _A, []=_W, R) ->
    F = fun() -> do_generator2(S-1, lists:reverse(R), []) end,
    {0, F}; %% All weights were extracted. Try get weights from LVL=2.
% reverse L2
do_generator(#uca_options{strength=S, backwards=true}, []=_S, _D, _A, []=_W, R) ->
    F = fun() -> do_generator3(S-1, R, []) end,
    {0, F}; %% All weights were extracted. Try get weights from LVL=2.
do_generator(C=#uca_options{}, [_|_]=S, D, A, []=_W, R) ->
    {NewW, NewS} = do_extract(C, S, D),
    do_generator(C, NewS, D, A, NewW, R).

%% @param S::integer() Strength (not string).
-spec do_generator2(0..4, [[uca_weight()]],
    [[uca_weight()]]) -> {uca_weight(), fun()}|stop.

do_generator2(0, _W, _R) ->
    stop;
do_generator2(1, []=_W, _R) ->
    stop;
do_generator2(_S, []=_W, []=_R) ->
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


%% backward version
do_generator3(0, _W, _R) ->
    stop;
do_generator3(1, []=_W, _R) ->
    stop;
do_generator3(_S, []=_W, []=_R) ->
    stop;
do_generator3(S, []=_W, R) ->
    F = fun() -> % not reverse
            do_generator2(S-1, R, []) 
        end,
    {0, F};
do_generator3(S, [[0=_WH]|WT], R) ->
    do_generator3(S, WT, R);
do_generator3(S, [[0=_WH|WR]|WT], R) ->
    do_generator3(S, WT, [WR|R]);
do_generator3(S, [[WH]|WT], R) ->
    F = fun() -> do_generator3(S, WT, R) end,
    {WH, F};
do_generator3(S, [[WH|WR]|WT], R) ->
    F = fun() -> do_generator3(S, WT, [WR|R]) end,
    {WH, F}.

    
%% 1|x|x  => 1|x|x
%% 1|1|x  => 1|x|x
%% 1|0|1  => 1|x|x
%% 1|0|0  => 1|1|1
%%           1|3|2
prefix_weight([N,0|T]) ->
    NewN = N + 1,
    NewEl = [NewN|T],
    prefix_weight(NewEl);
prefix_weight([N]) ->
    false;
prefix_weight(El) -> 
    El.

prefix_weights([H|T], Acc) ->
    case prefix_weight([1|H]) of
    false -> 
        %skip:
        prefix_weights(T, Acc);

    NewH -> 
        NewAcc = [NewH|Acc],
        prefix_weights(T, NewAcc)
    end;
prefix_weights([], Acc) ->
    lists:reverse(Acc).
    
%% http://unicode.org/reports/tr10/#Searching
-spec search(Target::string(), Pattern::string()) -> 
                search_result().

search(T, P) ->
    C = get_options(),
    M = 'minimal',
    search(C, T, P, M).

-type search_result()::{string(),string(),string()}.

-spec search(Target::string(), Pattern::string(), MatchStyle::atom()) -> 
                search_result();
            (#uca_options{}, Target::string(), Pattern::string()) -> 
                search_result().

%% M is match-style:
search(T, P, M)
    when is_atom(M) ->
    C = get_options(),
    search(C, T, P, M);
search(C=#uca_options{}, T, P) ->
    M = 'minimal',
    search(C, T, P, M).


-spec search(#uca_options{}, Target::string(), Pattern::string(), 
        MatchStyle::atom()) -> 
            search_result().

search(C=#uca_options{}, T, P, 'medium') ->
    NewOpts = [{'sort_key_format', 'uncompressed'}],
    NewC = ux_uca_options:get_options(C, NewOpts),

    % Retrieve the sort key of the substring;
    {_NewAlt, AltW} = weights(NewC, P),
    % Convert to weights with prefix:
    PW = prefix_weights(AltW, []), 
    
    D = get_ducet(),
    A = ux_uca_alt:get_alternate_function(C, D),
    Skipped = [],

    

    case do_search('first_minimal', C, P, D, A, PW, Skipped) of
    {[], _, []} ->
        % is equal
        do_search('first_minimal', C, T, D, A, PW, Skipped);
    
    {SubBefore, SubMatch, SubAfter} = SubV ->
        case do_search('maximal', C, T, D, A, PW, Skipped) of
        false -> false;
        {MaxBefore, MaxMatch, MaxAfter} = MaxV ->
            MinV =
                do_search('first_minimal', C, MaxMatch, D, A, PW, Skipped),
            {MinBefore, MinMatch, MinAfter} = MinV,


%   error_logger:info_msg(
%       "~w: "
%           "Max ~w. ~n"
%           "Min ~w. ~n"
%           "Sub ~w. ~n", 
%       [?MODULE, MaxV, MinV, SubV]),
        
        % concat the left part        
        {MedBeforeTail, MedMatch1} = 
            do_split(lists:reverse(MinBefore), lists:reverse(SubBefore), MinMatch),

        MedBefore = MaxBefore++lists:reverse(MedBeforeTail),
        
        % concat the right part        
        {MedAfterTail, MedMatch2} = 
            do_split(MinAfter, SubAfter, lists:reverse(MedMatch1)),
        
        MedAfter = MedAfterTail++MaxAfter,
                
        MedMatch = lists:reverse(MedMatch2),

        {MedBefore, MedMatch, MedAfter}
        end        
    end;

search(C, S, P, M) ->
    NewOpts = [{'sort_key_format', 'uncompressed'}],
    NewC = ux_uca_options:get_options(C, NewOpts),

    % Retrieve the sort key of the substring;
    {_NewAlt, AltW} = weights(NewC, P),
    % Convert to weights with prefix:
    PW = prefix_weights(AltW, []), 
    
    D = get_ducet(),
    A = ux_uca_alt:get_alternate_function(C, D),
    Skipped = [],

    NewM = case M of
    'minimal' -> 'first_minimal';
    'maximal' -> 'maximal'
    end,

    do_search(NewM, C, S, D, A, PW, Skipped).

%% @private
do_split([H|T1], [H|T2], Acc) ->
    do_split(T1, T2, [H|Acc]);
do_split(T1, _T2, Acc) ->
    {T1, Acc}.


do_search(M, C, [H|T]=S, D, A, PW, Skipped) ->
    case do_search_extract(M, C, S, D, A, PW) of
    'stop' -> false;
    false -> 
        NewSkipped = [H|Skipped],
        do_search(M, C, T, D, A, PW, NewSkipped);
    {true,  NewT} ->
        NewSkipped = lists:reverse(Skipped),
        Matched = delete_tail(S, NewT),
        {NewSkipped, Matched, NewT}
    end;
do_search(M, _C, []=_S, _D, _A, _K, _Skipped) ->
    false.

%% Delete `length(Tail)' charactes from `From'.
%% Fast realization, But don't check Tail string.
delete_tail(From, Tail) ->
    Rev = lists:reverse(From),
    do_delete_tail(Rev, Tail).

do_delete_tail([_FH|FT], [_TH|TT]) ->
    do_delete_tail(FT, TT);
do_delete_tail(Rev, []) ->
    lists:reverse(Rev).


-spec do_search_extract(M::atom(), #uca_options{}, S::string(), 
        D::fun(), A::fun(),
        PW::uca_weights()) -> term().

do_search_extract('first_minimal'=_M, 
    C=#uca_options{strength=L}, S, D, A, PW) ->
    case do_extract(C, S, D) of
    {[], _} -> 
        'stop';
    {[_|_]=NewW, NewS} -> 
        {NewA, AltW} = do_weights(A, L, NewW, []),
        case is_ignorable_array(L, AltW) of
        true -> false; % reject ignorables
        false -> 
    
            case search_match(C, PW, AltW) of
            false -> false;
            true  -> {true, NewS};
            {'more', NewPW} ->
                NewM = 'minimal',
                do_search_extract(NewM, 
                    C, NewS, D, NewA, NewPW) 
            end

        end
    end;

do_search_extract(M, 
    C=#uca_options{strength=L}, S, D, A, PW) ->

    case do_extract(C, S, D) of
    {[], _} -> 
        'stop';

    {[_|_]=NewW, NewS} -> 
        {NewA, AltW} = do_weights(A, L, NewW, []),
    
        case search_match(C, PW, AltW) of
        false -> 
            false;

        true when M=:='maximal' -> 
            NewNewS = delete_ignorables(C, NewS, D, A),
            {true, NewNewS};

        true -> 
            {true, NewS};

        {'more', NewPW} ->
            do_search_extract(M, 
                C, NewS, D, NewA, NewPW) 
        end

    end.


search_match(C=#uca_options{strength=MaxL}, PW, W) ->
    L = 1,
    SkippedPW = [], % skipped weights from PW.
    SkippedW = [], % skipped weights from W.
    do_search_match(MaxL, L, PW, SkippedPW, W, SkippedW).





%% @param MaxL strength
%% @param L Level:    1->2...->MaxL
%% @param PW          Pattern weights (weights after `prefix_weights')
%% @param SkippedPW   Skipped pattern weights
%% @param W           Target weights
%% @param SkippedW    Skipped target weights

% skip ignorable
do_search_match(MaxL, L, PW, SkippedPW, [[0]=_HW|TW], SkippedW) ->
    do_search_match(MaxL, L, PW, SkippedPW, TW, SkippedW);
    
% skip and save the tail
do_search_match(MaxL, L, PW, SkippedPW, [[0|THW]=_HW|TW], SkippedW) ->
    NewSkippedW = [THW|SkippedW],
    do_search_match(MaxL, L, PW, SkippedPW, TW, NewSkippedW);

% matched H
do_search_match(MaxL, L, [[L,H|THS]=_HS|TS], SkippedPW, 
    [[H|HHW]=_HW|TW], SkippedW) ->
    NewHS = prefix_weight([L+1|THS]),
    NewSkippedPW = 
        case NewHS of
        false ->
            %skip:
            SkippedPW;
        _ ->
            [NewHS|SkippedPW]
        end,

    NewSkippedW = 
        case HHW of
        [] -> SkippedW;
        _ -> [HHW|SkippedW]
        end,

    do_search_match(MaxL, L, TS, NewSkippedPW, 
        TW, NewSkippedW);
    
% skip WTF
do_search_match(MaxL, L, PW, SkippedPW, [[]=_HW|TW], SkippedW) ->
    do_search_match(MaxL, L, PW, SkippedPW, TW, SkippedW);
    
% All levels was matched
do_search_match(MaxL, L, PW, SkippedPW, []=_W, []=_SkippedW) ->
    NewPW = append_skipped(PW, SkippedPW),
    case NewPW of
    [] -> true;
    _  -> {'more', NewPW}
    end;

% This level was matched. Run next level.
do_search_match(MaxL, L, PW, SkippedPW, []=_W, SkippedW) ->
    NewL = L + 1,
    NewPW = append_skipped(PW, SkippedPW),
    NewSkippedPW = [],
    NewW = lists:reverse(SkippedW),
    NewSkippedW = [],
    do_search_match(MaxL, NewL, NewPW, NewSkippedPW, NewW, NewSkippedW);

do_search_match(_MaxL, _L, _PW, _SkippedPW, _W, _SkippedW) ->
    false.

    
    





%% @private
append_skipped(W, [H|T]) ->
    append_skipped([H|W], T);
append_skipped(W, []) ->
    W.

    



%% Deletes all ignorables from the beginning of the string
%% @private
delete_ignorables(C=#uca_options{strength=L},[_|_]=S,D,A) ->
    {NewW,NewS} = do_extract(C, S, D),
    {AltA, AltW} = do_weights(A, L, NewW, []),
    case is_ignorable_array(L, AltW) of
    true -> 
        delete_ignorables(C,NewS,D,A);
    false ->
        S
    end;
delete_ignorables(_C,[]=_S,D,A) ->
    [].

%% Warning: length(El) =< L
%%
%% @param L::integer() Max Level
%% @param A::[El] Array
%% @private
is_ignorable_array(L, A) ->
    Mask = lists:duplicate(L, 0),
    lists:all(fun(El) -> El=:=Mask end, A).

    
preprocess(S) ->
    %% TODO: normalization can be delayed.
    ux_string:to_nfd(S).
    

%%
%% Tests
%%

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-define(TO(X), {timeout, 30, [X]}).

delete_tail_test_() ->
    F = fun delete_tail/2,
    [?_assertEqual(F("Head and End", " and End"), "Head")
    ].

split_levels_test_() ->
    F = fun(W) -> split_levels(1, false, W) end,
    F2 = fun(W) -> split_levels(2, true, W) end,
    [{"Test common behavior.", 
        [?_assertEqual(F([[1,2,3],[4,5,6],[7,8,9]]), {[1,4,7], [[2,3],[5,6],[8,9]]})
        ,?_assertEqual(F([[1,2,3],[4],[7,8]]), {[1,4,7], [[2,3],[8]]})
        ]}
    ,{"Test backwards.",
        [?_assertEqual(F2([[1,2,3],[4],[7,8]]), {[7,4,1], [[2,3],[8]]})
        ]}
    ].


search_test_() ->
    C = ux_uca_options:get_options([{strength,2}]),
    F = fun(StyleType, Target, Pattern) ->
        ux_uca:search(C, Target, Pattern, StyleType)
        end,

    FF = fun(StyleType) ->
        Target = "def$!Abc%$ghi",
        Pattern = "*!abc!*",
        ux_uca:search(C, Target, Pattern, StyleType)
        end,

    FF2 = fun(StyleType) ->
        Target = "def@!Abc%@ghi",
        Pattern = "*!abc!*",
        ux_uca:search(C, Target, Pattern, StyleType)
        end,


   [{"Simple match tests",
    [?TO(?_assertEqual(F('minimal',"F","F"), {"","F",""}))
    ]
    }
   ,{"http://unicode.org/reports/tr10/#Matches_Table",
    [{"The minimal match is the tightest one, because $! and %$ are "
            "ignored in the target.",
        
         [?TO(?_assertEqual(FF('minimal'), {"def$!","Abc","%$ghi"}))
         ,?TO(?_assertEqual(FF2('minimal'), {"def@!","Abc","%@ghi"}))
         ]
     }

    ,{"The medial one includes those characters that are binary equal.",
        
         [?TO(?_assertEqual(FF('medium'),  {"def$","!Abc","%$ghi"}))
         ,?TO(?_assertEqual(FF2('medium'),  {"def@","!Abc","%@ghi"}))
         ]
     }

    % TODO: Is this error in UCA?
    % ux_unidata:ducet("$").
    % [[non_variable,5492,32,2,36]]
    % 
    % $ is not ignorable, but in example it is.

    ,{"The maximal match is the loosest one, including the surrounding"
            "ignored characters.",

    % From example:
%     ?_assertEqual(FF('maximal'), {"def","$!Abc%$","ghi"})
    
    % For real data:
         [?TO(?_assertEqual(FF('maximal'), {"def$","!Abc%","$ghi"}))
         ,?TO(?_assertEqual(FF2('maximal'), {"def","@!Abc%@","ghi"}))
         ]
    }
    ]}].
    

-endif.
