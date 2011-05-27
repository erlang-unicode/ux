
% Additional information (and links)
% =================================

% Collation
% ---------
% 1. Hangul Collation Requirements 
% http://www.open-std.org/jtc1/sc22/wg20/docs/n1037-Hangul%20Collation%20Requirements.htm
% PS: There is the main source of information.

% 2. Terminator weight for Hangul
% http://code.activestate.com/lists/perl-unicode/2163/

% 3. Theory vs. practice for Korean text collation
% http://blogs.msdn.com/b/michkap/archive/2005/02/25/380266.aspx
% PS: there is no any practice. They do not the UCA :/

% 4. http://en.wikipedia.org/wiki/Unicode_collation_algorithm

% 6. Unicode implementer's guide part 3: Conjoining jamo behavior
% http://useless-factor.blogspot.com/2007/08/unicode-implementers-guide-part-3.html

% 7. Unicode implementer's guide part 5: Collation
% http://useless-factor.blogspot.com/2007/10/unicode-implementers-guide-part-5.html

% 8. Unicode collation works now
% http://useless-factor.blogspot.com/2008/05/unicode-collation-works-now.html
% PS: I found it so late. :(
% FIXED: Combining character contractions. Apparently, two combining marks can 
%        form a contraction. A straight reading of the UCA wouldn't predict 
%        this, but not all of the UCA tests pass unless you check for 
%        non-adjacent combining marks being in a contraction together, without 
%        a noncombining mark to start it off.


-module(uxcol).
-include("uxstring.hrl").
-include("uxchar.hrl").
-include("uxcol.hrl").

-export([non_ignorable/2,
        blanked/2,
        shifted/2,
        shift_trimmed/2,
        sort_array/1,
        sort_array_non_ignorable/1,
        sort_array_blanked/1,
        sort_array_shifted/1,
        sort_array_shift_trimmed/1,
        sort_key/2,
        sort/2
        ]).


ducet_r(V) -> uxunidata:ducet_r(V).
ccc(V) -> uxunidata:ccc(V).


%     %  %%%%%     %
%     % %     %   % %
%     % %        %   %
%     % %       %     %
%     % %       %%%%%%%
%     % %     % %     %
 %%%%%   %%%%%  %     %


% UNICODE COLLATION ALGORITHM
% see Unicode Technical Standard #10

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

%% Variable collation elements are not reset to be ignorable, but
%% get the weights explicitly mentioned in the file.
%% * SPACE would have the value [.0209.0020.0002]
%% * Capital A would be unchanged, with the value [.06D9.0020.0008]
%% * Ignorables are unchanged.
non_ignorable(S1, S2) ->
    compare  (S1, S2,
        fun ducet_r/1, % ducet_r(reversed_in) -> non_reversed_key;
        fun non_ignorable_bin_to_list/1).

%% In:  not reversed string.
%% Out: not reversed weight list.
ducet(A) -> ducet_r(lists:reverse(A)).


%% Blanked: Variable collation elements and any subsequent ignorables 
%% are reset so that their weights at levels one through three are zero. 
%% For example,
%% * SPACE would have the value [.0000.0000.0000]
%% * A combining grave accent after a space would have the value [.0000.0000.0000]
%% * Capital A would be unchanged, with the value [.06D9.0020.0008]
%% * A combining grave accent after a Capital A would be unchanged
blanked(S1, S2) ->
    compare  (S1, S2,
        fun ducet_r/1, % ducet_r(reversed_in) -> non_reversed_key;
        fun blanked_bin_to_list/1).

%% Shifted: Variable collation elements are reset to zero at levels one through
%% three. In addition, a new fourth-level weight is appended, whose value 
%% depends on the type, as shown in Table 12.
%% Any subsequent primary or secondary ignorables following a variable are reset
%% so that their weights at levels one through four are zero.
%% * A combining grave accent after a space would have the value 
%%   [.0000.0000.0000.0000].
%% * A combining grave accent after a Capital A would be unchanged.
shifted(S1, S2) ->
    compare  (S1, S2,
        fun ducet_r/1, % ducet_r(reversed_in) -> non_reversed_key;
        fun shifted_bin_to_list/1).

%% Shift-Trimmed: This option is the same as Shifted, except that all trailing 
%% FFFFs are trimmed from the sort key. 
%% This could be used to emulate POSIX behavior.
shift_trimmed(S1, S2) ->
    compare  (S1, S2,
        fun ducet_r/1, % ducet_r(reversed_in) -> non_reversed_key;
        fun shift_trimmed_bin_to_list/1).

non_ignorable_bin_to_list(Value) ->
        {fun non_ignorable_bin_to_list/1, bin_to_list(Value)}.

%% Convert binary from DUCET to list [L1, L2, L3, L4]
%% A variable CE is "*" in ducet (1).
%% A non-varialbe CE is "." in ducet (0).
%% @private
bin_to_list(<<_Variable:8, L1:16, L2:16, L3:16, _:16>>) ->
    [L1, L2, L3];
bin_to_list(<<_Variable:8, L1:16, L2:16, L3:16, _:24>>) ->
    [L1, L2, L3];
% For hangul
bin_to_list(L1) when is_integer(L1) ->
    [L1, 0,  0].


%% @private
% If it is a tertiary ignorable, then L4 = 0.
shifted_bin_to_list(<<_:8, 0:48, _/binary>>) ->
    {fun shifted_bin_to_list/1, [0, 0, 0, 0]};
% If it is a variable, then L4 = Old L1.
shifted_bin_to_list(<<1:8, L1:16, _/binary>>) ->
    {fun shifted_bin_to_list2/1, [0, 0, 0, L1]};
shifted_bin_to_list(Value) ->
    {fun shifted_bin_to_list/1, set_l4_to_value(Value, 16#FFFF)}.

%% This function is a version of shifted_bin_to_list/1, but its value is
%% after variable.
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
blanked_bin_to_list(<<1:8, L1:16, _/binary>>) ->
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

%% This function is a version of shifted_bin_to_list/1, but its value is
%% after variable.
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
set_l4_to_value(<<_Variable:8, L1:16, L2:16, L3:16, _:16>>, Val) ->
    [L1, L2, L3, Val];
set_l4_to_value(<<_Variable:8, L1:16, L2:16, L3:16, _:24>>, Val) ->
    [L1, L2, L3, Val];
% For hangul
set_l4_to_value(L1, Val) when is_integer(L1) ->
    [L1, 0,  0, Val].



%% Example:
%%  f().
%%  RawData = ["death", "de luge", "de-luge", "deluge", "de-luge", "de Luge", "de-Luge", "deLuge", "de-Luge", "demark"].
%%  Data = lists:map(fun lists:flatten/1, RawData).
%%  uxstring:sort(Data, non_ignorable).
%%  uxstring:sort(Data, blanked).
%%  uxstring:sort(Data, shifted).
%%  uxstring:sort(Data, shift_trimmed).
sort(Lists, Fn) ->
    lists:map(fun({_, V} = X) -> X end, 
        lists:keysort(1, 
            sort_map(Lists, Fn, []))).

%% Lists: an array of strings;
%% Fn:    an map function.
%%
%% This function does:
%% lists:map(fun(X) -> sort_key(X, Fn) end, Lists).
%% @private
sort_map([H|T], Fn, Res) ->
    sort_map(T, Fn, [{apply(Fn, [H]), H}|Res]).

%% @private
get_sort_fn(non_ignorable) ->
    fun sort_array_non_ignorable/1;
get_sort_fn(blanked) ->
    fun sort_array_blanked/1;
get_sort_fn(shifted) ->
    fun sort_array_shifted/1;
get_sort_fn(shift_trimmed) ->
    fun sort_array_shift_trimmed/1.

sort_key(Str, Fn) when is_function(Fn) ->
    Array = apply(Fn, [Str]),
    sort_key1(Array, [], []);
sort_key(Str, FnName) when is_atom(FnName) ->
    Fn = get_sort_fn(FnName), 
    sort_key(Str, Fn).
 

%% @private
%% AT is Array Tail.
sort_key1([[0]|AT], Acc, Res) ->
    sort_key1(AT, Acc, Res);
sort_key1([[H]|AT], Acc, Res) ->
    sort_key1(AT, Acc, [H|Res]);
sort_key1([[0|T]|AT], Acc, Res) ->
    sort_key1(AT, [T|Acc], Res);
sort_key1([[H|T]|AT], Acc, Res) ->
    sort_key1(AT, [T|Acc], [H|Res]);
sort_key1([[]|AT], Acc, Res) ->
    sort_key1(AT, Acc, Res);
sort_key1([] = _InArray, [_|_] = Acc, Res) ->
    sort_key1(lists:reverse(Acc), [], [0|Res]);
sort_key1([] = _InArray, [] = _Acc, Res) -> 
    lists:reverse(Res).



% http://unicode.org/reports/tr10/#Variable_Weighting
sort_array(Str) -> 
    sort_array(Str, fun ducet_r/1, fun bin_to_bin/1).

sort_array_non_ignorable(Str) -> 
    sort_array(Str, fun ducet_r/1, fun non_ignorable_bin_to_list/1).

sort_array_blanked(Str) -> 
    sort_array(Str, fun ducet_r/1, fun blanked_bin_to_list/1).

sort_array_shifted(Str) -> 
    sort_array(Str, fun ducet_r/1, fun shifted_bin_to_list/1).

sort_array_shift_trimmed(Str) -> 
    sort_array(Str, fun ducet_r/1, fun shift_trimmed_bin_to_list/1).

%% This function does nothing :)
%% @private
bin_to_bin(Val) ->
    { fun bin_to_bin/1, Val }.

%% TableFun returns value from DUCET table
%% ComparatorFun http://unicode.org/reports/tr10/#Variable%20Weighting
compare (String1, String2, TableFun, ComparatorFun) ->
    compare1(uxstring:to_nfd(String1), 
        uxstring:to_nfd(String2), 
        [], % Buf 1, contains ducet(Char)
        [], % Buf 2
        false, % CompValue 1
        [], % Accumulator for String 1 
            % saves values for next levels comparation
        [], % Accumulator for String 2
        TableFun, % fun uxstring:ducet/1, in chars are REVERSED.
        ComparatorFun, ComparatorFun).

%% MANUAL:
%% S2.1   Find the longest initial substring S at each point 
%%        that has a match in the table.
%% S2.1.1 If there are any non-starters following S, process each non-starter C.
%% S2.1.2 If C is not blocked from S, find if S + C has a match in the table.
%% S2.1.3 If there is a match, replace S by S + C, and remove C.
%%
%% Returns:  {Not reversed list of weight elements, Tail of the string}.
%% @private
-spec extract(string(), fun()) 
    -> {[[integer(), ...], ...], Tail :: string()}.
extract(Str, TableFun) ->
    Res = extract0(Str, TableFun),
    {Weights, StrTail} = Res,
    case hangul(Weights, [], StrTail, TableFun) of
    % There is no any hangul jamo L chars in this string 
    % (or other char with a weight of jamo L char)
    false -> Res;
    HangulRes -> HangulRes
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
hangul([<<_:8, L1:16, _/binary>> = H | T], Acc, StrTail, TableFun) 
    when ?IS_L1_OF_HANGUL_L(L1) ->
    hangul2(l, T, [H|Acc], StrTail, TableFun);
hangul([H|T], Acc, StrTail, TableFun) ->
    hangul(T, [H|Acc], StrTail, TableFun);
hangul([   ], _Acc, _StrTail, _TableFun) ->
    false. % L1 is not found. There is no hangul jamo in this string.

%% L1 was found. 
%% Mod: l
%% @private
hangul2(Mod, [<<_:8, 0:16, _/binary>> = H | T], Acc, StrTail, TableFun) ->
    hangul2(Mod, T, [H|Acc], StrTail, TableFun); % skip an ignorable element.
hangul2(l,  [<<_:8, L1:16, _/binary>> = H | T], Acc, StrTail, TableFun) 
    when ?IS_L1_OF_HANGUL_L(L1) -> % L2 is found. LL*
    hangul2(ll, T, [H|Acc], StrTail, TableFun); 
hangul2(l,  [<<_:8, L1:16, _/binary>> = H | T], Acc, StrTail, TableFun) 
    when ?IS_L1_OF_HANGUL_V(L1) -> % V1 is found. LV*
    hangul2(lv, T, [H|Acc], StrTail, TableFun); 
hangul2(lv, [<<_:8, L1:16, _/binary>> = H | T], Acc, StrTail, _TableFun) 
    when ?IS_L1_OF_HANGUL_T(L1) -> % T1 is found. LVT
    hangul_result(T, [H|Acc], StrTail); 
hangul2(lv, [<<_:8, L1:16, _/binary>> = H | T], Acc, StrTail, _TableFun) 
    when ?IS_L1_OF_HANGUL_V(L1) -> % V2 is found. LVV
    hangul_result(T, [H|Acc], StrTail); 
hangul2(ll, [<<_:8, L1:16, _/binary>> = H | T], Acc, StrTail, _TableFun) 
    when ?IS_L1_OF_HANGUL_V(L1) -> % V1 is found. LLV
    hangul_result(T, [H|Acc], StrTail); 
%hangul2(lv, T, Acc, StrTail, _TableFun) 
%    % Skip and try to found other L. LVX
%    -> hangul_result(T, Acc, StrTail); 
hangul2(_Mod, [H|T], Acc, StrTail, TableFun) ->
    % Skip and try to found other L. LX
    hangul(T, [H|Acc], StrTail, TableFun);
hangul2(Mod,  [   ], Acc, [_|_] = StrTail, TableFun) -> 
    {Weights, StrTail2} = extract0(StrTail, TableFun), % We need more gold.
    hangul2(Mod, Weights, Acc, StrTail2, TableFun);
hangul2(_Mod, [   ], _Acc, [] = _StrTail, _TableFun) -> % L
    false.

%% @private
hangul_result(T, Acc, StrTail) ->
    {lists:reverse(append(T, [?COL_HANGUL_TERMINATOR|Acc])), StrTail}.


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
    case apply(TableFun, [[CP]]) of
    [_|_] = Value -> % from ducet 
        {Value, Tail};
    _ -> % other, more 
        {implicit_weight(CP, 16#FBC0), Tail}
    end;

% Try extract from ducet.
extract0([CP|[]], TableFun) -> % Last Char
    case apply(TableFun, [[CP]]) of 
    [_|_] = Value ->
        {Value, []};
    {set_ignorables_to_0, Value} ->
        {Value, []};
    _ ->
        {[], []}
    end;    
extract0([CP | Tail] = Str, TableFun) ->
    Res = extract1(Tail, TableFun, [CP], 
    false, % Max ccc among ccces of skipped chars beetween the starter char 
           % and the processed char. If there are no skipped chars, then 
           % Ccc1=false.
    [], false),
%   io:format(user, "In: ~w Out: ~w ~n", [Str, Res]),
    Res.




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
            Bin = apply(TableFun, [NewCPList]),
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
                append(CPList, 
                    append(Skipped, Str)), 
                {only_derived, TableFun}); % Or *timed out*
        OldVal =/= false -> {OldVal, append(Skipped, Str)}
    end.

    
%% Used in extract.
%% Fast realization of:
%% TIP: append(Head, Tail) == lists:reverse(Head) ++ Tail
%% @private
-spec append(InStr :: string(), OutStr :: string()) -> string().
append(InStr, OutStr) ->
%   io:format("App: ~w ~w ~n", [InStr, OutStr]),
    append1(InStr, OutStr).
%% @private
append1([H|T], Str) ->
    append1(T, [H|Str]);
append1([], Str) -> Str.


%% 7.1.3 Implicit Weights 
%% The result of this process consists of collation elements that are sorted in
%% code point order, that do not collide with any explicit values in the table,
%% and that can be placed anywhere (for example, at BASE) with respect to the 
%% explicit collation element mappings. By default, implicit mappings are given
%% higher weights than all explicit collation elements.
%% @private
implicit_weight(CP, BASE) ->
    AAAA = BASE + (CP bsr 15),
    BBBB = (CP band 16#7FFF) bor 16#8000,
    [<<0:8, AAAA:16, 16#0020:16, 0002:16, 0:16>>, BBBB]. % reversed


%% Compares on L1, collects data for {L2,L3,L4} comparations.
%% Extract chars from the strings.
%%
%% ComparatorFun    S2.3 Process collation elements according to the 
%%                  variable-weight setting, as described in Section 
%%                  3.6.2, Variable Weighting.
%% @private
-spec compare1(Str1 :: string(), Str1 :: string(), 
        Buf1 :: [binary(), ...], Buf2 :: [binary()], char(), 
        Acc1 :: [[integer(), ...], ...], 
        Acc2 :: [[integer()]], % Acc = [[L2,L3,L4], ...] 
        fun(), fun(), fun()) -> lower | greater | equal.

%% compare1 ALGORITHM.
%% 1. Extract weights from Str1 to Buf1.
%% 2. Extract weights from Str2 to Buf2.
%% 3. Extract L1 weight from Buf1 to W1L1.
%% 4. Exctaxt L1 weight from Buf1 and compare with W1L1.
%% 5a. If W1L1 > W2L1 then Str1 greater Str2.
%% 5b. If W1L1 < W2L1 then Str1 lower   Str2.
%% 5c. If W1L1 = W2L1 and strings have non-compared characters then go to a step 1.
%% 6. Run compare2.
%% @private
compare1([_|_] = Str1, StrTail2, [], Buf2, W1L1, Acc1, 
    Acc2, TableFun, ComparatorFun1, ComparatorFun2) ->
    {Buf1,     % [<<Flag,L1,L2,...>>, ..]
     StrTail1} = extract(Str1, TableFun), 
%   io:format("B1: ~w ~n", [Buf1]),
    compare1(StrTail1, StrTail2, Buf1, Buf2, W1L1, Acc1,
        Acc2, TableFun, ComparatorFun1, ComparatorFun2);
compare1(StrTail1, [_|_] = Str2, Buf1, [], W1L1, Acc1, 
    Acc2, TableFun, ComparatorFun1, ComparatorFun2) ->
    {Buf2,     % [<<Flag,L1,L2,...>>, ..]
     StrTail2} = extract(Str2, TableFun), 
%   io:format("B2: ~w ~n", [Buf2]),
    compare1(StrTail1, StrTail2, Buf1, Buf2, W1L1, Acc1, 
        Acc2, TableFun, ComparatorFun1, ComparatorFun2);
    
%% Extracts a non-ignorable L1 from the Str1.
compare1(StrTail1, StrTail2, [CV1Raw|Buf1], Buf2, false, Acc1, 
    Acc2, TableFun, ComparatorFun1, ComparatorFun2) ->
    % This function can reverse argument or set 0.
    case apply(ComparatorFun1, [CV1Raw]) of 
    {NewFun, [0|Acc]} -> % Find other W1L1
        compare1(StrTail1, StrTail2, Buf1, Buf2, false, [Acc|Acc1], 
            Acc2, TableFun, NewFun, ComparatorFun2);
    {NewFun, [W1L1|Acc]} -> % W1L1 was found. Try find W2L1.
        compare1(StrTail1, StrTail2, Buf1, Buf2, W1L1,  [Acc|Acc1],
            Acc2, TableFun, NewFun, ComparatorFun2)
    end;

compare1(StrTail1, StrTail2, Buf1, [CV2Raw|Buf2], W1L1, Acc1, 
    Acc2, TableFun, ComparatorFun1, ComparatorFun2) ->
    case apply(ComparatorFun2, [CV2Raw]) of
    {NewFun, [0|Acc]} -> % Find other W2L1
        compare1(StrTail1, StrTail2, Buf1, Buf2, W1L1, Acc1, 
            [Acc|Acc2], TableFun, ComparatorFun1, NewFun);
    {_NewFun, [_|_]} when W1L1 == true -> 
        lower;   % Sting 1 was ended; 
                 % string 2 still has a non-ignorable char 
                 % => string2 greater.
    {_NewFun, [W2L1|_]} when W1L1 > W2L1 ->
        greater; % Return result: S1 greater S2 on L1
    {_NewFun, [W2L1|_]} when W1L1 < W2L1 ->
        lower;   % Return result: S1 lower S2 on L1
    {NewFun, [W2L1|Acc]} when W1L1 == W2L1 ->
        compare1(StrTail1, StrTail2, Buf1, Buf2, false, Acc1, 
            [Acc|Acc2], TableFun, ComparatorFun1, NewFun)
    end;

% MANUAL:
% This guarantees that when two strings of unequal length are compared, 
% where the shorter string is a prefix of the longer string, the longer 
% string is always sorted after the shorter in the absence of special
% features like contractions. For example: "abc" < "abcX" where "X" can
% be any character(s).

%% String 1 conrains more codepaints, but we cannot throw them.
compare1([CP1|StrTail1], [] = _Str2, [] = _Buf1, [] = _Buf2, _, Acc1, 
    Acc2, TableFun, ComparatorFun1, ComparatorFun2) ->
    compare1(StrTail1, [], CP1, [], false, Acc1, 
        Acc2, TableFun, ComparatorFun1, ComparatorFun2);

%% String 2 conrains more codepaints, but we cannot throw them.
compare1([] = _Str1, [CP2|StrTail2], [] = _Buf1, [] = _Buf2, _, Acc1, 
    Acc2, TableFun, ComparatorFun1, ComparatorFun2) ->
    compare1([], StrTail2, [], CP2, true, Acc1, 
        Acc2, TableFun, ComparatorFun1, ComparatorFun2);

compare1(_Str1, [] = _Str2, _Buf1, [] = _Buf2, W1L1, _Acc1, 
    _Acc2, _TableFun, _ComparatorFun1, _ComparatorFun2) 
    when (W1L1 > 0) and (W1L1 =/= false) ->
    greater;

compare1([] = _Str1, [] = _Str2, [W1Raw|Buf1], [] = _Buf2, W1L1, Acc1, 
    Acc2, TableFun, ComparatorFun1, ComparatorFun2) when W1L1 == 0 ->
    {NewFun, [W1L1New|Acc]} = apply(ComparatorFun1, [W1Raw]),
    compare1([], [], Buf1, [], W1L1New, [Acc|Acc1], 
        Acc2, TableFun, NewFun, ComparatorFun2);

%% L1 was ended :(
%% Now, Funs are not neeaded.
%% Acc1 and Acc2 are reversed.
compare1([] = _Str1, [] = _Str2, [] = _Buf1, [] = Buf2, false, Acc1, 
    Acc2, _TableFun, _ComparatorFun1, _ComparatorFun2) -> 
%   io:format(user, "~w ~w ~n", [Acc1, Acc2]),
    compare2(lists:reverse(Acc1), 
        lists:reverse(Acc2),
        false, % W1L{2,3,4} 
        [], % Other accumulator. Contains L3 and L4.
        []  % Other acc...
    ).


%%% L2 comparation.
%% Try extract W1LX, but 0 was found => try next weigth in InAcc.
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

%% http://unicode.org/reports/tr10/#Step_2
%% Produce Sort Array
%% @private
sort_array(Str, TableFun, CompFun) ->
    sort_array1(uxstring:to_nfd(Str), TableFun, CompFun, [], []).

sort_array1([], _TableFun, _CompFun, [], Res) ->
    lists:reverse(Res);
sort_array1(Str, TableFun, CompFun, [H|T], Res) ->
    {NewCompFun, Val} = apply(CompFun, [H]),
    sort_array1(Str, TableFun, NewCompFun, T, [Val|Res]);
sort_array1([_|_] = Str, TableFun, CompFun, [], Res) ->
    {Buf, StrTail} = extract(Str, TableFun), 
    sort_array1(StrTail, TableFun, CompFun, Buf, 
%       [test|Res]).
        Res).


% Collation end



  %%%%%  %%%%%%   %%%%    %%%%%   %%%%
    %    %       %          %    %
    %    %%%%%    %%%%      %     %%%%
    %    %            %     %         %
    %    %       %    %     %    %    %
    %    %%%%%%   %%%%      %     %%%%


-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).


sort_key_test_() ->
    M = 'uxcol',
    F = 'sort_key',
    FF = fun uxutils:nope/1,
    [?_assertEqual(M:F([[1,2,3], [4,5,6], [0,7], [8,9]], FF), 
        [1,4,8,0,2,5,7,9,0,3,6])
    ].

append_test_() ->
    F = fun append/2,
    [?_assertEqual(F("ABC", "DEF"), "CBADEF")
    ,?_assertEqual(F("123", F("ABC", "DEF")), "321CBADEF")
    ].

shifted_equal_test_() ->
    F = fun shifted/2,
    [?_assertEqual(F([10973,98], [10972,98]), F([10972,98], [10973,98]))
    ].

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
test(InFd, F, false, StrNum, Max, Res) ->
    OldVal = test_read(InFd, StrNum),
    test(InFd, F, OldVal, StrNum, Max, Res);
test(InFd, F, {OldFullStr, OldVal, StrNum}, _OldStrNum, Max, Res) ->
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
        case F(Val, OldVal) of % collation compare
        % error
        lower -> io:format(user, "Error: ~w ~w ~w ~n", 
                [Val, lower, OldVal]),
            io:format(user,
                " Data1: ~ts Data2: ~ts",
                [OldFullStr, FullStr]),
                
            test(InFd, F, Result, NewStrNum, Max - 1, error);
        % OK (equal or upper). Try next
        _ -> test(InFd, F, Result, NewStrNum, Max - 1, Res)
        end;
    _ -> ok
    end.

%% Read line from a testdata file InFd (see CollationTest.html).
%% Return list of codepaints.
%% Used by test/4.
test_read(InFd, StrNum) ->
    case io:get_line(InFd, "") of
    eof -> ok;
    Data -> 
        try % parse Data
            [Value|_] = uxstring:split(["#", ";", "\n"], Data), 
            %% Converts "0009 0021" to [16#0009, 16#0021]
            lists:map(fun uxstring:hex_to_int/1, 
                      string:tokens(Value, " "))
        of Res -> {Data, Res, StrNum + 1} % {FullStr, Codepaints}
        catch                   
        error:_Reason -> 
            test_read(InFd, StrNum + 1)
        end
    end.

prof(File, Fun, Count) ->
    {ok, InFd} = file:open(File, [read]),
    io:setopts(InFd,[{encoding,utf8}]),
    Res = test(InFd, Fun, false, 0, Count, ok),
    ?assertEqual(Res, ok).


non_ignorable_test_() ->
    {timeout, 600, 
        fun() -> 
            prof(
                ?COLLATION_TEST_DATA_DIRECTORY 
                    % Slow, with comments.
%                   ++ "CollationTest_NON_IGNORABLE.txt", 
                    % Fast version (data from slow version are equal).
                    ++ "CollationTest_NON_IGNORABLE_SHORT.txt", 
                fun non_ignorable/2, 
                1000000) 
        end}.

shifted_test_() ->
    {timeout, 600, 
        fun() -> 
            prof(
                ?COLLATION_TEST_DATA_DIRECTORY 
                    % Slow, with comments.
%                   ++ "CollationTest_SHIFTED.txt", 
                    ++ "CollationTest_SHIFTED_SHORT.txt", 
                fun shifted/2, 
                1000000) end}.


-endif.
