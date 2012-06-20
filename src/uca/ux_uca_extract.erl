-module(ux_uca_extract).
-export([extract/3]).

-include("ux.hrl").
-include("ux_uca.hrl").


-type uca_array() :: ux_uca:uca_array().
-type uca_weight() :: ux_uca:uca_weight().
-type uca_elem() :: ux_uca:uca_elem().
-type result() :: ux_uca:uca_result().
-type ux_ccc() :: ux_types:ux_ccc().

%% @doc MANUAL:
%% S2.1 Find the longest initial substring S at each point
%% that has a match in the table.
%% S2.1.1 If there are any non-starters following S, process each non-starter C.
%% S2.1.2 If C is not blocked from S, find if S + C has a match in the table.
%% S2.1.3 If there is a match, replace S by S + C, and remove C.
%%
%% Returns: {Not reversed list of weight elements, Tail of the string}.
%% @end
%% @private
-spec extract(string(), #uca_options{}, fun()) ->
    result().
extract(C=#uca_options{},D,S) when is_list(S), is_function(D) ->
    do_extract(C,D,S).

%% @param C::#uca_options{} Config
%% @param S::string() String
%% @param D::fun() DUCET function
%% @param W::fun() Weights before extracted weights.
-spec do_extract(string(), #uca_options{}, fun()) ->
    result().
do_extract(#uca_options {
        case_sensitive=CS,
        case_first=CF
    } = C,D,S) ->
    R1 = do_extract0(S, D),
    {W1,S1} = R1,
    {W2,S2} = check_mod(C, W1, D, S1),

    W3 = case CF of
        off -> W2;
        lower -> W2;
        upper -> lists:map(fun case_first_hack/1, W2)
        end,

    W4 = case CS of
        false -> W3;
        true -> lists:map(fun case_sensitive_hack/1, W3)
        end,
    
% ok = check_weights(W4),

    {W4, S2}.


check_mod(#uca_options{natural_sort=NS} = C, W1, D, S1) ->
    case has_mod(W1, NS) of
    true ->
       ?DBG("Run post processing.~n", []),
       % Form function for proxy.
       F = do_proxy(C,D,S1),
       mod_weights(F, W1, NS, []);
    false ->
       ?DBG("Skip post processing.~n", []),
       {W1, S1}
    end.


check_weights(W) -> do_check_weights(W).
do_check_weights([[variable,_,_,_,_]|T]) ->
    do_check_weights(T);
do_check_weights([[non_variable,_,_,_,_]|T]) ->
    do_check_weights(T);
do_check_weights([]) -> ok.

%% This function hides C,D,S from client.
-spec do_proxy(#uca_options{}, fun(), string()) -> fun().
do_proxy(C,D,S) ->
    fun(get_more) ->
        case S of
        [] -> no_more;
        _ ->
            {W, NewS} = do_extract0(S, D),
            F = do_proxy(C,D,NewS),
            {W, F}
        end;
       (term) -> get_terminator(C);
       (restart) ->
            F = do_proxy(C,D,S),
            fun(W, Acc) ->
                do_mod(C,F,W,Acc)
            end;
       %% One hangul sequance was found, restart check_mod
       (mod_continue) ->
            fun(W, Acc) -> 
                {W1, S1} = check_mod(C, W, D, S),
                %% lists:reverse(Acc) ++ W1
                {lists:reverse(Acc, W1), S1} 
                end;
       (Result) -> {lists:reverse(Result),S}
        end.

-spec do_mod(#uca_options{}, fun(), uca_array(), uca_array()) ->
    result().
do_mod(#uca_options {
        natural_sort=NS
    }, F, W, Acc) ->
    mod_weights(F, W, NS, Acc).

-spec get_terminator(#uca_options{}) -> uca_weight().
get_terminator(#uca_options {
        hangul_terminator=Term
    }) -> [non_variable, Term, 0,0,0].

%% @doc Uppercase to sort before lowercase. Remap L3.
%% @private
-spec case_first_hack(uca_elem()) -> uca_elem().
case_first_hack([Var,L1,L2,L3,L4]) ->
    NewL3 = case_invert(L3),
    [Var,L1,L2,NewL3,L4].

%% @private
-spec case_invert(uca_weight()) -> uca_weight().
case_invert(L3) when L3 >= 2, L3 =< 6 ->
    L3 + 6;
case_invert(L3) when L3 >= 8, L3 =< 12 ->
    L3 - 6;
case_invert(L3) ->
    L3.


%% @doc Move L3 before L1.
%% @private
-spec case_sensitive_hack(uca_elem()) -> uca_elem().
case_sensitive_hack([Var,L1,L2,L3,L4]) ->
    [Var,L3,L2,L1,L4].
        

% Hack for numbers.
has_mod([[_Var,L1|_]|T], _NS=true)
    when ?IS_L1_OF_DECIMAL(L1) ->
    true;
has_mod([[_Var,L1|_]|_], _NS)
    when ?IS_L1_OF_HANGUL_L(L1) ->
    true;
has_mod([[_|_]|T], NS) ->
    has_mod(T, NS);
has_mod([], _NS) ->
    false.

% 7.1.5 Hangul Collation
% Interleaving Method
% MANUAL:
% Generate a modified weight table:
% 1. Assign a weight to each precomposed Hangul syllable character,
% with a 1-weight gap between each one.
% (See Section 6.2, Large Weight Values)
% 2. Give each jamo a 1-byte internal weight.
% Also add an internal terminator 1-byte weight (W).
% These are assigned so that al W < T < V < L.
% These weights are separate from the default weights, and are just used
% internally.
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
-spec mod_weights(fun(), uca_array(), boolean(), uca_array()) -> result().
% Hack for numbers.
mod_weights(E, [[Var,L1|LOther]=H|T], _NS=true, Acc)
    when ?IS_L1_OF_DECIMAL(L1) ->
    F = fun(W) -> [Var,W|LOther] end, % define F.
    Num = ?COL_WEIGHT_TO_DECIMAL(L1),
    do_decimal(E, F, Num, T, Acc);
mod_weights(E, [[Var,L1|_]=H|T], _NS, Acc)
    when ?IS_L1_OF_HANGUL_L(L1) ->
    do_hangul(E, l, T, [H|Acc]);
mod_weights(E, [H|T], NS, Acc) ->
    mod_weights(E, T, NS, [H|Acc]);
mod_weights(E, [], _NS, Acc) ->
    E(Acc). % L1 is not found. There is no hangul jamo in this string.


%% @doc Scans the string for the digits.
%% When a non-digit character is extracted, stop extraction and
%% form the weights.
%%
%% @end
%% @param E The proxy function
%% @param F The function which forms a weight element.
%% @param N Number
%% @param W The tail of the weights
%% @param Acc Accumulator for the weights
-spec do_decimal(fun(), fun(), boolean(), uca_array(), uca_array()) -> result().
do_decimal(E, F, N, [[_,0|_]=H|T]=_W, Acc) ->
    do_decimal(E, F, N, T, [H|Acc]); % skip an ignorable element.
do_decimal(E, F, N, [[_,L1|_]=H|T]=_W, Acc)
    when ?IS_L1_OF_DECIMAL(L1) ->
    NewN = (N * 10) + ?COL_WEIGHT_TO_DECIMAL(L1),
    ?DBG("old ~w; new ~w~n", [N, NewN]),
    do_decimal(E, F, NewN, T, Acc);
do_decimal(E, F, N, []=_W, Acc) ->
    % We need more gold. Try extract 1 more char. :)
    case E(get_more) of
    {NewW, NewE} ->
    ?DBG("more ~w~n", [NewW]),
        do_decimal(NewE, F, N, NewW, Acc);
    no_more ->
        NewAcc = decimal_result(F, N, Acc),
        E(NewAcc)
    end;
% Bad char. Cancel last extraction.
do_decimal(E, F, N, W, Acc) ->
    NewAcc = decimal_result(F, N, Acc),
    Restarter = E(restart),
    Restarter(W, NewAcc).


-spec decimal_result(fun(), integer(), uca_array()) -> uca_array().
%% @doc Forms the weight elements.
%% F is function, which gets the L1 weights and returns the full element.
%% For example:
%% ```
%% > decimal_result(F, 100, []).
%% [[100],[16#FFFE],[1]].
%% '''
%% @end
decimal_result(F, N, Acc) ->
    NewAcc = [F(16#FFFE), F(1)|Acc],
    do_decimal_result(F, N, Acc).

-spec do_decimal_result(fun(), integer(), uca_array()) -> uca_array().
do_decimal_result(F, N, Acc) ->
    ?DBG("Res: ~w~n", [N]),
    case N div 16#FFFE of
    0 -> [F(N)|Acc];
    Div ->
        Rem = N rem 16#FFFE,
        do_decimal_result(F, Div, [F(Rem), F(16#FFFE)|Acc])
    end.

%% L1 was found.
%% Mod: l
%% @private
%% @param E Proxy function
%% @param E l, lv, ll Step
%% @param Tail of weights
%% @param Accumulator for weights
-spec do_hangul(fun(), atom(), uca_array(), uca_array()) -> result().
do_hangul(E, Mod, [[_,0|_]=H|T], Acc) ->
   % skip an ignorable element.
   do_hangul(E, Mod, T, [H|Acc]);
do_hangul(E, l, [[_,L1|_]=H|T], Acc)
    when ?IS_L1_OF_HANGUL_L(L1) -> % L2 is found. LL*
    do_hangul(E, ll, T, [H|Acc]);
do_hangul(E, l, [[_,L1|_] = H|T], Acc)
    when ?IS_L1_OF_HANGUL_V(L1) -> % V1 is found. LV*
    do_hangul(E, lv, T, [H|Acc]);
do_hangul(E, lv, [[_,L1|_]=H|T], Acc)
    when ?IS_L1_OF_HANGUL_T(L1) -> % T1 is found. LVT
    hangul_result(E, T, [H|Acc]);
do_hangul(E, lv, [[_,L1|_]=H|T], Acc)
    when ?IS_L1_OF_HANGUL_V(L1) -> % V2 is found. LVV
    hangul_result(E, T, [H|Acc]);
do_hangul(E, lv, [_|_] = W, Acc) -> % X is found. LVX
    hangul_result_and_continue(E, W, Acc);
do_hangul(E, ll, [[_,L1|_]=H|T], Acc)
    when ?IS_L1_OF_HANGUL_V(L1) -> % V1 is found. LLV
    hangul_result(E, T, [H|Acc]);
do_hangul(E, Mod, [], Acc) ->
    case E(get_more) of
    {NewW, NewE} ->
        do_hangul(NewE, Mod, NewW, Acc);
    no_more ->
        E(Acc)
    end;
do_hangul(E, _Mod, W, Acc) -> % L
    Continue = E(mod_continue),
    Continue(W, Acc).


%% @private
-spec hangul_result(fun(), uca_array(), uca_array()) -> result().
hangul_result(E, T, Acc) ->
    TermWeight = E(term),
    E([TermWeight | Acc]).


hangul_result_and_continue(E, W, Acc) ->
    TermWeight = E(term),
    Continue = E(mod_continue),
    Continue(W, [TermWeight|Acc]).


%% Step 0: try extract derived weights.
%% @private
%% @param Str:string() String 
%% @param D::fun() Ducet Function

-spec do_extract0(string(), fun()) -> result().
do_extract0([], _) -> % No Any Char
    {[], []};

% Try extract from ducet.
%% var DFn:fun() Ducet function
%% var CFn:fun() CCC function
%% var LFn:fun() DUCET lookup function
do_extract0([H], D) -> % Last Char
    case D([H]) of
    [_|_] = W ->
        {W, []};
    _ ->
        {[], []}
    end;    

do_extract0([H|T]=S, DFn) ->
    % Max ccc among ccces of skipped chars beetween the starter char 
    % and the processed char. If there are no skipped chars, then 
    % Ccc1=false.
    OldCCC = false, 
    Key = [], 
    Skipped = [],

    LFn = ducet_lookup(DFn),
    CFn = ux_unidata:ccc(skip_check),
    MFn = get_more(LFn, CFn),
    Res = false,
    
    case do_extract1(S, MFn, Key, OldCCC, Skipped, Res) of
    {result, Key2, T2} ->
        W = DFn(Key2),
        ?DBG("W:~w T: ~w~n", [W, T2]),
        {W, T2};
    not_found ->
        {do_implicit(H), T}
    end.



%% @param S:string() String
%% Res is a last good Key.
-spec do_extract1(string(), fun(), string(), ux_ccc()|false, 
    string(), uca_array()) -> 
    {result,string(),string()}|not_found.
do_extract1([H|T]=S, MFn, Key, OldCCC, Skipped, Res) 
    when is_list(Skipped) ->
    NewKey = [H|Key],
    case MFn(NewKey, OldCCC) of
    {false, _NewCCC} when Res =:= more -> 
        more_error;
    {false, NewCCC} -> 
        NewSkipped = [H|Skipped],
        do_extract1(T, MFn, Key, NewCCC, NewSkipped, Res);

    {true, NewCCC} -> 
        CCC = select_ccc(OldCCC, NewCCC),
    ?DBG("selected ccc is ~w.~n", [CCC]),
        do_extract1(T, MFn, NewKey, CCC, Skipped, NewKey);

    {maybe, NewCCC} when Res =:= more ->
        CCC = select_ccc(OldCCC, NewCCC),
        do_extract1(T, MFn, NewKey, CCC, Skipped, more);
    {maybe, NewCCC} ->
        CCC = select_ccc(OldCCC, NewCCC),
        case do_extract1(T, MFn, NewKey, CCC, Skipped, more) of
        more_error -> 
            NewSkipped = [H|Skipped],
            do_extract1(T, MFn, Key, NewCCC, NewSkipped, Res);
        Return -> Return
        end;

    bad_ccc when Res =:= more ->
        more_error;
    bad_ccc when Res =:= false ->
        % http://unicode.org/reports/tr10/#Unassigned_And_Other
        not_found;
    bad_ccc ->
        {result, do_extract1_return(Res), lists:reverse(Skipped, S)}
    end;
        
    
do_extract1([]=_S, _MFn, _Key, _OldCCC, Skipped, _Res=more)
    when is_list(Skipped) ->
    more_error;
do_extract1([]=_S, _MFn, _Key, _OldCCC, Skipped, _Res=false)
    when is_list(Skipped) ->
    not_found;
do_extract1([]=_S, _MFn, _Key, _OldCCC, Skipped, Res) 
    when is_list(Skipped) ->
    {result, do_extract1_return(Res), lists:reverse(Skipped)}.





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
do_implicit(H)  
    when ?CHAR_IS_UNIFIED_IDEOGRAPH(H) ->
    if
        (?CHAR_IS_CJK_COMPATIBILITY_IDEOGRAPH(H) 
            or ?CHAR_IS_CJK_UNIFIED_IDEOGRAPH(H)) ->
        implicit_weight(H, 16#FB40);
      true ->
        implicit_weight(H, 16#FB80)
    end;

do_implicit(H) ->
    implicit_weight(H, 16#FBC0).

        


%% After skiping a character, we set OldCCC = NewCCC.
-spec select_ccc(false|ux_ccc(), ux_ccc()) -> false|ux_ccc().
select_ccc(_OldCCC=false, _NewCCC) ->
    false;
select_ccc(_OldCCC, NewCCC) ->
    NewCCC.
        
-spec do_extract1_return(string()) -> string().
do_extract1_return(Res) -> lists:reverse(Res).








%% @param L::string() List of unicode codepaints
-spec ducet_lookup(fun()) -> fun().
ducet_lookup(D) ->
    D(member_function).



    

-spec get_more(fun(), fun()) -> term().
get_more(LFn, CFn) ->
    fun([H|_]=K, OldCCC) when is_integer(H) ->
        case CFn(H) of
        NewCCC when OldCCC =:= false;
                    OldCCC=/=0, OldCCC<NewCCC ->
            ?DBG("ccc is ok. OldCCC is ~w. NewCCC is ~w. ~n", 
                [OldCCC, NewCCC]),
            Status = LFn(K),
            ?DBG("Status is ~w. ~n", [Status]),
            {Status, NewCCC};

        NewCCC when OldCCC =:= NewCCC, OldCCC =/= 0 ->
            ?DBG("Char is blocked. CCC is ~w. ~n", [OldCCC]),
            {false, NewCCC}; % blocked
        NewCCC ->
            ?DBG("Bad CCC. OldCCC is ~w. NewCCC is ~w ~n", 
                [OldCCC, NewCCC]),
            bad_ccc
        end
    end.
            

% Note: A non-starter in a string is called blocked if there is another 
%       non-starter of the same canonical combining class or zero between 
%       it and the last character of canonical combining class 0.



%% @doc 7.1.3 Implicit Weights 
%% The result of this process consists of collation elements that are sorted in
%% code point order, that do not collide with any explicit values in the table,
%% and that can be placed anywhere (for example, at BASE) with respect to the 
%% explicit collation element mappings. By default, implicit mappings are given
%% higher weights than all explicit collation elements.
%% @end
%% @private
implicit_weight(CP, BASE) when is_integer(CP) and is_integer(BASE) ->
    AAAA = BASE + (CP bsr 15),
    BBBB = (CP band 16#7FFF) bor 16#8000,
    [[non_variable, AAAA, 32, 2, 0], 
     [non_variable, BBBB, 0, 0, 0]]. % reversed


