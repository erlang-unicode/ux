%%% Convert bin weight to [int].
%%% @private
-module(ux_uca_alt).
-export([get_alternate_function/2]).
-include("ux_uca.hrl").

-spec get_alternate_function(#uca_options{}, fun()) -> fun().
get_alternate_function(#uca_options{alternate='shifted', strength=S}, D)
    when S>=4 ->
    Alt = 'shifted',
    F = get_function(Alt),
    R = D({reassign_function, 4}),
    Common = R(get_common_value), 
    get_proxy_fun(F, Common);
get_alternate_function(#uca_options{alternate=Alt}, _D) ->
    get_function(Alt).
    
-spec get_function(Alt :: uca_alternate()) -> fun().
get_function(non_ignorable) ->
    fun non_ignorable_weight/1;
get_function(blanked) ->
    fun blanked_weight/1;
get_function(shifted) ->
    fun shifted_weight/1;
get_function(shift_trimmed) ->
    fun shift_trimmed_weight/1.


%% Replaces 16#FFFF from the 4 level on the Common value.
get_proxy_fun(F, Common) ->
    fun(W) ->
        {NewF, NewW} = F(W),
        case NewW of
        [L1,L2,L3,16#FFFF] ->
            NewW2 = [L1,L2,L3,Common],
            NewF2 = get_proxy_fun(NewF, Common),
            {NewF2,NewW2};
        _ ->
            NewF2 = get_proxy_fun(NewF, Common),
            {NewF2,NewW}
        end
    end.
        
    

non_ignorable_weight(Value) ->
        {fun non_ignorable_weight/1, weight(Value)}.

weight([_Var|L]) -> L.

%% @private
% If it is a tertiary ignorable, then L4 = 0.
shifted_weight([_Var,0,0,0,0]) ->
    {fun shifted_weight/1, []}; % [0,0,0,0]
% If it is a variable, then L4 = Old L1.
shifted_weight([variable,L1|_]) ->
    {fun shifted_weight2/1, [0, 0, 0, L1]};
shifted_weight([_|_] = Value) ->
    {fun shifted_weight/1, set_l4_to_value(Value, 16#FFFF)}.


%% @doc This function is a version of shifted_weight/1, but its value is
%%      after variable.
%% @end
%% @private
% If it is a ignorable, then L4 = 0.
shifted_weight2([_Var,0,0,0,0]) ->
    {fun shifted_weight2/1, []}; % [0,0,0,0]
% If it is a variable, then L4 = Old L1.
shifted_weight2([variable,L1|_]) ->
    {fun shifted_weight2/1, [0, 0, 0, L1]};
shifted_weight2([_|_] = Value) ->
    {fun shifted_weight/1, set_l4_to_value(Value, 16#FFFF)}.

%% @private
%% Alternate=Shifted, Strength=L3
blanked_weight([variable|_]) ->
    {fun blanked_weight2/1, []}; % [0,0,0]
blanked_weight([_|_] = Value) ->
    {fun blanked_weight/1, weight(Value)}.

%% @private
blanked_weight2([_Var, 0|_]) ->
    {fun blanked_weight2/1, []};
blanked_weight2(['variable'|_]) ->
    {fun blanked_weight2/1, []};
blanked_weight2([_|_] = Value) ->
    {fun blanked_weight/1, weight(Value)}.


%% @private
% If it is a tertiary ignorable, then L4 = 0.
shift_trimmed_weight([_Var, 0|_]) ->
    {fun shift_trimmed_weight/1, []}; % [0,0,0,0]
% If it is a variable, then L4 = Old L1.
shift_trimmed_weight([variable, L1|_]) ->
    {fun shift_trimmed_weight2/1, [0, 0, 0, L1]};
shift_trimmed_weight([_|_] = Value) ->
    {fun shift_trimmed_weight/1, set_l4_to_value(Value, 0)}.

%% @doc This function is a version of shifted_weight/1, but its value is
%%      after variable.
%% @end
%% @private
% If it is a ignorable, then L4 = 0.
shift_trimmed_weight2([_Var,0|_]) ->
    {fun shift_trimmed_weight2/1, []}; % [0,0,0,0]
% If it is a variable, then L4 = Old L1.
shift_trimmed_weight2([variable, L1|_]) ->
    {fun shift_trimmed_weight2/1, [0, 0, 0, L1]};
shift_trimmed_weight2([_|_] = Value) ->
    {fun shift_trimmed_weight/1, set_l4_to_value(Value, 0)}.


%% @private
%% Return: [_, _, _, _]
set_l4_to_value([Var, L1, L2, L3, _L4], NewL4) ->
    [L1, L2, L3, NewL4].
