%%% Convert bin weight to [int].
%%% @private
-module(ux_uca_alt).
-export([get_alternate_function/2]).
-include("ux_uca.hrl").

-spec get_alternate_function(#uca_options{}, fun()) -> fun().
get_alternate_function(#uca_options{alternate='shifted', strength=4}, D) ->
    R = D({reassign_function, 4}),
    Common = R(get_common_value), 
    shifted_weight(Common);

get_alternate_function(C=#uca_options{alternate=Alt}, _D) ->
    get_function(Alt).

    
-spec get_function(Alt :: ux_uca:uca_alternate()) -> fun().
get_function(non_ignorable) ->
    fun non_ignorable_weight/1;
get_function(blanked) ->
    fun blanked_weight/1;
% Strength<4
get_function(shifted) ->
    fun short_shifted_weight/1;
get_function(shift_trimmed) ->
    fun shift_trimmed_weight/1.


non_ignorable_weight(Value) ->
        {fun non_ignorable_weight/1, weight(Value)}.

weight([_Var|L]) -> L.

%% @private
% If it is a tertiary ignorable, then L4 = 0.
shifted_weight(Common) ->
    fun(['variable',0,0,0,_]) ->
        {shifted_weight2(Common), []}; % [0,0,0,0]
       (['non_variable',0,0,0,_]) ->
        {shifted_weight(Common), []}; % [0,0,0,0]
% If it is a variable, then L4 = Old L1.
       (['variable',L1|_]) ->
        {shifted_weight2(Common), [0, 0, 0, L1]};
       ([_|_] = Value) ->
        {shifted_weight(Common), set_l4_to_value(Value, Common)}
    end.


%% @doc This function is a version of shifted_weight/1, but its value is
%%      after variable.
%% @end
%% @private
% If it is a ignorable, then L4 = 0.
shifted_weight2(Common) ->
    fun([_Var,0,_,_,_]) ->
        {shifted_weight2(Common), []}; % [0,0,0,0]
% If it is a variable, then L4 = Old L1.
       ([variable,L1|_]) ->
        {shifted_weight2(Common), [0, 0, 0, L1]};
       ([_|_] = Value) ->
        {shifted_weight(Common), set_l4_to_value(Value, Common)}
    end.



%% This realizations is faster.
%% When strenght < 4

%% @private
% If it is a tertiary ignorable, then L4 = 0.
short_shifted_weight(['non_variable',0,0,0|_]) ->
    {fun short_shifted_weight/1, []}; % [0,0,0,0]
% If it is a variable, then L4 = Old L1.
short_shifted_weight(['variable'|_]) ->
    {fun short_shifted_weight2/1, []};
short_shifted_weight([_|T]) ->
    {fun short_shifted_weight/1, T}.


%% @doc This function is a version of shifted_weight/1, but its value is
%%      after variable.
%% @end
%% @private
% If it is a ignorable, then L4 = 0.
short_shifted_weight2([_Var,0,_,_|_]) ->
    {fun short_shifted_weight2/1, []}; % [0,0,0,0]
% If it is a variable, then L4 = Old L1.
short_shifted_weight2(['variable'|_]) ->
    {fun short_shifted_weight2/1, []};
short_shifted_weight2([_|T]) ->
    {fun short_shifted_weight/1, T}.










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
