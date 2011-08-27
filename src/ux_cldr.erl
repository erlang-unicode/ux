-module(ux_cldr).

-export([set_value/3, get_value/1]).



set_value('node', Key, Value) ->
    ux_cldr_server:set_default(Key, Value);
    
set_value(Level, Key, Value) ->
    ClientPid = case Level of
        'process' -> 
            self();
        'application' ->
            % We unload data, when the application die.
            {ok, AppName1} = application:get_application(), % let it crash
            get_application_pid(AppName1)
    end,


    case Level of
    'process' -> 
        % Put to the process dictionary.
        put(Key, Value);
    'application' ->
        {ok, AppName2} = application:get_application(), 
    application:set_env(AppName2, Key, Value)
    end,
    ok.


get_value(Key) ->
    case get_value('process', Key) of
    'undefined' ->
        case get_value('application', Key) of
        'undefined' ->
            get_value('node', Key);
        Value ->
            Value
        end;
    Value ->
        Value
    end.
            

get_value('process', Value) ->
    erlang:get(Value);
get_value('application', Value) ->
    application:get_env(Value);
get_value('node', Value) ->
    case erlang:whereis(ux_cldr_server) of
    undefined ->
        ux:start(),
        ux_cldr_server:get_default(Value);
    _ -> 
        ux_cldr_server:get_default(Value) 
    end.


%% Convert the name of the application to its pid.
get_application_pid(Name) ->
    AInfo = application:info(),
    {'running', R} = lists:keyfind('running', 1, AInfo),
    {Name, Pid} = lists:keyfind(Name, 1, R).







%%
%% This functions are used in CLDR parsers.
%%

get_env() ->
    lists:map(fun(X) ->
        {X, get_value(X)}
        end, ['locale']).


set_env(Env) ->
    lists:map(fun({Key, Value}) ->
        set_value('process', Key, Value)
        end, Env).






    
        
