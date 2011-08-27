-module(ux_cldr).

-export([set_value/3, get_value/2]).



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




















%% Read CLDR, get some UNIDATA and run convertor from an ETS table with UNIDATA.
%% This function must be running from the client code, but not by a client directly.
generate_function(Type) ->
    L = get_value('locale'),
    Options = 
        case element(1, Type) of
        'collation' ->
            D = ux_unidata_filelist:get_source('allkeys', 'ducet'),
            [{'ducet', D}]
        end,

    
        
