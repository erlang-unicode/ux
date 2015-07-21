%%% @doc Key-value store for the list of the servers which 
%%%      serve the unidata files.
%%% @private
-module(ux_unidata_filelist).
-include("ux.hrl").

% OTP 
-export([start_link/0]).
-export([init/1, terminate/2, handle_call/3, handle_info/2]).

% Inter-module exports
-export([reg_pid/2, file_owner/1]).

% Unidata API
-export([set_source/4, set_source/2, 
    get_source/2, get_source/1, get_source_from/2]).

-behavior(gen_server).

-record(state, {
        key2server :: dict:dict()
}).


%% Exported Client Functions
%% Operation & Maintenance API
start_link() ->
    Arguments = [],
    Opts = [],
    gen_server:start_link({local, ?MODULE}, ?MODULE, Arguments, Opts).

init([]) ->
    Dict = dict:new(),
    State = #state{
            key2server = Dict
            },
    {ok, State}.

terminate(_Reason, _LoopData) ->
    ok.


% Save pid of ux_unidata_store server in a dict.
% Key is a combination of filename and fileoptions.
handle_call({reg_pid, Key, StorePid}, _From, State = #state{key2server = K2S}) ->
    erlang:monitor(process, StorePid),
    ?DBG("~w: Registrate a new process ~w with the key ~w. ~n", 
        [?MODULE, StorePid, Key]),
    
    {Reply, K2S_2} = case dict:is_key(Key, K2S) of
        false -> 
            {ok, dict:store(Key, StorePid, K2S)};
        true  -> 
            error_logger:error_msg("~w: The key ~w is already registred. ~w",
                [?MODULE, Key]),
            {{error, key_already_registred}, K2S}
    end,
    State_2 = State#state{key2server = K2S_2},
    {reply, Reply, State_2};

handle_call({get_pid, Key}, _From, State = #state{key2server = K2S}) ->
    Reply = dict:find(Key, K2S), % {ok, Value} or error
    {reply, Reply, State}.



% Server is dead, unregister it.
% Delete pid from the dict. FromPid is a pid of ux_unidata_store server.
handle_info({'DOWN', _Ref, process, ServerPid, _Reason},
            State = #state{key2server = K2S}) ->
    ?DBG("~w: Delete Pid = ~w from the dictionary. ~n", 
         [?MODULE, FromPid]),
    K2S_2   = dict:filter(fun(_K, V) -> V =/= ServerPid end, K2S),
    State_2 = State#state{key2server = K2S_2},
    {noreply, State_2}.


%%
%% API
%%
-spec set_source(Level::atom(), Parser::atom(), Types::[atom()] | all,
                 FileName::string()) -> ok.

%% @doc Set a data source (data file) with UNIDATA.
%%
%% 1. Runs server which parses file and returns a list of funs.
%% 2. Registers returned funs on the process, application or node level.
%%
%% Test me:
%% ux_unidata_filelist:set_source(process, blocks, all, code:priv_dir(ux) ++ "/UNIDATA/Blocks.txt").
%% ux_unidata_filelist:set_source(process, blocks, [blocks], code:priv_dir(ux) ++ "/UNIDATA/Blocks.txt").
%% F=ux_unidata_filelist:get_source(blocks, blocks).
%% {ok,S}=ux_unidata_filelist:file_owner(code:priv_dir(ux) ++ "/UNIDATA/Blocks.txt").
%% ux_unidata_store:remove_type(S, blocks).
%%
%% Returns nothing.

set_source('node', Parser, Types, FileName) ->
    Key = {Parser, Types, FileName},
    ux_unidata_server:set_default(Key);
    
set_source(Level, Parser, Types, FileName) ->
    Key = {Parser, Types, FileName},
    ClientPid = case Level of
        'process' -> 
            self();
        'application' ->
            % We unload data, when the application die.
            {ok, AppName1} = application:get_application(), % let it crash
            get_application_pid(AppName1)
    end,
    Key = {Parser, Types, FileName},
    Funs = lists:map(fun({Type, Ets, Fun}) ->
        %% This function allows to extract data from different data sources:
        %% * ETS tables;
        %% * dinamicly compilled modules.
        DataSourceFun = fun
            %% Run check only once.
            %% For fast realizations of filters.
            ('skip_check') ->

                case ets:info(Ets, 'owner') of
                undefined -> 
                   set_source(process, Parser, [Type], FileName),
                   NewFun = get_source(Parser, Type);
                _ -> Fun
                end;

            %% For ux_unidata_server. Check ETS before return value.
            ('test') ->
                case ets:info(Ets, 'owner') of
                'undefined' -> 
                    false;
                _ -> true
                end;

            %% Get an ETS table (need for CLDR).
            ('get_table') ->
                case ets:info(Ets, 'owner') of
                'undefined' -> 
                    set_source('node', Parser, [Type], FileName),
                    NewFun = get_source(Parser, Type),
                    NewFun('get_table');
                _ -> 
                    Ets
                end;

            ('reload') ->
                case ets:info(Ets, 'owner') of
                'undefined' -> 
                    set_source('node', Parser, [Type], FileName),
                    NewFun = get_source(Parser, Type),
                    ok;
                _ -> ok
                end;

            %% Check an ETS table and run function.
            (Val) ->
                try
                    Fun(Val)
                catch
                error:badarg -> 
                    case ets:info(Ets) of
                    'undefined' -> 
                        set_source(Level, Parser, [Type], FileName),
                        NewFun = get_source(Parser, Type),
                        NewFun(Val) 
                    end
                end 
            end, %% of DataSourceFun

        % Set an upgrade trigger.
        {{Parser, Type}, DataSourceFun}
        
        end, get_funs(Key, ClientPid)),

    ?DBG("~w: Loaded funs: ~w. ~n", 
        [?MODULE, Funs]),

    case Level of
    'process' -> 
        % Put to the process dictionary.
        set_proc_dict(Funs);
    'application' ->
        {ok, AppName2} = application:get_application(), 
        set_app_env(AppName2, Funs)
    end,
    ok.


%% This is a short form of function.
set_source(Level, {Parser, Types, Filename} = _Key) -> 
    set_source(Level, Parser, Types, Filename).

%% @doc Return registred fun.
%% Check: the dict of client process, then application enviroment, 
%% then try get the default value from the server.
get_source(Parser, Type) ->
    Value = {Parser, Type},
    get_source(Value).


-spec get_source({Parser::atom(), Type::atom()}) -> fun() | undefined.

%% @doc Try retrieve the information about the data source:
%% Step 1: Check process dictionary.
%% Step 2: Check application enviroment.
%% Step 3: Use defaults.
get_source(Value) ->
    case get_source_from(process, Value) of            % step 1
    'undefined' -> 
        case get_source_from(application, Value) of    % step 2
        'undefined' -> get_source_from('node', Value); % step 3
        Fun -> Fun
        end;
    Fun -> Fun
    end.


%%
%% Inter-module exports
%%

get_source_from('process', Value) ->
    erlang:get(Value);
get_source_from('application', Value) ->
    application:get_env(Value);
get_source_from('node', Value) ->
    case erlang:whereis(ux_unidata_server) of
    undefined ->
        ux:start(),
        ux_unidata_server:get_default(Value);
    _ -> 
        ux_unidata_server:get_default(Value) 
    end.
    
    
    
%% Return the list of functions from the server.
-spec get_funs(Key::{Parser::atom(), Types::[atom()], FileName::string()},
    pid()) -> [{Type::atom(), Ets::integer(), fun()}].
get_funs({_,Types,_} = Key, ClientPid) ->
    ServerPid = get_pid(Key, ClientPid),
    ux_unidata_store:get_funs(ServerPid, Types).

set_app_env(Name, [{Key, Val}|Tail]) ->
    ?DBG("~w: Set a application enviroment variable ~w::~w to ~w. ~n", 
        [?MODULE, Name, Key, Val]),
    application:set_env(Name, Key, Val),
    set_app_env(Name, Tail);
set_app_env(_Name, []) -> ok.
    
    
set_proc_dict([{Key, Val}|Tail]) ->
    ?DBG("~w: Put the value to the process dictionary: ~w::~w to ~w. ~n", 
        [?MODULE, self(), Key, Val]),
    erlang:put(Key, Val),
    set_proc_dict(Tail);
set_proc_dict([]) -> ok.


%% Convert the name of the application to its pid.
get_application_pid(Name) ->
    AInfo = application:info(),
    {'running', R} = lists:keyfind('running', 1, AInfo),
    {Name, Pid} = lists:keyfind(Name, 1, R).


%% Try to get a pid of the owner of an ETS table with the UNIDATA 
%% from the Key-file.
%% Also, try to monitor ClientPid on the server.
get_pid(Key, ClientPid) when is_pid(ClientPid) ->
    case file_owner(key_to_id(Key)) of
        error -> 
            {ok, StoreServerPid} = ux_unidata_store_sup:read_file(Key, ClientPid),
            StoreServerPid;
            
        % Server is already running.
        {ok, StoreServerPid} when is_pid(StoreServerPid) -> 
            ux_unidata_store:monitor_client_process(StoreServerPid, ClientPid), 
            ux_unidata_store:check_types(StoreServerPid, key_to_types(Key)),
            StoreServerPid
    end.

%% @private
%% Returns a pid of the server which serves a file with UNIDATA.
%% Each file has an own owner.
file_owner(FileName) ->
    gen_server:call(?MODULE, {get_pid, FileName}).

%% Used only by ux_unidata_store:init/1. 
%% Don't use this function from user code.
%% Throws {badmatch,{error,key_already_registred}} if self() is already 
%% registred.
reg_pid(Key, StoreServerPid) when is_pid(StoreServerPid) ->
    ok = gen_server:call(?MODULE, {reg_pid, key_to_id(Key), StoreServerPid}).

%%
%% Private helper functions.
%%

key_to_id({Parser, _Types, FileName} = Key) ->
    % Get client enviroment:
    Env = ux_unidata_store:get_env(Key),
    {Parser, FileName, Env}.

key_to_types({_Parser, Types, _FileName}) ->
    Types.
