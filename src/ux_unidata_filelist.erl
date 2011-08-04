%%% Key-value store for a list of unidata files.
-module(ux_unidata_filelist).

% OTP 
-export([start_link/0]).
-export([init/1, terminate/2, handle_call/3, handle_info/2]).

% Inter-module exports
-export([reg_pid/2, file_owner/1]).

% Unidata API
-export([set_source/4, set_source/2, 
    get_source/2, get_source/1, get_source_from/2]).

-behavior(gen_server).

%% Exported Client Functions
%% Operation & Maintenance API
start_link() ->
    Arguments = [],
    Opts = [],
    gen_server:start_link({local, ?MODULE}, ?MODULE, Arguments, Opts).

init([]) ->
    Dict = dict:new(),
    LoopData = {Dict},
    {ok, LoopData}.

terminate(_Reason, _LoopData) ->
    ok.


% Save pid of ux_unidata_store server in a dict.
% Key is a combination of filename and fileoptions.
handle_call({reg_pid, Key, StorePid}, _From, {Dict} = _LoopData) ->
    erlang:monitor(process, StorePid),
    error_logger:info_msg(
        "~w: Registrate new Pid = ~w with Key = ~w. ~n", 
        [?MODULE, StorePid, Key]),
    
    {Reply, NewDict} = case dict:is_key(Key, Dict) of
        false -> {ok, dict:store(Key, StorePid, Dict)};
        true  -> error_logger:error_msg("~w: Key ~w is already registred. ~w",
                [?MODULE, Key]),
            {{error, key_already_registred}, Dict}
    end,
    {reply, Reply, {NewDict}};
handle_call({get_pid, Key}, _From, {Dict} = _LoopData) ->
    
    Reply = dict:find(Key, Dict), % {ok, Value} or error
    {reply, Reply, {Dict}}.

% Delete pid from dict. FromPid is a pid of ux_unidata_store server.
handle_info({'DOWN', _Ref, process, FromPid, _Reason}, {Dict} = _LoopData) ->
    error_logger:info_msg(
        "~w: Delete Pid = ~w from dictionary. ~n", 
        [?MODULE, FromPid]),
    NewDict = dict:filter(fun(_K, V) -> V =/= FromPid end, Dict),
    {noreply, {NewDict}}.

%%
%% API
%%
-spec set_source(Level::atom(), Parser::atom(), Types::[atom()] | all,
    FileName::string()) -> ok.

%% Test me:
%% ux_unidata_filelist:set_source(process, blocks, all, code:priv_dir(ux) ++ "/UNIDATA/Blocks.txt").
%% ux_unidata_filelist:set_source(process, blocks, [blocks], code:priv_dir(ux) ++ "/UNIDATA/Blocks.txt").
%% F=ux_unidata_filelist:get_source(blocks, blocks).
%% {ok,S}=ux_unidata_filelist:file_owner(code:priv_dir(ux) ++ "/UNIDATA/Blocks.txt").
%% ux_unidata_store:remove_type(S, blocks).
set_source(node, Parser, Types, FileName) ->
    Key = {Parser, Types, FileName},
    ux_unidata_server:set_default(Key);
    
set_source(Level, Parser, Types, FileName) ->
    Key = {Parser, Types, FileName},
    ClientPid = case Level of
        process -> 
            self();
        application ->
            {ok, AppName1} = application:get_application(), % let it crash
            get_application_pid(AppName1)
    end,
    Key = {Parser, Types, FileName},
    Funs = lists:map(fun({Type, Ets, Fun}) ->
        % Set upgrade trigger.
        {{Parser, Type}, fun(Val) ->
            case ets:info(Ets) of
            undefined -> 
                set_source(Level, Parser, [Type], FileName),
                NewFun = get_source(Parser, Type),
                NewFun(Val);
            _ -> Fun(Val)
            end
        end}
        
        end, get_funs(Key, ClientPid)),

    error_logger:info_msg(
        "~w: Loaded funs: ~w. ~n", 
        [?MODULE, Funs]),

    case Level of
    process -> 
        set_proc_dict(Funs);
    application ->
        {ok, AppName2} = application:get_application(), 
        set_app_env(AppName2, Funs)
    end.

set_source(Level, {Parser, Types, Filename} = _Key) -> 
    set_source(Level, Parser, Types, Filename).

get_source(Parser, Type) ->
    Value = {Parser, Type},
    get_source(Value).

-spec get_source({Parser::atom(), Type::atom()}) -> fun() | undefined.
-ifdef(TEST).
get_source({Parser, Type} = Value) ->
    case get_source_from(process, Value) of
    undefined -> 
        Key = {Parser, all, ux_unidata:get_source_file(Parser)},
        % Example:
        % ux_unidata_store:start_link({unidata, [ccc], code:priv_dir(ux) ++ "/UNIDATA/UnicodeData.txt"},self()).
        {ok, ServerPid} = ux_unidata_store:start_link(Key, self()),
        Funs = lists:map(fun({Type, Ets, Fun}) ->
                {{Parser, Type}, fun(Val) -> Fun(Val) end}
            end, ux_unidata_store:get_funs(ServerPid, all)),
        set_proc_dict(Funs),
        get_source_from(process, Value);
    Fun -> Fun
    end.
-else.
get_source(Value) ->
    % Step 1: Check process dictionary:
    case get_source_from(process, Value) of
    undefined -> 
        % Step 2: Check application enviroment:
        case get_source_from(application, Value) of
        % Step 3: Use defaults:
        undefined -> get_source_from(node, Value);
        Fun -> Fun
        end;
    Fun -> Fun
    end.
-endif.

get_source_from(process, Value) ->
    erlang:get(Value);
get_source_from(application, Value) ->
    application:get_env(Value);
get_source_from(node, Value) ->
    ux_unidata_server:get_default(Value).
    
    
-spec get_funs(Key::{Parser::atom(), Types::[atom()], FileName::string()},
    pid()) -> [{Type::atom(), Ets::integer(), fun()}].
get_funs({_,Types,_} = Key, ClientPid) ->
    ServerPid = get_pid(Key, ClientPid),
    ux_unidata_store:get_funs(ServerPid, Types).

set_app_env(Name, [{Key, Val}|Tail]) ->
    error_logger:info_msg(
        "~w: Set app ``~w`` env: ~w = ~w. ~n", 
        [?MODULE, Name, Key, Val]),
    application:set_env(Name, Key, Val),
    set_app_env(Name, Tail);
set_app_env(_Name, []) -> ok.
    
    
set_proc_dict([{Key, Val}|Tail]) ->
    error_logger:info_msg(
        "~w: Put to proc ``~w`` dictionary: ~w = ~w. ~n", 
        [?MODULE, self(), Key, Val]),
    erlang:put(Key, Val),
    set_proc_dict(Tail);
set_proc_dict([]) -> ok.



get_application_pid(Name) ->
    {running, R} = lists:keyfind(running, 1, application:info()),
    {Name, Pid} = lists:keyfind(Name, 1, R).


%%
%% Inter-module exports
%%

%% Try to get a pid of the owner of an ETS table with UNIDATA from  the Key-file.
%% Also, try to monitor ClientPid on the server.
get_pid(Key, ClientPid) when is_pid(ClientPid) ->
    case file_owner(key_to_filename(Key)) of
        error -> 
            {ok, StoreServerPid} = ux_unidata_store_sup:read_file(Key, ClientPid),
            StoreServerPid;
            
        % Server is already running.
        {ok, StoreServerPid} when is_pid(StoreServerPid) -> 
            ux_unidata_store:monitor_client_process(StoreServerPid, ClientPid), 
            ux_unidata_store:check_types(StoreServerPid, key_to_types(Key)),
            StoreServerPid
    end.

% @private
file_owner(FileName) ->
    gen_server:call(?MODULE, {get_pid, FileName}).

%% Used only by ux_unidata_store:init/1. 
%% Don't use this function from user code.
%% Throws {badmatch,{error,key_already_registred}} if self() is already 
%% registred.
-ifdef(TEST).
reg_pid(Key, StoreServerPid) when is_pid(StoreServerPid) ->
    ok.
-else.
reg_pid(Key, StoreServerPid) when is_pid(StoreServerPid) ->
    ok = gen_server:call(?MODULE, {reg_pid, key_to_filename(Key), StoreServerPid}).
-endif.

%%
%% Private helper functions.
%%

key_to_filename({_Parser, _Types, FileName}) ->
    FileName.
key_to_types({_Parser, Types, _FileName}) ->
    Types.
