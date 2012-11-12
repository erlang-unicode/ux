%%% @private
-module(ux_unidata_store).
-include("ux.hrl").

-export([start_link/2]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2]).
-export([monitor_client_process/2, get_funs/2]).

% First argument is a server pid.
-export([check_types/2, table_list/1, remove_type/2]).

% This functions are not connected to process directly, 
% but called from different processes.
-export([get_env/1]).

-behavior(gen_server).
-record(state, {
    clients = [] :: [pid()],
    ets = [] :: [{atom(), integer()}],
    funs = [] :: [{atom(), fun()}],
    parser :: atom(),
    types = all :: atom() | [atom()],
    remain = [] :: [atom()],
    filename :: string()
}).

%% Exported Client Functions
%% Operation & Maintenance API
start_link(File, ClientPid) ->
    % Check a parser module and types.
    ok = ux_unidata_parser:check(File),

    % We in the client code
    % Move some env variables from the client dictionary to the store dicntionary.
    ClientEnv = get_env(File),
    Arguments = [File, ClientPid, ClientEnv],
    Opts = [],
    Ret = gen_server:start_link(?MODULE, Arguments, Opts).


init([{ParserType, Types, FileName} = File, ClientPid, ClientEnv]) ->
    % We in the process code: extract client data.

    set_env(File, ClientEnv),

    % Registrate pid of this server.
    % PS: reg_pid uses env.
    ux_unidata_filelist:reg_pid(File, self()),

    set_monitor(ClientPid),

    % Escape deadlocks.
    ok = gen_server:cast(self(), {run_parser, File}),

    LoopData = #state{
        clients = [ClientPid]
        },
    {ok, LoopData}.

terminate(_Reason, _LoopData) ->
    ok.


handle_call({check_types, _Types}, _From, 
    #state{remain=[]} = LoopData) ->
    ?DBG("~w~w: All types were already generated. ~n", 
        [?MODULE, self()]),
    Reply = ok,
    {reply, Reply, LoopData};
% Generate remains.
handle_call({check_types, all}, From, 
    #state{remain=Types} = LoopData) ->
    handle_call({check_types, Types}, From, LoopData);
handle_call({check_types, Types}, _From, 
    #state{
        types=RegistredTypes, 
        parser=ParserType, 
        filename=FileName,
        remain=RemTypes,
        funs=Funs,
        ets=Ets} = LoopData) ->
    NewRemTypes = RemTypes -- Types,
    {Reply, NewLoopData} = 
        case NewRemTypes == RemTypes of
        true  -> 
            ?DBG("~w~w: Types were already generated. ~n", 
                [?MODULE, self()]),
            {ok, LoopData};
        false ->
            AddTypes = Types -- RegistredTypes,
            File = {ParserType, AddTypes, FileName},
            {ok, AddedEts, _RemTypesWithRegistred} = 
                ux_unidata_parser:run(File),
            AddedFuns = ux_unidata_parser:get_functions(ParserType, AddedEts),
            {ok, LoopData#state{
                    types=AddTypes ++ RegistredTypes,
                    ets=AddedEts ++ Ets,
                    remain=NewRemTypes,
                    funs=AddedFuns ++ Funs
                }}
        end,    
    {reply, Reply, NewLoopData};

handle_call({monitor_client_pid, ClientPid}, _From, 
    #state{clients=Clients} = LoopData) ->
    NewLoopData = case lists:member(ClientPid, Clients) of
        true -> LoopData;
        false ->
            set_monitor(ClientPid),
            NewClients = [ClientPid | Clients],
            LoopData#state{clients=NewClients}
        end,
    Reply = ok,
    {reply, Reply, NewLoopData};


handle_call({get_funs, all}, _From, 
    #state{funs=Funs} = LoopData) ->
    Reply = Funs,
    {reply, Reply, LoopData};
handle_call({get_funs, Types}, _From, 
    #state{funs=Funs} = LoopData) ->
    ?DBG("~w~w: Try get the list of the functions: ~w. ~n", 
        [?MODULE, self(), Funs]),
    Reply = get_elems(Types, Funs),
    {reply, Reply, LoopData};

handle_call(table_list, _From, 
    #state{ets=Ets} = LoopData) ->
    Reply = {ok, Ets},
    {reply, Reply, LoopData}.

handle_info({'DOWN', _Ref, process, FromPid, _Reason}, 
    #state{clients=Clients} = LoopData) ->
    ?DBG("~w~w: Delete the process ~w from the process list: ~w. ",
        [?MODULE, self(), FromPid, Clients]),
    NewClients = Clients -- [FromPid],
    case NewClients of
        [] -> % wait 15 second and stop server. 
            Timeout = 15000,
            ?DBG("~w~w: Nobody use this server and ETS table. "
                    "Wait ~w ms and stop. ~n", 
                [?MODULE, self(), Timeout]),
            
            timer:send_after(Timeout, self(), delete_timeout),
            ok;
        _ -> ok
    end,
    {noreply, LoopData#state{clients=NewClients}};
handle_info(delete_timeout, State=#state{clients=[]}) ->
    ?DBG("~w~w: Nobody use this server and ETS table. Stop. ~n", 
        [?MODULE, self()]),
    {stop, normal, State};
% We have new clients.
handle_info(delete_timeout, LoopData) ->
    ?DBG("~w~w: New users use this server. Cancel stop. ~n", 
        [?MODULE, self()]),
    {noreply, LoopData}.


handle_cast({run_parser, {ParserType, Types, FileName} = File},
    #state{ets=[]} = LoopData) ->
    % Run parser.
    {ok, Ets, RemTypes} = ux_unidata_parser:run(File),
    Funs = ux_unidata_parser:get_functions(ParserType, Ets),
    ?DBG("~w~w: Init. Parser ~w generated ets: ~w and funs: ~w. ~n", 
        [?MODULE, self(), ParserType, Ets, Funs]),
    NewLoopData = LoopData#state{
        ets    = Ets,
        types  = Types,
        parser = ParserType,
        filename = FileName,
        remain = RemTypes,
        funs   = Funs
        },
    {noreply, NewLoopData};

handle_cast({remove_type, Type}, 
    #state{ets=Ets,
        types=Types,
        remain=Remain,
        funs=Funs} = LoopData) ->
    NewLoopData = case lists:keyfind(Type, 1, Ets) of
        false -> LoopData;
        {Type, Table} -> 
            true = ets:delete(Table),
            NewEts = lists:keydelete(Type, 1, Ets),
            NewFuns = lists:keydelete(Type, 1, Funs),
            NewTypes = lists:delete(Type, Types),
            NewRemain = [Type | Remain],
            LoopData#state{ets=NewEts,
                types=NewTypes,
                remain=NewRemain,
                funs=NewFuns}
        end,
    {noreply, NewLoopData}.

%% Monitor a proccess which called this function.
%% ServerPid is a pid of gen_server with ETS.
%% If all clients die then gen_server dies.
%% This function is called by fun ux_unidata_filelist:get_pid/2.
monitor_client_process(ServerPid, ClientPid) ->
    ok = gen_server:call(ServerPid, {monitor_client_pid, ClientPid}).

%% If all types are not on the server then try to generate them.
check_types(ServerPid, Types) ->
    ok = gen_server:call(ServerPid, {check_types, Types}).

remove_type(ServerPid, Type) ->
    ok = gen_server:cast(ServerPid, {remove_type, Type}).

table_list(ServerPid) ->
    {ok, TableList} = gen_server:call(ServerPid, table_list),
    TableList.

get_funs(ServerPid, Types) ->
    gen_server:call(ServerPid, {get_funs, Types}, 30000).



get_elems(Types, Elems) ->
    lists:reverse(do_get_elems(Types, Elems, [])).

do_get_elems([Type|Tail], Elems, Acc) ->
    El = lists:keyfind(Type, 1, Elems),
    true = El =/= false,
    do_get_elems(Tail, Elems, [El|Acc]);
do_get_elems([], _Elems, Acc) -> Acc.


set_monitor(ClientPid) ->
    ?DBG("~w~w: Set the monitor on the process ~w. ~n", 
        [?MODULE, self(), ClientPid]),
    erlang:monitor(process, ClientPid).
    



%%
%% Helpers
%%

get_env(File) ->
    ux_unidata_parser:get_env(File).

set_env(File, Env) ->
    ux_unidata_parser:set_env(File, Env).
