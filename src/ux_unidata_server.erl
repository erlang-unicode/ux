-module(ux_unidata_server).

-export([start_link/0]).
-export([init/1, terminate/2, 
    handle_call/3, handle_info/2, handle_cast/2]).
-export([set_default/1, get_default/1]).

% spawn export.
-export([spawn_waiter/1]).

-behavior(gen_server).

%% Exported Client Functions
%% Operation & Maintenance API
start_link() ->
    Arguments = [],
    Opts = [],
    gen_server:start_link({local, ?MODULE}, ?MODULE, Arguments, Opts).

init([]) ->
    error_logger:info_msg(
        "~w~w: All defaults types were generated. ~n",
        [?MODULE, self()]),
    {ok, []}.

terminate(_Reason, _LoopData) ->
    ok.

%% Spawns process which waits result from ux_unidata_store.
spawn_waiter(Key) ->
    ok = load_default(Key),
    Reply = ux_unidata_filelist:get_source_from(process, Key),
    % Reply to ux_unidata_server.
    gen_server:cast(?MODULE, {waiter_reply, Key}),
    % Reply to clients.
    spawn_waiter_reply(Reply).

spawn_waiter_reply(ReplyVal) ->
    receive
    {reply_to, Pid} ->
        Pid ! {reply, ReplyVal},
        spawn_waiter_reply(ReplyVal)
    after 5000 ->
        ok
    end.

%% Runs from a client process.
wait_respond(WaiterPid) ->
    WaiterPid ! {reply_to, self()},
    {ok, Result} = 
        receive
        {reply, Val} -> {ok, Val}
        after 20000 -> 
            {error, timeout}
        end,
    Result.
    
check_key(Key) ->
    case erlang:get(Key) of
    Pid when is_pid(Pid) ->
        case erlang:is_process_alive(Pid) of
        true -> ok; % still working.
        false ->
        % a spawn_waiter process failed and dead.
        erlang:erase(Key),
        ok
        end;
    Fun when is_function(Fun) -> ok
    end.

% Ref stores Key.
% Key stores Pid of a waiter or Fun.
% Delete pid from dict. FromPid is a pid of ux_unidata_store server.
handle_info({'DOWN', Ref, process, FromPid, _Reason}, LoopData) ->
    error_logger:info_msg(
        "~w: Delete Pid = ~w from the process dictionary. ~n", 
        [?MODULE, FromPid]),

    case erlang:get(Ref) of
    undefined -> ok;
    Key -> erlang:erase(Ref), check_key(Key)
    end,
    {noreply, LoopData}.

handle_cast({waiter_reply, Key}, LoopData) ->
    ok = load_default(Key),
    {noreply, LoopData}.

handle_call({get_default, Key}, _From, LoopData) ->
    case ux_unidata_filelist:get_source_from(process, Key) of
    undefined -> 
        {WaiterPid, Ref} = spawn_monitor(?MODULE, spawn_waiter, [Key]),
        put(Key, WaiterPid),
        put(Ref, Key),
        Reply = WaiterPid,
        {reply, Reply, LoopData};
    Reply -> 
        {reply, Reply, LoopData}
    end;
handle_call({set_default, Key}, _From, LoopData) ->
    Reply = ux_unidata_filelist:set_source(process, Key),
    {reply, Reply, LoopData}.

%% If cannot load data from a default sources, then return undefined.
get_default(Key) ->
    Reply = gen_server:call(?MODULE, {get_default, Key}, 60000),
    case Reply of
    Fun when is_function(Fun) -> Fun;
    WaiterPid when is_pid(WaiterPid) ->
        wait_respond(WaiterPid)
    end.

set_default(Key) ->
    gen_server:call(?MODULE, {set_default, Key}, 60000).


%%
%% Private
%% 
load_default({Parser, Type} = _Key) ->
    FileName = ux_unidata:get_source_file(Parser),
%   Types = [Type],
    Types = all,
    ux_unidata_filelist:set_source(process, Parser, Types, FileName).

