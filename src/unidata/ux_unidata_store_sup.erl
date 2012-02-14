%%% @private
-module(ux_unidata_store_sup).
-behavior(supervisor).
-export([start_link/0]).
-export([init/1]).
-export([read_file/2]).
-export([restart/0]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    ChildSpec = {ux_unidata_store, 
                {ux_unidata_store, start_link, []},
        temporary, 2000, worker, [ux_unidata_store]},
    {ok, {{simple_one_for_one,0,1}, [ChildSpec]}}.

%% @doc Read file with UNIDATA.
%%      Filename is {parser, types, filename}
read_file({_,_,_} = Filename, ClientPid) when is_pid(ClientPid) ->
    SupervisorName = ?MODULE,
    Ret = supervisor:start_child(SupervisorName, [Filename, ClientPid]),
    {ok, ServerPid} = Ret.

    
%% @doc Restart this supervisor.
restart() ->
    exit(whereis('ux_unidata_store_sup'), 'kill').

