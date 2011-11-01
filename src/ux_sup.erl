%% @doc Supervisor for the ux application.
%% @private

-module(ux_sup).

-behaviour(supervisor).

%% External exports
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init([]) ->
    % Control unidata stores.
    StoreSup = {ux_unidata_store_sup, 
        {ux_unidata_store_sup, start_link, []},
        permanent, infinity, supervisor, [ux_unidata_store_sup]},

    % Provide information about unidata stores.
    FileListWorker = {ux_unidata_filelist, 
        {ux_unidata_filelist, start_link, []},
        permanent, 2000, worker, [ux_unidata_filelist]},

    % Provide a global dictionary for this node.
    DefaultUnidataWorker = {ux_unidata_server, 
        {ux_unidata_server, start_link, []},
        permanent, 10000, worker, [ux_unidata_server]},

    % Can add functions to a module on the fly
    MetaWorker = {ux_unidata_meta, 
        {metamodule, start_link, [ux_unidata_meta]},
        permanent, 10000, worker, [metamodule]},

    Strategy = {one_for_one, 10, 10},
    {ok, {Strategy, [StoreSup, FileListWorker, DefaultUnidataWorker, MetaWorker]}}.

