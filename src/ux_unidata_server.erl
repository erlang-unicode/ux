-module(ux_unidata_server).

-export([start_link/0]).
-export([init/1, terminate/2, handle_call/3]).
-export([set_default/1, get_default/1]).

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

handle_call({get_default, Key}, _From, LoopData) ->
    case ux_unidata_filelist:get_source_from(process, Key) of
    undefined -> 
        ok = load_default(Key),
        Reply = ux_unidata_filelist:get_source_from(process, Key),
        {reply, Reply, LoopData};
    Reply -> 
        {reply, Reply, LoopData}
    end;
handle_call({set_default, Key}, _From, LoopData) ->
    Reply = ux_unidata_filelist:set_source(process, Key),
    {reply, Reply, LoopData}.

%% If cannot load data from a default sources, then return undefined.
get_default(Key) ->
    gen_server:call(?MODULE, {get_default, Key}).
set_default(Key) ->
    gen_server:call(?MODULE, {set_default, Key}).


%%
%% Private
%% 
load_default({Parser, Type} = _Key) ->
    FileName = ux_unidata:get_source_file(Parser),
    Types = [Type],
    ux_unidata_filelist:set_source(process, Parser, Types, FileName).

