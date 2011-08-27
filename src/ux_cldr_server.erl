%%% @private
-module(ux_cldr_server).
-include("ux.hrl").

-export([start_link/0]).
-export([init/1, terminate/2, 
    handle_call/3]).
-export([set_default/2, get_default/1]).

-behavior(gen_server).


%% Exported Client Functions
%% Operation & Maintenance API
start_link() ->
    Arguments = [],
    Opts = [],
    gen_server:start_link({local, ?MODULE}, ?MODULE, Arguments, Opts).

init([]) ->
    put('locale', 'root'),
    {ok, []}.

terminate(_Reason, _LoopData) ->
    ok.



handle_call({get_default, Key}, From, LoopData) ->
    Reply = ux_cldr:get_value('process', Key),
    {reply, Reply, LoopData};

handle_call({set_default, Key, Value}, _From, LoopData) ->
    Reply = ux_cldr:set_value('process', Key, Value),
    {reply, Reply, LoopData}.

%%
%% API
%%

get_default(Key) ->
    gen_server:call(?MODULE, {get_default, Key}).

set_default(Key, Value) ->
    gen_server:call(?MODULE, {set_default, Key, Value}).

