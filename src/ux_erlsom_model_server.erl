-module(ux_erlsom_model_server).

-behaviour(gen_server).

-export([start_link/0, get_model/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, []}.

get_model(Name) ->
    {ok, M} = gen_server:call(?MODULE, {get_model, Name}),
    M.

%% callbacks
handle_call({get_model, Name}, _From, State) ->
    Reply = case get(Name) of
    undefined ->
        Filename = ux_unidata:get_xsd_file(Name),
        {ok, Model} = erlsom:compile_xsd_file(Filename),
        put(Name, Model),
        Model;
    Model -> Model
    end,
    {reply, {ok, Reply}, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



