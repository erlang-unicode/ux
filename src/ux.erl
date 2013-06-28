%% @author Uvarov Michael <arcusfelis@gmail.com>
%% @copyright 2010 ux Uvarov Michael <arcusfelis@gmail.com>

%% @doc ux.
%% @private

-module(ux).
-author("Uvarov Michael <arcusfelis@gmail.com>").
-export([start/0, stop/0]).

-define(APP, ux).

%% @spec start() -> ok
%% @doc Start the ux server.
start() ->
    application:load(?APP),
    {ok, Deps} = application:get_key(?APP, applications),
    true = lists:all(fun ensure_started/1, Deps),
    ux_deps:ensure(),
    application:start(ux).


%% @spec stop() -> ok
%% @doc Stop the ux server.
stop() ->
    application:stop(ux).


ensure_started(App) ->
    case application:start(App) of
        ok ->
            true;
        {error, {already_started, App}} ->
            true;
        Else ->
            error_logger:error_msg("Couldn't start ~p: ~p", [App, Else]),
            Else
    end.

