%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc ux.
%% @private

-module(ux).
-author("Mochi Media <dev@mochimedia.com>").
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.


%% @spec start() -> ok
%% @doc Start the ux server.
start() ->
    ux_deps:ensure(),
    ensure_started(crypto),
    application:start(ux).


%% @spec stop() -> ok
%% @doc Stop the ux server.
stop() ->
    application:stop(ux).
