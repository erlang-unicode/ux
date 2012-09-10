%% @author Uvarov Michael <arcusfelis@gmail.com>
%% @copyright 2010 ux Uvarov Michael <arcusfelis@gmail.com>

%% @doc ux.
%% @private

-module(ux).
-author("Uvarov Michael <arcusfelis@gmail.com>").
-export([start/0, stop/0]).

%% @spec start() -> ok
%% @doc Start the ux server.
start() ->
    ux_deps:ensure(),
    application:start(ux).


%% @spec stop() -> ok
%% @doc Stop the ux server.
stop() ->
    application:stop(ux).
