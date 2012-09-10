% vim: set filetype=erlang shiftwidth=4 tabstop=4 expandtab tw=80:
%% @author Uvarov Michael <arcusfelis@gmail.com>
%% @copyright 2010 ux Uvarov Michael <arcusfelis@gmail.com>

%% @doc Callbacks for the web_col application.
%% @private

-module(ux_app).
-author("Mochi Media <dev@mochimedia.com>").

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for web_col.
start(_Type, _StartArgs) ->
    ux_deps:ensure(),
    ux_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for web_col.
stop(_State) ->
    ok.
