% TODO: write or delete me.
-module(ux_unidata_parser_cldr_collation).

-export([bootstrap/1, init/2, get_function/2]).

-export([format/0]).

-export([types/0]).


types() ->
    [rules,
     settings
    ].

format() ->
    'manual'.

%% This function will be runned in the client code.
bootstrap(_File) ->
    Env = ux_cldr:get_env(),
    Env.

%% This function will be runned in the store code.
init(_File, Env) ->
    ux_cldr:set_env(Env),
    ok.


parse(ZipName, Tables) ->
    ok.
    



    
get_function('rules', Table) ->
    ok;
get_function('settings', Table) ->
    ok.
