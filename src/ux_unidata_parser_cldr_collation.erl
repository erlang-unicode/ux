-module(ux_unidata_parser_cldr_collation).

-export([bootstrap/1, init/2]).


%% This function will be runned in the client code.
bootstrap(_File) ->
    Env = [{'ducet', ux_unidata_filelist:get_source('allkeys', 'allkeys')}],
    Env.

%% This function will be runned in the store code.
init(_File, Env) ->
    DFun = lists:keyfind('ducet', 1, Env),
    DTbl = DFun('get_table'),
    
    ok.
