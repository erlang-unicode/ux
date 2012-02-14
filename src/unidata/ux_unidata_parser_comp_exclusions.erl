%%% CompositionExclusions.txt
%%% @private
-module(ux_unidata_parser_comp_exclusions).
-export([parse/1, types/0, get_function/2]).
%%% Example:
%%% ux_unidata_filelist:get_pid({comp_exclusions, [comp_exclusions], code:priv_dir(ux) ++ "/UNIDATA/CompositionExclusions.txt"}).

types() ->
    [is_exclusion].

parse(In) ->
    case ux_unidata_parser:delete_spaces(In) of
    [] -> skip;
    [_|_] = Code -> {ok, 
        [{is_exclusion, {ux_unidata_parser:hex_to_int(Code)}}]
        }
    end.

get_function(is_exclusion, Table) -> 
    ux_unidata_parser:bool_fun(Table).
