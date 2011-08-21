%%% Blocks.txt
%%% @private
-module(ux_unidata_parser_scripts).
-export([parse/1, types/0, get_function/2]).

-define(TO_INT(C), ux_unidata_parser:hex_to_int(C)).
-define(CLEAN(S), ux_unidata_parser:delete_spaces(S)).

types() ->
    [script
    ].

parse(In) ->
    case In of
    [] -> skip;
    Data -> 

        case ux_unidata_parser:split($;, Data) of
        [Code, Name] ->
        Atom = list_to_atom(?CLEAN(Name)),
        ParsedKey = parse_code(?CLEAN(Code)),
        {ok, 
        [{script,  {ParsedKey, Atom}}
        ]};
        _Skip -> skip
        end
    end.


get_function(script, Table) ->
    DefValue = 'Unknown',
    ux_unidata_parser:expand_fun(Table, DefValue).

%%
%% Helpers
%%
parse_code(Code) -> case string:tokens(Code, "..") of
    [From, To]  -> {?TO_INT(From), ?TO_INT(To)};
    [Code]      -> ?TO_INT(Code)
    end.

