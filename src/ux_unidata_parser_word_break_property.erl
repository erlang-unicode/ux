%%% Blocks.txt
%%% @private
-module(ux_unidata_parser_word_break_property).
-export([parse/1, types/0, get_function/2]).
%       after_parse/1]).

-define(TO_INT(C), ux_unidata_parser:hex_to_int(C)).

types() ->
    ['word_break_property'
    ].

parse(In) ->
    case In of
    [] -> 'skip';
    Data -> 

        case ux_unidata_parser:split($;, Data) of
        [Code, Name] ->
        Atom = list_to_atom(string:strip(Name)),
        ParsedKey = parse_code(string:strip(Code)),
        {ok, 
        [{'word_break_property',  {ParsedKey, Atom}}
        ]};
        _Skip -> 'skip'
        end
    end.


get_function('word_break_property', Table) ->
    DefValue = 'Any',
    ux_unidata_parser:expand_opt_fun(Table, DefValue).

%%
%% Helpers
%%
parse_code(Code) -> case string:tokens(Code, "..") of
    [From, To]  -> {?TO_INT(From), ?TO_INT(To)};
    [Code]      -> ?TO_INT(Code)
    end.

