%%% Blocks.txt
-module(ux_unidata_parser_blocks).
-export([parse/1, types/0, get_function/2]).

%%% Example:
%%% ux_unidata_filelist:get_pid({blocks, all, code:priv_dir(ux) ++ "/UNIDATA/Blocks.txt"}).
-define(TO_INT(C), ux_unidata_parser:hex_to_int(C)).

types() ->
    [block
    ].

parse(In) ->
    case In of
    [] -> skip;
    Data -> 

        case ux_unidata_parser:split($;, Data) of
        [Code, [_|BlockName]] ->
        Atom = list_to_atom(BlockName),
        ParsedKey = parse_code(Code),
        {ok, 
        [{block,  {ParsedKey, Atom}}
        ]};
        _Skip -> skip
        end
    end.

%after_parse(Ets) ->
%    do_after(Ets),
%    ok.

%do_after([{_Name, Table} | Tail]) ->
%    ux_unidata_parser:expand_table(Table),
%    do_after(Tail);
%do_after([]) -> ok.


get_function(block, Table) ->
    DefValue = other,
    ux_unidata_parser:expand_fun(Table, DefValue).

%%
%% Helpers
%%
parse_code(Code) -> case string:tokens(Code, "..") of
    [From, To]  -> {?TO_INT(From), ?TO_INT(To)};
    [Code]      -> ?TO_INT(Code)
    end.

