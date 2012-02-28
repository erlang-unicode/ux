%%% Example:
%%% ux_unidata_filelist:get_pid({unidata, [ccc], code:priv_dir(ux) ++ "/UNIDATA/UnicodeData.txt"}).
%%% @private
%%% http://www.ksu.ru/eng/departments/ktk/test/perl/lib/unicode/UCDFF301.html
-module(ux_unidata_parser_unidata).
-export([parse/1, types/0, get_function/2]).

types() ->
    [to_upper
    ,to_lower
    ,is_upper
    ,is_lower
    ,type       % General Category
    ,is_compat
    ,decomp     % Character Decomposition Mapping
    ,comp_tag   % compatibility formatting tag
    ,comp   
    ,comment 
    ,ccc        % Canonical Combining Classes
    ,w3         % Tertiary Weight Assignments (Case or Kana Subtype)
    ].

%% w3
%% http://unicode.org/reports/tr10/#Tertiary_Weight_Table


parse(In) ->
    Tokens = ux_unidata_parser:split($;, In),
    [Code,Comment,Abbr,Ccc,_,DecompMap,_,_,_,_,_,_,UC,LC|_] = Tokens,
    Compat = case DecompMap of [$<|_] -> true; _ -> false end,
    Dec = case DecompMap of 
        [_|_] -> ux_unidata_parser:from_hex(DecompMap);
        _ -> []
        end,

    case ux_unidata_parser:hex_to_int(Code) of 
    false -> skip;
    Char -> 
        Excl = ux_unidata:is_comp_excl(Char),
        {ok, 
        [{to_upper, case ux_unidata_parser:hex_to_int(UC) of 
                        false -> skip; V -> {Char, V} end}
        ,{to_lower, case ux_unidata_parser:hex_to_int(LC) of
                        false -> skip; V -> {Char, V} end}
        ,{is_upper, case Abbr of "Lu" -> {Char}; _ -> skip end}
        ,{is_lower, case Abbr of "Ll" -> {Char}; _ -> skip end}

        ,{type,     case Abbr of
                        [] -> skip; V -> {Char, list_to_atom(V)} end}

        ,{is_compat, case Compat of true -> {Char}; false -> skip end}

        ,{decomp,  case Dec of 
                    [_|_] -> {Char, Dec}; _ -> skip end}

        ,{comp,    case Dec of 
                    [D1,D2] when (not Excl)   and is_integer(D1)
                             and (not Compat) and is_integer(D1)
                       -> {{D1, D2}, Char}; 
                    _  -> skip end}

        ,{w3, case parse_comment(Comment) of
            skip -> skip;
            W3Type -> {Char, W3Type} end}

        ,{comp_tag, case Compat of
            true -> {Char, parse_tag(DecompMap)};
            false -> skip end}

        ,{comment, case Comment of 
                   [] -> skip;
                   _  -> {Char, list_to_binary(Comment)} end}

        ,{ccc,     case string:to_integer(Ccc) of 
                   {Int, []} when Int>0 -> {Char, Int}; _ -> skip end}
        ]}
    end.
    
get_function(w3, Table) -> 
    DefValue = false,
    ux_unidata_parser:ets_fun(Table, DefValue);
get_function(comp_tag, Table) -> 
    DefValue = false,
    ux_unidata_parser:ets_fun(Table, DefValue);
get_function(ccc, Table) -> 
    DefValue = 0,
    ux_unidata_parser:ets_fun(Table, DefValue);
get_function(is_upper, Table) -> 
    ux_unidata_parser:bool_fun(Table);
get_function(is_lower, Table) -> 
    ux_unidata_parser:bool_fun(Table);
get_function(is_compat, Table) -> 
    ux_unidata_parser:bool_fun(Table);
get_function(type, Table) -> 
    DefValue = other,
    ux_unidata_parser:ets_fun(Table, DefValue);
get_function(comment, Table) -> 
    DefValue = <<>>,
    ux_unidata_parser:ets_fun(Table, DefValue);
get_function(comp, Table) -> 
    DefValue = false,
    ux_unidata_parser:ets_fun(Table, DefValue);
get_function(decomp, Table) -> 
    DefValue = [],
    ux_unidata_parser:ets_fun(Table, DefValue);
get_function(to_upper, Table) -> 
    DefValue = noop, % fun(C) -> C.
    ux_unidata_parser:ets_fun(Table, DefValue);
get_function(to_lower, Table) -> 
    DefValue = noop, % fun(C) -> C.
    ux_unidata_parser:ets_fun(Table, DefValue).









%%
%% Helpers
%%

parse_tag([$<|T]) -> parse_tag_(T, []).

parse_tag_([$>|_], Acc) -> list_to_atom(lists:reverse(Acc));
parse_tag_([H|T],  Acc) -> parse_tag_(T, [H|Acc]).


parse_comment(Comment) ->
    SH = string:str(Comment, "HIRAGANA LETTER SMALL") > 0,
    NH = string:str(Comment, "HIRAGANA LETTER") > 0,
    SK = string:str(Comment, "KATAKANA LETTER SMALL") > 0,
    NK = string:str(Comment, "KATAKANA LETTER") > 0,

    if 
        SH -> small_hiragana;
        NH -> normal_hiragana;
        SK -> small_katakana;
        NK -> normal_katakana;
        true -> skip end.
