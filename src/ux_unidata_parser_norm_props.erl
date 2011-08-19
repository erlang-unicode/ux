%%% DerivedNormalizationProps.txt
%%% @private
-module(ux_unidata_parser_norm_props).
-export([parse/1, types/0, get_function/2
%, after_parse/1
]).

%%% Example:
%%% ux_unidata_filelist:get_pid({norm_props, all, code:priv_dir(ux) ++ "/UNIDATA/DerivedNormalizationProps.txt"}).
-define(TO_INT(C), ux_unidata_parser:hex_to_int(C)).

types() ->
    [nfc_qc
    ,nfd_qc
    ,nfkc_qc
    ,nfkd_qc
    ].

parse(In) ->
    case ux_unidata_parser:delete_spaces(In) of
    [] -> skip;
    Data -> 
        case ux_unidata_parser:split($;, Data) of
        [Code, Form, Props] when (Form=="NFC_QC" orelse Form=="NFKC_QC" 
                           orelse Form=="NFD_QC" orelse Form=="NFKD_QC")
                             and (Props=="N" 
%                          orelse Props=="Y"  
                           orelse Props=="M") ->
        Atom = list_to_atom(string:to_lower(Props)),
        ParsedKey = parse_code(Code),
        {ok, 
        [{nfc_qc,  case Form of "NFC_QC"  -> {ParsedKey, Atom}; _ -> skip end}
        ,{nfd_qc,  case Form of "NFD_QC"  -> {ParsedKey, Atom}; _ -> skip end}
        ,{nfkc_qc, case Form of "NFKC_QC" -> {ParsedKey, Atom}; _ -> skip end}
        ,{nfkd_qc, case Form of "NFKD_QC" -> {ParsedKey, Atom}; _ -> skip end}
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


get_function(_Type, Table) ->
    DefValue = y,
    ux_unidata_parser:expand_opt_fun(Table, DefValue).

%%
%% Helpers
%%
parse_code(Code) -> case string:tokens(Code, "..") of
    [From, To]  -> {?TO_INT(From), ?TO_INT(To)};
    [Code]      -> ?TO_INT(Code)
    end.

