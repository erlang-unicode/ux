-module(ux_cldr_collation).
-compile(export_all).

%% http://erlsom.sourceforge.net/erlsom.htm
-include("erlsom/ldml.hrl").

%load_example_bin() ->
%    Archive = ux_unidata:get_source_file('cldr_core'),
%    Filename = "common/collation/root.xml",
%    Options = [memory, {file_list, [Filename]}],
%    {ok, [{Filename, Bin}]} = zip:extract(Archive, Options),
%    Bin.

load_example_bin() ->
    Filename = code:priv_dir(ux) ++ "/root.xml",
    {ok, Bin} = file:read_file(Filename),
    Bin.


parse_example(BinXml) ->
    M = ux_erlsom_model_server:get_model('ldml'),
    {ok, Result, _} = erlsom:scan(BinXml, M),
    Result.


example() ->
    Bin = load_example_bin(),
    Result = parse_example(Bin),
    Collations = (Result#ldml.choice)#'ldml/SEQ1'.collations,
    % Collations#collatins.validSubLocales
    Choice = Collations#collations.choice,
    DefType = 
        try
            [Default] = Choice#'collations/SEQ1'.default,
            Default#default.type
        catch error:_ ->
            'undefined'
        end,
        


    CollationList = Choice#'collations/SEQ1'.collation,
    
    lists:foldl(fun parse_el/2, [], CollationList)
    .


parse_el(El = #collation{
        type = TypeString,
        choice = #'collation/SEQ1'{
            settings = Settings,
            suppress_contractions = SC,
            optimize = Opt,
            rules = #rules{
                    choice = #'rules/SEQ1'{
                        choice = RuleH,
                        choice1 = RuleT
                    }
                }
            }
        }, Acc) ->
        Rules = [RuleH|RuleT],
        Type = list_to_atom(TypeString),        

    [{Type, Rules}|Acc]
    .


reassign(E, [#reset{choice = [Chars]}|T]) ->
    ets:lookup(E,Chars),
    reassign(E, T);
reassign(E, [_H|T]) ->
    reassign(E, T);
reassign(E, []) ->
    ok.


