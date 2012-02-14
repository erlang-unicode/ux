%%% @doc Default Word Cluster Boundary Breaker
%%% 
%%%      [UTR29: Word Cluster Boundaries]
%%%      (http://unicode.org/reports/tr29/#Word_Cluster_Boundaries)
%%% @end

-module(ux_wb).
-include("ux.hrl").
-export([split/1, words/1]).


% Carriage Return
-define(CR, 16#000D).

% Line Feed
-define(LF, 16#000A).

% Zero width non-joiner
-define(ZWNJ, 16#200C).

% Zero width joiner
-define(ZWJ, 16#200D).



%% Adds the atom 'x' between non-breaked characters.
split(S) -> 
    Acc = [],

    % extract general classes
    TypesFn = ux_unidata:break_props('word'),
    Types = lists:map(TypesFn, S),
    {ColS, ColTypes} = collapse(S, Types),

    LastType = '',
    Res = do_split(LastType, ColS, ColTypes, Acc),

    {Types, expand(Res)}.

    
words(S) ->
    {Types, Splitted} = ux_wb:split(S),
    Mod = false,
    Word = [],
    Acc = [],
    do_words(Mod, Splitted, Types, Word, Acc).
    


%% Extract words.

% Extract word.
do_words(true, ['-'|ST], TT, Word=[_|_], Acc) ->
    RWord = lists:reverse(Word),
    NewAcc = [RWord|Acc],

    Mod = false,
    NewWord = [],
    do_words(Mod, ST, TT, NewWord, NewAcc);

% Skip extracting. Not word.
do_words(_Mod, ['-'|ST], TT, _Word, Acc) ->
    Mod = false,
    NewWord = [],
    do_words(Mod, ST, TT, NewWord, Acc);
    
% Word.
do_words(_Mod, [SH|ST], [TH|TT], Word, Acc) 
    when TH=:='ALetter' ->
    Mod = true,
    NewWord = [SH|Word],
    do_words(Mod, ST, TT, NewWord, Acc);
    
% Maybe word.
do_words(Mod, [SH|ST], [TH|TT], Word, Acc) ->
    NewWord = [SH|Word],
    do_words(Mod, ST, TT, NewWord, Acc);

% End of string.
do_words(true, [], [], [_|_] = Word, Acc) ->
    RWord = lists:reverse(Word),
    NewAcc = [RWord|Acc],
    lists:reverse(NewAcc);

do_words(false, [], [], _Word, Acc) ->
    lists:reverse(Acc).
    
    
    
    
    


% WB4
collapse(S, T) ->
    SR = [],
    TR = [],
    do_collapse(S, T, SR, TR).
    

do_collapse([SH1,SH2|ST], [TH1,TH2|TT], SR, TR) 
    when (TH2=:='Extend' orelse 
          TH2=:='Format')
    andalso TH1=/='Newline'
    andalso TH1=/='CR'
    andalso TH1=/='LF' ->
    {NewST, NewTT, SH} = do_collapse2(ST, TT, [SH2,SH1]),
    do_collapse(NewST, NewTT, [SH|SR], [TH1|TR]);

do_collapse([SH|ST], [TH|TT], SR, TR) ->
    do_collapse(ST, TT, [SH|SR], [TH|TR]);
    
do_collapse([], [], SR, TR) ->
    {lists:reverse(SR), lists:reverse(TR)}.


do_collapse2([SH|ST], [TH|TT], SR) 
    when TH=:='Extend'
       ; TH=:='Format' ->
    do_collapse2(ST, TT, [SH|SR]);
do_collapse2(ST, TT, SR) ->
    {ST, TT, SR}.



expand(S) ->
    Acc = [],
    do_expand(S, Acc).

do_expand([[_|_]=H|T], Acc) ->
    NewAcc = do_expand2(H, Acc),
    do_expand(T, NewAcc);
    
do_expand([H|T], Acc) ->
    NewAcc = [H|Acc],
    do_expand(T, NewAcc);
    
do_expand([], Acc) ->
    Acc.

do_expand2([H|T], Acc) ->
    do_expand2(T, [H|Acc]);
do_expand2([], Acc) ->
    Acc.






% WB3
do_split(_LT, [_CR,_LF|ST], 
              ['CR','LF'|TT], Acc) ->
    NewAcc = [?LF,?CR|Acc],
    do_split('LF', ST, TT, NewAcc);

% WB3a
do_split(_LT, [SH|ST], 
              [TH1|TT = [_|_]], Acc) 
    when TH1=:='Newline'
       ; TH1=:='CR'
       ; TH1=:='LF' ->
    NewAcc = ['-',SH|Acc],
    do_split(TH1, ST, TT, NewAcc);

% WB3b
do_split(_LT, [SH|ST], 
              [TH1|TT=[TH2|_]], Acc) 
    when TH2=:='Newline'
       ; TH2=:='CR'
       ; TH2=:='LF' ->
    NewAcc = ['-',SH|Acc],
    do_split(TH1, ST, TT, NewAcc);
    




% WB5
do_split(_LT, [SH|ST], 
              ['ALetter'|TT = ['ALetter'|_]], Acc) ->
    NewAcc = [SH|Acc],
    do_split('ALetter', ST, TT, NewAcc);

% WB6 
do_split(_LT, [SH|ST], 
              ['ALetter'|TT = [TH2,'ALetter'|_]], Acc) 
    when TH2=:='MidLetter'
       ; TH2=:='MidNumLet' ->
    NewAcc = [SH|Acc],
    do_split('ALetter', ST, TT, NewAcc);

 
% WB7 
do_split('ALetter', [SH|ST], 
              [TH1|TT = ['ALetter'|_]], Acc) 
    when TH1=:='MidLetter'
       ; TH1=:='MidNumLet' ->
    NewAcc = [SH|Acc],
    do_split(TH1, ST, TT, NewAcc);
 
% WB8
do_split(_LT, [SH|ST], 
              ['Numeric'|TT = ['Numeric'|_]], Acc) ->
    NewAcc = [SH|Acc],
    do_split('Numeric', ST, TT, NewAcc);

% WB9
do_split(_LT, [SH|ST], 
              ['ALetter'|TT = ['Numeric'|_]], Acc) ->
    NewAcc = [SH|Acc],
    do_split('ALetter', ST, TT, NewAcc);

% WB10
do_split(_LT, [SH|ST], 
              ['Numeric'|TT = ['ALetter'|_]], Acc) ->
    NewAcc = [SH|Acc],
    do_split('Numeric', ST, TT, NewAcc);


 
% WB11
do_split('Numeric', [SH|ST], 
              [TH1|TT = ['Numeric'|_]], Acc) 
    when TH1=:='MidNum'
       ; TH1=:='MidNumLet' ->
    NewAcc = [SH|Acc],
    do_split(TH1, ST, TT, NewAcc);

% WB12
do_split(_LT, [SH|ST], 
              ['Numeric'|TT = [TH2,'Numeric'|_]], Acc) 
    when TH2=:='MidNum'
       ; TH2=:='MidNumLet' ->
    NewAcc = [SH|Acc],
    do_split('Numeric', ST, TT, NewAcc);


% WB13
do_split(_LT, [SH|ST], 
              ['Katakana'|TT = ['Katakana'|_]], Acc) ->
    NewAcc = [SH|Acc],
    do_split('Katakana', ST, TT, NewAcc);

% WB13a
do_split(_LT, [SH|ST], 
              [TH1|TT = ['ExtendNumLet'|_]], Acc) 
    when TH1=:='ALetter'
       ; TH1=:='Numeric'
       ; TH1=:='Katakana'
       ; TH1=:='ExtendNumLet' ->
    NewAcc = [SH|Acc],
    do_split(TH1, ST, TT, NewAcc);

% WB13b
do_split(_LT, [SH|ST], 
              ['ExtendNumLet'|TT = [TH2|_]], Acc) 
    when TH2=:='ALetter'
       ; TH2=:='Numeric'
       ; TH2=:='Katakana' ->
    NewAcc = [SH|Acc],
    do_split('ExtendNumLet', ST, TT, NewAcc);



% Any
do_split(_LT, 
            [SH|ST], 
            [TH|TT=[_|_]], Acc) ->
    NewAcc = ['-',SH|Acc],
    do_split(TH, ST, TT, NewAcc);

% Any
do_split(_LT, 
            [SH], 
            [TH], Acc) ->
   [SH|Acc];

do_split(_T, [], [], ['-'|Acc]) ->
   Acc;

do_split(_T, [], [], Acc) ->
   Acc.

    
