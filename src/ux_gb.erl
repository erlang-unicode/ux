%%% @doc Default Grapheme Cluster Boundary Breaker
%%% 
%%%      [UTR29: Grapheme Cluster Boundaries]
%%%      (http://unicode.org/reports/tr29/#Grapheme_Cluster_Boundaries)
%%% @end

-module(ux_gb).
-include("ux_char.hrl").
-export([split/2]).


% Carriage Return
-define(CR, 16#000D).

% Line Feed
-define(LF, 16#000A).

% Zero width non-joiner
-define(ZWNJ, 16#200C).

% Zero width joiner
-define(ZWJ, 16#200D).



%% Adds the atom 'x' between non-breaked characters.
split(T,S) when
    T=:='legacy';
    T=:='extended' -> 
    Acc = [],

    % extract general classes
    TypesFn = ux_unidata:break_props('grapheme'),
    Types = lists:map(TypesFn, S),

    do_split(T, S, Types, Acc).


%% http://unicode.org/reports/tr29/#Table_Combining_Char_Sequences_and_Grapheme_Clusters

% GB3
do_split(T, [_CR,_LF|ST], 
            ['CR','LF'|TT], Acc) ->
    NewAcc = [?LF,'x',?CR|Acc],
    do_split(T, ST, TT, NewAcc);

% GB4
do_split(T, [SH|ST], 
            [_|TT = [TH|_]], Acc) 
    when TH=:='Control'
       ; TH=:='CR'
       ; TH=:='LF' ->
    NewAcc = [SH|Acc],
    do_split(T, ST, TT, NewAcc);

% GB5
do_split(T, [SH|ST], 
            [TH|TT], ['x'|Acc]) 
    when TH=:='Control'
       ; TH=:='CR'
       ; TH=:='LF' ->
    NewAcc = [SH|Acc],
    do_split(T, ST, TT, NewAcc);

% GB5
do_split(T, [SH|ST], 
            [TH|TT], Acc) 
    when TH=:='Control'
       ; TH=:='CR'
       ; TH=:='LF' ->
    NewAcc = [SH|Acc],
    do_split(T, ST, TT, NewAcc);
    
% GB6
do_split(T, [SH|ST], 
            ['L'|TT = [TH2|_]], Acc) 
    when TH2=:='L'
       ; TH2=:='V'
       ; TH2=:='LV'
       ; TH2=:='LVT' ->
    NewAcc = ['x',SH|Acc],
    do_split(T, ST, TT, NewAcc);

% GB7
do_split(T, [SH|ST], 
            [TH1|TT = [TH2|_]], Acc) 
    when (TH2=:='V'  orelse TH2=:='T')
     and (TH1=:='LV' orelse TH1=:='V') ->
    NewAcc = ['x',SH|Acc],
    do_split(T, ST, TT, NewAcc);

% GB8
do_split(T, [SH|ST], 
            [TH1|TT = ['T'|_]], Acc) 
    when TH1=:='LVT'
       ; TH1=:='T' ->
    NewAcc = ['x',SH|Acc],
    do_split(T, ST, TT, NewAcc);

 
% GB 9
do_split(T, [SH|ST], 
            [_|TT = ['Extend'|_]], Acc) ->
    NewAcc = ['x',SH|Acc],
    do_split(T, ST, TT, NewAcc);

% GB 9a
do_split('extended'=T, 
            [SH|ST], 
            [_|TT = ['SpacingMark'|_]], Acc)
     ->
    NewAcc = ['x',SH|Acc],
    do_split(T, ST, TT, NewAcc);

% GB 9b
do_split('extended'=T, 
            [SH|ST], 
            ['Prepend'|TT = [_|_]], Acc) ->
    NewAcc = ['x',SH|Acc],
    do_split(T, ST, TT, NewAcc);

% Any
do_split(T, 
            [SH|ST], 
            [_|TT], Acc) ->
    NewAcc = [SH|Acc],
    do_split(T, ST, TT, NewAcc);

do_split(_T, [], [], ['x'|Acc]) ->
    lists:reverse(Acc);

do_split(_T, [], [], Acc) ->
    lists:reverse(Acc).

    
