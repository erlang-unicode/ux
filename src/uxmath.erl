-module(uxmath).
-author('Uvarov Michael <freeakk@gmail.com>').

-export([factorial/1]). % :)

% Statistics
-export([average/1]).     % http://en.wikipedia.org/wiki/Arithmetic_mean
%-export([expectation/1]). % http://en.wikipedia.org/wiki/Expected_value
-export([variance/1]).    % http://en.wikipedia.org/wiki/Variance 
-export([stdev/1]).       % http://en.wikipedia.org/wiki/Standard_deviation
-export([stdev_error/1]).       

-define(POW2(X), ((X) * (X))).

factorial(N) when N > 0, is_integer(N) -> factorial1(N, 1);
factorial(0) -> 1.
factorial1(0, X) -> 
    X;
factorial1(N, X)  -> 
    factorial1(N - 1, N * X).

average([X|Tail]) ->
    average1(Tail, X, 1).
average1([X|Tail], Sum, Count) ->
    average1(Tail, Sum + X, Count + 1);
average1([], Sum, Count) ->
    Sum / Count.

% Dispersion
variance([_|_] = X) ->
    variance1(X, average(X), 0, 0).
variance1([X|Tail], A, Sum, Count) ->
    Diff = X - A,
    variance1(Tail, A, Sum + ?POW2(Diff), Count + 1);
variance1([], _, Sum, Count) -> Sum / Count.


stdev([_,_|_] = X) ->
    stdev1(X, average(X), 0, 0).
stdev1([X|Tail], A, Sum, Count) ->
    Diff = X - A,
    stdev1(Tail, A, Sum + ?POW2(Diff), Count + 1);
stdev1([], _, Sum, Count) -> math:sqrt(Sum / (Count - 1)).

stdev_error(Val) ->
    stdev_error1(Val, 3*stdev(Val), average(Val), []). % 3-sigma rule

stdev_error1([H|T], Sigma3, Avg, Res) ->
    Res2 = case erlang:abs(H - Avg) >= Sigma3 of
        true -> [H|Res];
        false -> Res
    end,
    stdev_error1(T, Sigma3, Avg, Res2);
stdev_error1([   ],_Sigma3,_Avg, Res) ->
    lists:reverse(Res).

