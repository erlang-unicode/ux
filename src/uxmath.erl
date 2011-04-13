-module(uxmath).
-author('Uvarov Michael <freeakk@gmail.com>').

-export([factorial/1]). % :)

% Statistics
-export([average/1]).     % http://en.wikipedia.org/wiki/Arithmetic_mean
%-export([expectation/1]). % http://en.wikipedia.org/wiki/Expected_value
-export([variance/1]).    % http://en.wikipedia.org/wiki/Variance 
-export([stdev/1]).       % http://en.wikipedia.org/wiki/Standard_deviation

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


