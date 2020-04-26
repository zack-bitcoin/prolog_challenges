
april(1, 940500).
april(2, 1020920).
april(3, 1117272).
april(4, 1201186).
april(5, 1274854).
april(6, 1346822).
april(7, 1429982).
april(8, 1514776).
april(9, 1600590).
april(10,1694667).
april(11,1775429).
april(12,1847796).
april(13,1919174).
april(14,1992903).
april(15,2076502).
april(16,2161885).
april(17,2249004).
april(18,2330764).
april(19,2406786).
april(20,2480741).
april(21,2556720).
april(22,2637439).
april(23,2722857).
april(24,2828682).


april2(N, M) :-
    april(1, S),
    april(24, F),
    Rise is F - S,
    Run is 23,
    Slope is Rise / Run,
    N2 is N - 1,
    M is S + (Slope * N2).

diffs(S, S, []).
diffs(S, E, [H|T]) :-
    S < E,
    april(S, Real),
    april2(S, Projected),
    H is (abs(Real - Projected)) / Real,
    S2 is S + 1,
    diffs(S2, E, T).

average(L, A) :-
    length(L, S),
    sum(M, L),
    A is M / S.
sum(M, L) :-
    sum2(M, 0, L).
sum2(M, M, []).
sum2(F, A, [H|T]) :-
    B is A + H,
    sum2(F, B, T).

doit(R) :-
    diffs(1, 25, X),
    average(X, R).
