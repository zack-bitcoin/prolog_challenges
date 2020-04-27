
%actual raw data
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

cases_per_day(S, S, []).
cases_per_day(S, E, [H|T]) :-
    april(S, N1),
    S2 is S+1,
    april(S2, N2),
    H is N2 - N1,
    cases_per_day(S2, E, T).
%cases_per_day(1, 24, X).
%[80420,96352,83914,73668,71968,83160,84794,85814,94077,80762,72367,71378,73729,83599,85383,87119,81760,76022,73955,75979,80719,85418,105825] ?


variance(V) :-
    cases_per_day(1, 24, C),
    average(C, A),
    squared_difference(C, A, C2),
    average(C2, V).
%V = 68914811.852551982

squared_difference([], _, []).
squared_difference([H|T], A, [H2|T2]) :-
    B is H - A,
    H2 is B*B,
    squared_difference(T, A, T2).


april2(N, M) :-
    %a linear model
    april(1, S),
    april(24, F),
    Rise is F - S,
    Run is 23,
    Slope is Rise / Run,
    N2 is N - 1,
    M is S + (Slope * N2).

diffs(S, S, []).
diffs(S, E, [H|T]) :-
    %comparing how far the model is from the raw data.
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

%calculating how far each day is from the model
%diffs(1, 25, X), average(X, A).
%X = [0.0,0.0016405492743970208,0.011261591510003999,0.011989309985587492,0.0046864360461122403,0.0030830709819760264,0.0021589204558548334,0.00025620088813245592,0.0020811347554718874,0.00903609612569517,0.00787432465495729,0.002301355118535079,0.0033683422048280381,0.0074415585444079155,0.006417607068542895,0.0046431903076531583,0.0022293926749256576,0.0022948623750389694,0.0047456035451066622,0.0078853543111233187,0.010043098698468219,0.010257397751972812,0.0087151585392779389,0.0] 



%calculate how far the average day is from the model.
doit(R) :-
    diffs(1, 25, X),
    average(X, R).
