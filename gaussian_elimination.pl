%example of using this
%simplify(X, [[1,2,3,6],[2,-3,2,14],[3,1,-1,-2]]).
%X = [[1.0,2.0,3.0,6.0],[-0.0,1.0,0.5714285714285714,-0.2857142857142857],[0.0,0.0,1.0,2.9999999999999996]] 



multiply_array(_, [], []).
multiply_array(X, [A|T1], [B|T2]) :-
    %multiply every element of an array.
    B is X*A,
    multiply_array(X, T1, T2).

subtract_array([],[],[]).
subtract_array([A|AT],[B|BT],[C|CT]) :-
    C is A - B,
    subtract_array(AT, BT, CT).

first_non_zero(X, [0.0|T]) :-
    first_non_zero(X, T).
first_non_zero(X, [X|_]).

leading_one(F, S) :-
    %multiply S by a constant so that the first non-zero term becomes a 1, this is F.
    first_non_zero(C, S),
    IC is 1 / C,
    multiply_array(IC, S, F).

simplify([F], [S]) :-
    leading_one(F, S).
simplify([F|FT], [S|ST]):-
    leading_one(F, S),
    zero_column(F, ST, ST2),
    simplify(FT, ST2).

zero_column(_, [], []).
zero_column(F, [MH|M], [M2H|M2]) :-
    %the first non-zero term of F is 1.
    %subtract an amount of F from each row of M to calculate M2.
    %M2 has a zeroed out column.
    zero2(F, MH, M2H),
    zero_column(F, M, M2).

zero2([0.0|F], [0.0|M], [0.0|M2]) :-
    %subtract an amount of F from M to get M2.
    %M2 should have one more leading zero than F.
    zero2(F, M, M2).
zero2([_|FT], [0.0|MT], [0.0|MT]).
    %if it is already zero, we don't need to subtract any of F.
zero2([_|FT], [MH|MT], [0.0|MF]) :-
    C is 1 / MH,
    multiply_array(C, MT, M2),
    subtract_array(M2, FT, MF).
