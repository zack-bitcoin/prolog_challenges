
multiply_array(_, [], []).
multiply_array(X, [A|T1], [B|T2]) :-
    %multiply every element of an array.
    B is X*A,
    multiply_array(X, T1, T2).

plug_in(_, N, [N]).
plug_in(X, Y, [H|T]) :-
    %plug X into the polynomial to calculate Y.
    multiply_array(X, T, T2),
    plug_in(X, Y2, T2),
    Y is Y2 + H.

derivative(D, [_|F]) :-
%D is the derivative of F.
    derivative2(D, F, 1).
derivative2([D], [F], N) :-
    D is F*N.
derivative2([D1|D2], [F1|F2], N) :-
    D1 is F1*N,
    M is N+1,
    derivative2(D2, F2, M).

newton_improve(X, Y, F) :-
    %X is current guess of the zero of the function F.
    %Y is a better guess.
    %Y = X - (F(X)/F'(X))
    plug_in(X, T, F),
    derivative(FP, F),
    plug_in(X, B, FP),
    Y is X - (T / B).

newtons_method(X, F) :-
    newton_improve(0.1, X1, F),
    newton_improve(X1, X2, F),
    newton_improve(X2, X3, F),
    newton_improve(X3, X4, F),
    newton_improve(X4, X5, F),
    newton_improve(X5, X, F).

%example of using this
% calculate a zero of Y = -5 + X^2 + 3*X^3
% newtons_method(X, [-5, 0, 1, 3]).
% X = 2.251083120039576
