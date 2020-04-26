
half(4294967296).

%to_big(X, b([X])).
to_big(X, R) :-
    half(H),
    X1 is X mod H,
    X2 is X div H,
    add(b([X1, X2]), b([0]), R).
from_big(b([X]), X).
from_big(b([X, Y]), Z) :-
    half(H),
    Z is X + (Y * H).

add(A, B, C) :-
    add2(A, B, 0, C).
add2(b([]), b([]), 0, b([])).
add2(b([]), b([]), N, b([N])).
add2(b([]), b([B|B2]), N, X) :-
    add2(b([0]), b([B|B2]), N, X).
add2(b([A|A2]), b([]), N, X) :-
    add2(b([A|A2]), b([0]), N, X).
add2(b([A|A2]), b([B|B2]), N, b([C|C2])) :-
    X is A + B + N,
    half(H),
    M is X div H,
    C is X mod H,
    add2(b(A2), b(B2), M, b(C2)).

sub(A, B, C) :-
    flip_sign(B, B2),
    add(A, B2, C).

flip_sign([],[]).
flip_sign([X|T],[-X|T2]) :-
    flip_sign(T, T2).

%mul(b([]), _, b([0])).
%mul(_, b([]), b([0])).
%mul(b([0]), _, b([0])).
%mul(_, b([0]), b([0])).
mul(A, B, C) :-
    mul2(A, B, X),
    sum_up(X, b([0]), C).

sum_up([], A, A).
sum_up([H|T], A, C) :-
    add(b(H), A, B),
    sum_up(T, B, C).

mul2(b([]), _, []).
mul2(b([H|T]), b(B), [H2|T2]) :-
    mul3(H, B, H2, 0),
    mul2(b(T), b([0|B]), T2).

mul3(_, [], [], 0) :- !.
mul3(_, [], [C], C).
mul3(X, [H|T], [H2|T2], C) :-
    half(Half),
    X2 is (X * H) + C,
    H2 is X2 mod Half,
    C2 is X2 div Half,
    mul3(X, T, T2, C2).
    
