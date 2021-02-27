
test_k2p :-
    key2path(1, P),
    test_k2p(P).
test_k2p(P) :-
    get2(P, nil, empty),
    test_k2p(P).

isalist([]).
isalist([_|T]) :-
    is_list(T).

test_k2p2 :-
    %key2path(1, P),
    %_ = P,
    %K = 1,
    hash(1, K1, K2),
    path_size(PS0),
    PS is PS0 div 2,
    _ is K1 + K2 + PS,

    key2path2(K1, [1], [1], 0),
    %[1] = [1],
    %L = [],
    H is K1 mod 2,
    K1b is K1 div 2,
    M is 1 - 1,
    key2path2(K1b, [], [], 0),
    !,
    %L = [],

%    X = [1],
    %isalist([1]),

    %p(K),
    %_ is K + 0,
    %p1(_Prime1),
    %p2(_Prime2),
    %K = 9117007991229937,
    %_K3 is K + 55,
    %X is 1 + 2,
    %_ is X * 2,
    test_k2p2.

test :-
    %atom_get('key', nil, empty),
    get(1, nil, empty),
    test2.
test2 :-
    test.


infinite_loop :-
    [1] = [1],
    infinite_loop.
same(A, A).
crashes(X, Y) :-
    %same([1], [1]),
    %same(X, X),
    atom_property(X, hash(Y)),
    crashes(X, Y).

%crashes2(A, B) :-
%    B = A.
