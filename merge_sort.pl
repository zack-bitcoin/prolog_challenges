listify([], []).
listify([H|T], [[H]|T2]) :-
    listify(T, T2).
merge_sort(L, R) :-
    listify(L, L2),
    merge2(L2, R).
merge2([X], X).
merge2(L, R) :-
    merge_pairs(L, L2),
    merge2(L2, R).
merge_pairs([], []).
merge_pairs([X], [X]).
merge_pairs([A1|[B1|T1]], [M|T2]) :-
    merge_pair(A1, B1, M),
    merge_pairs(T1, T2).
merge_pair([], [], []).
merge_pair([], X, X).
merge_pair(X, [], X).
merge_pair([H1|T1], [H2|T2], [H1|R]) :-
    H1 @> H2,
    merge_pair(T1, [H2|T2], R).
merge_pair([H1|T1], [H2|T2], [H2|R]) :-
    H1 @=< H2,
    merge_pair([H1|T1], T2, R).
