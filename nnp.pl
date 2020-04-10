%https://www.ic.unicamp.br/~meidanis/courses/mc336/2009s2/prolog/problemas/    
my_last(X, [_|T]) :-
    my_last(X, T).
my_last(X, [X]).

second_last(X, [X, _]).
second_last(X, [_|T]) :-
    second_last(X, T).

element_at(X, [X|_], 0).
element_at(X, [_|T], N) :-
    M is N-1,
    element_at(X, T, M).

reverse_list(X, L) :-
    reverse2(X, L, []).
reverse2(L, [], L).
reverse2(L, [H|T], X) :-
    reverse2(L, T, [H|X]).

palindrome(L) :-
    reverse(L, L).

my_flatten(L, X) :-
    my_flatten2(L, X).
my_flatten2([], []).
my_flatten2([T], X) :-
    my_flatten2(T, X).
my_flatten2([[]|T], X) :-
    my_flatten2(T, X).
my_flatten2([[H|T1]|T2], [H|X]) :-
    my_flatten([T1|T2], X).

compress([], []).
compress([X|[X|T]], L) :-
    compress([X|T], L).
compress([H|T], [H|T2]) :-
    compress(T, T2).
% #9
pack([], []).
pack([A], [[A]]).
pack([A|[A|T]], [[A|[A|T2]]|T3]) :-
    pack([A|T], [[A|T2]|T3]).
pack([A|[B|T]], [[A]|T3]) :-
    pack([B|T], T3).

encode(L, X) :-
    pack(L, L2),
    encode2(L2, X).
encode2([], []).
encode2([H1|T1], [[S, A]|T2]) :-
    length(H1, S),
    compress(H1, [A]),
    encode2(T1, T2).

% #11
encode_modified(L, X) :-
    pack(L, L2),
    encode2b(L2, X).
encode2b([], []).
encode2b([[A]|T1], [A|T2]) :-
    encode2b(T1, T2).
encode2b([H1|T1], [[S, A]|T2]) :-
    length(H1, S),
    compress(H1, [A]),
    encode2b(T1, T2).

decode([], []).
decode([[0, _]|T], L) :-
    decode(T, L).
decode([[S, A]|T], [A|T2]) :-
    M is S-1,
    decode([[M, A]|T], T2).
decode([A|T], [A|T2]) :-
    decode(T, T2).
    
encode_direct([], []).
encode_direct([A], [A]).
encode_direct([A], [[1, A]]).
encode_direct([A|[A|T]], [[S, A]|T2]) :-
    encode_direct([A|T], [[M, A]|T2]),
    S is M+1.
encode_direct([A|[B|T]], [A|T2]) :-
    encode_direct([B|T], T2).
encode_direct([A|T], [[1, A]|T2]) :-
    encode_direct(T, T2).

dupli([], []).
dupli([X|T], [X|[X|T2]]) :-
    dupli(T, T2).

dupli([], _, []).
dupli([H|T], N, L) :-
    dupli2(H, N, L, L2),
    dupli(T, N, L2).
dupli2(_, 0, L, L).
dupli2(X, N, [X|L], L2) :-
    M is N-1,
    dupli2(X, M, L, L2).

drop([], _, []).
drop(L, N, R) :-
    drop2(N, L, L2, R, R2),
    drop(L2, N, R2).

drop2(_, [], [], [], []).
drop2(1, [_|T], T, R, R).
drop2(N, [A|T], L2, [A|R], R2) :-
    M is N-1,
    drop2(M, T, L2, R, R2).

split(L, 0, [], L).
split([H|T], N, [H|T2], L) :-
    M is N-1,
    split(T, M, T2, L).

slice(L1, S, E, L2) :-
    split(L1, S, _, L3),
    E2 is E-S,
    split(L3, E2, L2, _).

rotate(L, N, X) :-
    split(L, N, A, B),
    length(B, M),
    split(X, M, B, A).

remove_at(H, [H|T], 0, T).
remove_at(X, [H|T], N, [H|R]) :-
    M is N-1,
    remove_at(X, T, M, R).

insert_at(X, L, 0, [X|L]).
insert_at(X, [H|T1], N, [H|T2]) :-
    M is N-1,
    insert_at(X, T1, M, T2).
    
range(X, X, [X]).
range(A, B, [A|T]) :-
    A < B,
    C is A+1,
    range(C, B, T).

rng_select(_, 0, []).
rng_select(L, N, [H|T]) :-
    random(R),
    length(L, S),
    RS is truncate(R * S),
    remove_at(H, L, RS, L2),
    M is N-1,
    rng_select(L2, M, T).

lotto(Many, End, Result) :-
    range(0, End, Range),
    rng_select(Range, Many, Result).

rnd_permu(A, B) :-
    length(A, S),
    rng_select(A, S, B).

is_in(X, [X|_]).
is_in(X, [_|T]) :-
    is_in(X, T).

combinations(0, _, []).
combinations(N, L, [H|T]) :-
    M is N-1,
    length(L, S),
    S2 is S-1,
    range(0, S2, R), 
    is_in(E, R),
    remove_at(H, L, E, L2),
    combinations(M, L2, T).

