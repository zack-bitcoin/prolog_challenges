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

%R is every way to get N elements, from list L.
combinations(0, [], []).
combinations(N, [H|L], [H|R]) :-
    M is N-1,
    combinations(M, L, R).
combinations(N, [_|L], R) :-
    combinations(N, L, R).

%group(L, S, Gs).
%L is input list, S is list of sizes, Gs is output list of lists of sublists of L. Every possible combination.

group(_, [], []).
group(L, [SH|ST], [G|GT]) :-
    combinations(SH, L, G),
    remove_things(L, G, L2),
    group(L2, ST, GT).
remove_things(L, [], L).
remove_things(L, [H|G], L2) :-
    remove_it(L, H, L3),
    remove_things(L3, G, L2).
remove_it([H|L], H, L).
remove_it([X|L], H, [X|L2]) :-
    remove_it(L, H, L2).

sort2([X], [X]).
sort2(L, [H|[H2|T]]) :-
    is_in(H, L),
    remove_it(L, H, L2),
    sort2(L2, [H2|T]),
    H @>= H2.

lsort([X], [X]).
lsort(L, [H|[H2|T]]) :-
    is_in(H, L),
    remove_it(L, H, L2),
    lsort(L2, [H2|T]),
    length(H, S1),
    length(H2, S2),
    S2 @=< S1.

lfsort(L, R) :-
    lengths(L, L2),
    sort2(L2, L3),
    encode(L3, F),
    lfsort2(F, L, R).
lengths([],[]).
lengths([H|T], [S|R]) :-
    length(H, S),
    lengths(T, R).
lfsort2(_, [X], [X]).
lfsort2(Frequencies, L, [H|[H2|T]]) :-
    is_in(H, L),
    remove_it(L, H, L2),
    lfsort2(Frequencies, L2, [H2|T]),
    length(H, S1),
    length(H2, S2),
    lookup(S1, Frequencies, F1),
    lookup(S2, Frequencies, F2),
    F2 @=< F1.
lookup(K, [[V, K]|_], V).
lookup(K, [_|T], V) :-
    lookup(K, T, V).


sqrt(X, R) :-
    sqrt_improve(1, B, X),
    sqrt_improve(B, B2, X),
    sqrt_improve(B2, B3, X),
    sqrt_improve(B3, B4, X),
    sqrt_improve(B4, R, X).
sqrt_improve(S, B, X) :-
    N is X / S,
    B is floor((N + S) / 2).
is_prime(2).
is_prime(X) :-
    X @> 1,
    sqrt(X, SX),
    is_prime2(X, SX).
is_prime2(X, S) :-
    range(2, S, R),
    none_divides(R, X).
is_not_prime(0).
is_not_prime(1).
is_not_prime(X) :-
    X @> 3,
    sqrt(X, SX),
    is_not_prime2(X, SX).
is_not_prime2(X, S) :-
    some_divides(2, S, X).
none_divides([], _).
none_divides([H|T], X) :-
    X2 is X mod H,
    X2 =\= 0,
    none_divides(T, X).

some_divides(S, _, X) :-
    0 is X mod S, !.
some_divides(S, T, X) :-
    S @=< T,
    S2 is S + 1,
    some_divides(S2, T, X).

gcd(X, Y, D) :-
    X < Y,
    gcd(Y, X, D).
gcd(X, 0, X).
gcd(X, Y, D) :-
    Y =\= 0,
    Y2 is X mod Y,
    gcd(Y, Y2, D).

coprime(X, Y) :-
    gcd(X, Y, 1).
not_coprime(X, Y) :-
    gcd(X, Y, G),
    G =\= 1.

totient_phi(X, T) :-
    range(1, X, R),
    totient2(R, 0, X, T).
totient2([], T, _, T).
totient2([H|T], R, X, Z) :-
    coprime(H, X),
    S is R + 1,
    totient2(T, S, X, Z).
totient2([H|T], R, X, Z) :-
    not_coprime(H, X),
    totient2(T, R, X, Z).

prime_factors(1, []).
prime_factors(X, [F|T]) :-
    lowest_factor(X, F),
    Y is round(X / F),
    prime_factors(Y, T).
lowest_factor(X, F) :-
    sqrt(X, S),
    lowest_factor2(X, S, 2, F).
lowest_factor2(X, S, N, X) :-
    N > S.
lowest_factor2(X, S, N, F) :-
    coprime(X, N),
    M is N + 1,
    lowest_factor2(X, S, M, F).
lowest_factor2(X, _, N, N) :-
    not_coprime(X, N).
    

prime_factors_mult(X, R) :-
    prime_factors(X, L),
    encode(L, R).


totient_phi2(M, R) :-
    prime_factors_mult(M, F),
    %F [[Many, Prime1]...]
    % PI( (p - 1)*(p^(m-1)))

    totient_phi2b(F, 1, R).
totient_phi2b([], R, R).
totient_phi2b([[M, P]|T], A, R) :-
    A2 is A * (P - 1) * round(P ** (M - 1)),
    totient_phi2b(T, A2, R).

list_of_primes(S, S, []):-
    !.
list_of_primes(Start, End, [Start|T]) :-
    is_prime(Start),
    S2 is Start + 1,
    list_of_primes(S2, End, T).
list_of_primes(Start, End, T) :-
    is_not_prime(Start),
    S2 is Start + 1,
    list_of_primes(S2, End, T).

goldbach(N, [A, B]) :-
    goldbach2(0, N, A, B).
goldbach2(A, B, A, B) :-
    is_prime(A),
    is_prime(B),
    !.
goldbach2(A1, B1, A2, B2) :-
    A1 @< B1,
    (is_not_prime(A1);
     is_not_prime(B1)),
    A3 is A1 + 1,
    B3 is B1 - 1,
    goldbach2(A3, B3, A2, B2).

goldbach_list(Start, End, L) :-
    Start @=< End,
    1 is Start mod 2,
    S2 is Start + 1,
    goldbach_list(S2, End, L).
goldbach_list(Start, End, []) :-
    Start @> End.
goldbach_list(Start, End, [[Start, P1, P2]|T]) :-
    Start @=< End,
    0 is Start mod 2,
    goldbach(Start, [P1, P2]),
    S2 is Start + 2,
    goldbach_list(S2, End, T).

goldbach_list(Start, End, Min, L) :-
    goldbach_list(Start, End, L2),
    above_min(L2, L, Min).

above_min([], [], _).
above_min([[S, P1, P2]|T1],
          [[S, P1, P2]|T2], Min) :-
    P1 @> Min,
    above_min(T1, T2, Min).
above_min([[_, P1, _]|T1], T2, Min) :-
    P1 @=< Min,
    above_min(T1, T2, Min).
            
%Logic and Codes

and(A, B) :- A, B.
or(A, B) :- A; B.
not(A) :- \+ A.
nand(A, B) :- \+ and(A, B).
xor(A, B) :- A, \+ B.
xor(A, B) :- B, \+ A.
nor(A, B) :- \+ or(A, B).
impl(A, B) :- \+ A; B.
equ(A, B) :- A, B.
equ(A, B) :- \+ A, \+ B.

bind(true).
bind(fail).

table(A, B, Code) :-
    bind(A),
    bind(B),
    do(A, B, Code),
    fail.
do(A,B,_) :-
    write(A),
    write('  '),
    write(B),
    write('  '),
    fail.
do(_,_,Expr) :-
    Expr,
    !,
    write(true),
    nl.
do(_,_,_) :-
    write(fail),
    nl.

:- op(500, xfx, 'and').
:- op(500, xfx, 'or').
:- op(500, fx, 'not').
:- op(500, xfx, 'nand').
:- op(500, xfx, 'xor').
:- op(500, xfx, 'impl').
:- op(500, xfx, 'equ').

table(L, Code) :-
    bind_all(L),
    do(L, Code),
    fail.
bind_all([]).
bind_all([H|T]) :-
    bind(H),
    bind_all(T).
do(L, _) :-
    write_all(L),
    fail.
do(_, Expr) :-
    Expr,
    !,
    write(true),
    nl.
do(_, _) :-
    write(fail),
    nl.
write_all([]).
write_all([H|T]) :-
    write(H),
    write(' '),
    write_all(T).
