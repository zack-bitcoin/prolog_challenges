test(X) :-
    lisp('test.lisp', X).
lisp(FileLoc, Result) :-
    open(FileLoc, read, In),
    lisp2(In, Result),
    !,
    close(In).
lisp2(In, Result) :-
    parse(In, AST),
    run_all(AST, Result, nil).
run_all([], [], _).
run_all([H|T], [H2|T2], Funs) :-
    run(H, H3, Funs, Funs2),
    ((H3 = none,
      run_all(T, [H2|T2], Funs2));
     (H2 = H3,
      run_all(T, T2, Funs2))).
run([+, A, B], R, Funs, Funs) :-
    run(A, A2, Funs, Funs),
    run(B, B2, Funs, Funs),
    R is A2 + B2.
run([*, A, B], R, Funs, Funs) :-
    run(A, A2, Funs, Funs),
    run(B, B2, Funs, Funs),
    R is A2 * B2.
run([-, A, B], R, Funs, Funs) :-
    run(A, A2, Funs, Funs),
    run(B, B2, Funs, Funs),
    R is A2 - B2.
run([div, A, B], R, Funs, Funs) :-
    run(A, A2, Funs, Funs),
    run(B, B2, Funs, Funs),
    R is A2 div B2.
run([mod, A, B], R, Funs, Funs) :-
    run(A, A2, Funs, Funs),
    run(B, B2, Funs, Funs),
    R is A2 mod B2.
run(nil, [], _, _).
run([car, L], H2, Funs, Funs) :-
    run(L, [H2|_], Funs, Funs).
run([cdr, L], T2, Funs, Funs) :-
    run(L, [_|T2], Funs, Funs).
run([cons, H, T], [H2|T2], Funs, Funs) :-
    run(H, H2, Funs, Funs),
    run(T, T2, Funs, Funs).
run([if, Bool, A, B], R, Funs, Funs) :-
    run(Bool, X, Funs, _),
    ((X = 0,
      run(B, R, Funs, _));
     (not(X = 0),
      run(A, R, Funs, _))).
run([define, Name0, Vars, Fun], none, Funs, Funs2) :-
    numberify_atom(Name0, Name),
    add(Name, [Vars, Fun], Funs, Funs2).
run(R, Result, _, _) :-
    can_number(R),
    number_atom(Result, R).
run([FunName|Inputs], Result, Funs, _) :-
    numberify_atom(FunName, Name),
    get(Name, Funs, [Vars, Fun]),
    bind(Vars, Inputs, Pairs),
    load_vars(Pairs, Fun, Result0),
    run(Result0, Result, Funs, Funs).

load_vars([], X, X).
load_vars([Pair|T], Fun, Result) :-
    load_var(Pair, Fun, Fun2),
    load_vars(T, Fun2, Result).
load_var(_, [], []) :- !.
load_var([Key, Val], Key, Val) :- !.
load_var([Key, Val], [H|T], [H2|T2]) :-
    load_var([Key, Val], H, H2),
    load_var([Key, Val], T, T2),
    !.
load_var(_, X, X).

bind([], [], []).
bind([A|T], [N|TN], [[A, N]|R]) :-
    bind(T, TN, R).

parse(In, AST) :-
    get_char(In, C),
    parse2(In, C, AST).
parse2(_In, end_of_file, []).
parse2(In, C, AST) :-
    white_space(C),
    parse(In, AST).
parse2(In, '(', [H|T]) :-
    get_char(In, C),
    parse3(In, C, H),
    get_char(In, C2),
    parse2(In, C2, T).
parse2(In, C, [H|T]) :-
    read_word(In, C, '', H, D),
    parse2(In, D, T).

parse3(_, ')', []).
parse3(In, C, [H|T]) :-
    read_word(In, C, '', H, D),
    parse3(In, D, T).

read_word(In, W, '', Result, D) :-
    white_space(W),
    !,
    get_char(In, C),
    read_word(In, C, '', Result, D).
read_word(_, W, Result, Result, W) :-
    white_space(W).
read_word(_, ')', Result, Result, ')').
read_word(In, '(', '', Result, D) :-
    get_char(In, C),
    parse3(In, C, Result),
    get_char(In, D).
read_word(In, C, P, Result, D) :-
    atom_concat(P, C, P2),
    get_char(In, C2),
    read_word(In, C2, P2, Result, D).
    
white_space(' ').
white_space('\n').
white_space('\t').

can_number(A) :-
    atom(A),
    atom_chars(A, L),
    can_number2(L).
can_number2([]).
can_number2([H|T]) :-
    char_code(H, N),
    N > 47,
    N < 58,
    can_number2(T).

numberify_atom(A, N) :-
    atom_codes(A, L),
    numberify_atom2(1, L, N).
numberify_atom2(N, [], N):- !.
numberify_atom2(A, [H|T], R) :-
    A2 is H + (A*100),
    numberify_atom2(A2, T, R).

not(X) :- \+ X.
