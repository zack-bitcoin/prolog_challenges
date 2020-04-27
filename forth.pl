
test(X) :-
    forth('first.forth', X).

forth(OldLoc, Result) :-
    open(OldLoc, read, In),
    forth2(In, [], [], nil, nil, Result),
    close(In).

forth2(_In, [end_of_file|S], _R, _HT, _, S) :-
    !.
forth2(In, S, R, HT, Funs, F) :-
    get_word(In, '', Word),
    process_word(Word, S, R, HT, Funs, S2, R2, HT2, Funs2, In),
    forth2(In, S2, R2, HT2, Funs2, F).

process_word(end_of_file, S, R, HT, Funs,
             [end_of_file|S], R, HT, Funs, _).
process_word('swap', [A|[B|S]], R, HT, Funs,
             [B|[A|S]], R, HT, Funs, _).
process_word('dup', [H|S], R, HT, Funs,
             [H|[H|S]], R, HT, Funs, _).
process_word('drop', [_|S], R, HT, Funs,
             S, R, HT, Funs, _).
process_word('>r', [A|S], R, HT, Funs,
             S, [A|R], HT, Funs, _).
process_word('r>', S, [A|R], HT, Funs,
             [A|S], R, HT, Funs, _).
process_word('+', [A|[B|S]], R, HT, Funs,
             [C|S], R, HT, Funs, _) :-
    C is A + B.
process_word(':', S, R, HT, Funs,
             S, R, HT, Funs2, In) :-
    get_function(In, Funs, Funs2).
process_word(Word, S, R, HT, Funs,
             S2, R, HT, Funs, _) :-
    can_number(Word),
    number_atom(N, Word),
    S2 = [N|S].

get_function(In, F1, F2) :-
    get_word(In, '', Name0),
    numberify_atom(Name0, Name),
    get_function2(In, [], F),
    add(Name, F, F1, F2).
get_function2(In, Fun, Result) :-
    get_word(In, '', Word),
    get_function3(In, Fun, Result, Word).
get_function3(_, Fun, Fun, ';').
get_function3(In, Fun, Result, Word) :-
    append(Fun,[Word],Fun2),
    get_function2(In, Fun2, Result).
    

can_number(A) :-
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
numberify_atom2(N, [], N).
numberify_atom2(A, [H|T], R) :-
    A2 is H + (A*100),
    numberify_atom2(A2, T, R).


get_word(In, P, Result) :-
    get_char(In, C),
    get_word2(In, C, P, Result).
get_word2(_In, end_of_file, '', end_of_file) :-
    !.
get_word2(_In, end_of_file, X, X) :-
    !.
get_word2(In, ' ', '', Result) :-
    !,
    get_word(In, '', Result).
get_word2(In, '\n', '', Result) :-
    !,
    get_word(In, '', Result).
get_word2(In, '\t', '', Result) :-
    !,
    get_word(In, '', Result).

get_word2(_, ' ', Word, Word) :- !.
get_word2(_, '\n', Word, Word) :- !.
get_word2(_, '\t', Word, Word) :- !.

get_word2(In, C, Word, Result) :-
    get_char(In, C2),
    atom_concat(Word, C, Word2),
    get_word2(In, C2, Word2, Result).
    
