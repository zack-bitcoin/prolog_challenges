
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
process_word('*', [A|[B|S]], R, HT, Funs,
             [C|S], R, HT, Funs, _) :-
    C is A * B.
process_word('div', [A|[B|S]], R, HT, Funs,
             [C|S], R, HT, Funs, _) :-
    C is B div A.
process_word('-', [A|[B|S]], R, HT, Funs,
             [C|S], R, HT, Funs, _) :-
    C is B - A.
process_word('mod', [A|[B|S]], R, HT, Funs,
             [C|S], R, HT, Funs, _) :-
    C is B mod A.
process_word(':', S, R, HT, Funs,
             S, R, HT, Funs2, In) :-
    get_function(In, Funs, Funs2).
process_word(Word, S, R, HT, Funs,
             S2, R, HT, Funs, _) :-
    can_number(Word),
    number_atom(N, Word),
    S2 = [N|S].
process_word(Word, S, R, HT, Funs,
             S2, R2, HT2, Funs2, In) :-
    %check if this word is a defined function.
    numberify_atom(Word, Name),
    get(Name, Funs, Value),
    not(Value == empty),
    process_words(Value, S, R, HT, Funs, S2, R2, HT2, Funs2, In).


process_words([], S, R, HT, Funs, S, R, HT, Funs, _).
process_words([then|T], S, R, HT, Funs, S2, R2, HT2, Funs2, In) :-
    process_words(T, S, R, HT, Funs, S2, R2, HT2, Funs2, In).
process_words([if|T], [0|S], R, HT, Funs, S2, R2, HT2, Funs2, In) :-
    !,
    tillX_list(else, T, T2),
    process_words(T2, S, R, HT, Funs, S2, R2, HT2, Funs2, In).
process_words([if|T], [_|S], R, HT, Funs, S2, R2, HT2, Funs2, In) :-
    process_words(T, S, R, HT, Funs, S2, R2, HT2, Funs2, In).
process_words([else|T], S, R, HT, Funs, S2, R2, HT2, Funs2, In) :-
    !,
    tillX_list(then, T, T2),
    process_words(T2, S, R, HT, Funs, S2, R2, HT2, Funs2, In).
process_words([Word|T], S, R, HT, Funs, S2, R2, HT2, Funs2, In) :-
    process_word(Word, S, R, HT, Funs, S3, R3, HT3, Funs3, In),
    process_words(T, S3, R3, HT3, Funs3, S2, R2, HT2, Funs2, In).

not(X) :-
    \+ X.

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
    
%can this atom be converted to a number?
can_number(A) :-
    atom_chars(A, L),
    can_number2(L).
can_number2([]).
can_number2([H|T]) :-
    char_code(H, N),
    N > 47,
    N < 58,
    can_number2(T).

%the hashtable can only store by integer key. So we need a way to deterministically convert atoms into integers, and we don't want different atoms to collide with the same integer.
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
get_word2(In, '%', '', Result) :-
    !,
    tillX('\n', In),
    get_word(In, '', Result).

get_word2(_, ' ', Word, Word) :- !.
get_word2(_, '\n', Word, Word) :- !.
get_word2(_, '\t', Word, Word) :- !.
get_word2(In, '%', Word, Word) :-
    !,
    tillX('\n', In).

get_word2(In, C, Word, Result) :-
    get_char(In, C2),
    atom_concat(Word, C, Word2),
    get_word2(In, C2, Word2, Result).


tillX(X, In) :-
    tillX(X, In, ' ').
tillX(X, _, X).
tillX(_, _, end_of_file).
tillX(X, In, _) :-
    get_char(In, C),
    tillX(X, In, C).

tillX_list(X, [X|R], R).
tillX_list(X, [_|T], R) :-
    tillX_list(X, T, R).
    
    
