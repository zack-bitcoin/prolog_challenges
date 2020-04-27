
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
    process_word(Word, In, S, R, HT, Funs, F).

process_word(end_of_file, _In, S, _R, _HT, _, S).
process_word('dup', In, [H|S], R, HT, Funs, F) :-
    forth2(In, [H|[H|S]], R, HT, Funs, F), !.
process_word('drop', In, [_|S], R, HT, Funs, F) :-
    forth2(In, S, R, HT, Funs, F), !.
process_word('+', In, [A|[B|S]], R, HT, Funs, F) :-
    C is A + B,
    forth2(In, [C|S], R, HT, Funs, F), !.
process_word(Word, In, S, R, HT, Funs, F) :-
    number_atom(N, Word),
    forth2(In, [N|S], R, HT, Funs, F).


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
    
