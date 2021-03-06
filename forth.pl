%forth interpreter written in prolog.
%supports custom word definitions.
%each word definition can have 1 layer of if-else-then statements.

test(X) :-
    forth('first.forth', X).
forth(OldLoc, Result) :-
    open(OldLoc, read, In),
    forth2(In, [], [], nil, nil, Result),
    close(In).
forth2(_In, [end_of_file|S], _R, _HT, _, S) :-
    !.
forth2(In, S, R, HT, Funs, F) :-
    read_word(In, '', Word),
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
    read_function(In, Funs, Funs2).
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
    !,
    process_words(Value, S, R, HT, Funs, S2, R2, HT2, Funs2, In).

process_words([], S, R, HT, Funs, S, R, HT, Funs, _).
process_words([if|T], [0|S], R, HT, Funs, S2, R2, HT2, Funs2, In) :-
    !,
    remove_till(else, T, T2),
    remove_word(then, T2, T3),
    process_words(T3, S, R, HT, Funs, S2, R2, HT2, Funs2, In).
process_words([if|T], [_|S], R, HT, Funs, S2, R2, HT2, Funs2, In) :-
    remove_gap(else, then, T, T2),
    process_words(T2, S, R, HT, Funs, S2, R2, HT2, Funs2, In).
process_words([Word|T], S, R, HT, Funs, S2, R2, HT2, Funs2, In) :-
    %atom(Word),
    %numberify_atom(Word, Name),
    %get(Name, Funs, Value),
    ((%Value = empty,
      !,
      process_word(Word, S, R, HT, Funs, S3, R3, HT3, Funs3, In),
      process_words(T, S3, R3, HT3, Funs3, S2, R2, HT2, Funs2, In));
     ((not(Value = empty),
       append(Value, T, T2),
       !,
       process_words(T2, S, R, HT, Funs, S2, R2, HT2, Funs2, In)))).

read_function(In, F1, F2) :-
    read_word(In, '', Name0),
    numberify_atom(Name0, Name),
    read_function2(In, [], F),
    add(Name, F, F1, F2).
read_function2(In, Fun, Result) :-
    read_word(In, '', Word),
    read_function3(In, Fun, Result, Word).
read_function3(_, Fun, Fun, ';').
read_function3(In, Fun, Result, Word) :-
    append(Fun,[Word],Fun2),
    read_function2(In, Fun2, Result).
    
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

read_word(In, P, Result) :-
    get_char(In, C),
    read_word2(In, C, P, Result).
read_word2(_In, end_of_file, '', end_of_file).
read_word2(_In, end_of_file, X, X).
read_word2(In, C, '', Result) :-
    white_space(C),
    read_word(In, '', Result).
read_word2(In, '%', '', Result) :-
    ignore_comment('\n', In),
    read_word(In, '', Result).
read_word2(_, C, Word, Word) :-
    white_space(C),
    !.
read_word2(In, '%', Word, Word) :-
    ignore_comment('\n', In).
read_word2(In, C, Word, Result) :-
    get_char(In, C2),
    atom_concat(Word, C, Word2),
    read_word2(In, C2, Word2, Result).

ignore_comment(X, In) :-
    ignore_comment(X, In, ' ').
ignore_comment(X, _, X).
ignore_comment(_, _, end_of_file).
ignore_comment(X, In, _) :-
    get_char(In, C),
    ignore_comment(X, In, C).

remove_till(X, [X|R], R).
remove_till(X, [_|T], R) :-
    remove_till(X, T, R).
remove_word(X, [X|T], T) :- !.
remove_word(X, [H|T1], [H|T2]) :-
    remove_word(X, T1, T2).
remove_gap(Start, End, [Start|T1], T2) :-
    remove_till(End, T1, T2).
remove_gap(Start, End, [H|T1], [H|T2]) :-
    remove_gap(Start, End, T1, T2).
    
white_space(' ').
white_space('\n').
white_space('\t').


