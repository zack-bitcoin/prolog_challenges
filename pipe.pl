
test0 :-
    %read one character, write it somewhere else.
    open(oldFile, read, In),
    get_code(In, C),
    open(newFile, append, Out),
    put_char(Out, 'a'),
    put_code(Out, C),
    close(In),
    close(Out).


test :-
    %read characters from one file, write to the other.
    test(oldFile, newFile).
test(OldLoc, NewLoc) :-
    open(OldLoc, read, In),
    open(NewLoc, write, Out),
    test2(In, Out),
    close(In),
    close(Out).
test2(In, Out) :-
    get_code(In, C),
    test2b(C, In, Out).
test2b(-1, _, _) :- !.
test2b(C, In, Out) :-
    put_code(Out, C),
    put_code(Out, C),
    test2(In, Out).

    
