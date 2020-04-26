%example usage
%store 3 words using the keys [3, 4, 5]. then look up the word stored under key 4.

%add(3, 'hi', nil, X),
%add(4, 'hello', X, X2),
%add(5, 'greetings', X2, X3),
%get(4, X3, V).

p(9117007991229937).
p2(21630080583071).
path_size(32).%in bits

%can store about 2^(path_size/2) elements until we get our first collision, according to the birthday problem.

hash(K, K2) :-
    %not cryptographically secure! an attacker could make this tree unbalanced.
    %pseudo-random number generator using your key as the seed.
    p(Prime),
    p2(Prime2),
    K2 is (K * Prime) mod Prime2.

key2path(K, P) :-
    %This is converting K to binary.
    hash(K, K2),
    path_size(Path),
    key2path(K2, P, Path).
key2path(_, [], 0).
key2path(K, [H|T], N) :-
    H is K mod 2,
    K2 is K div 2,
    M is N - 1,
    key2path(K2, T, M).

add(K,V,T,U) :-
    %store a key-value pair in the dictionary.
    key2path(K, P),
    add2(P, V, T, U).

%an empty tree is 'nil', so this is adding the first element to a new tree.
add2(K,V,nil,leaf(K, V)).

%a tree with just one element is a 'leaf'. so this is how we add our second element to a tree.
%if our new leaf is following the same path as an existing one
add2([0|K],V,leaf([0|K2], V2), stem(S, nil)) :-
    add2(K, V, leaf(K2, V2), S).
add2([1|K],V,leaf([1|K2], V2), stem(nil, S)) :-
    add2(K, V, leaf(K2, V2), S).
%if our new leaf is splitting away from the path of the existing one
add2([0|K],V,leaf([1|K2], V2),
    stem(leaf(K, V), leaf(K2, V2))).
add2([1|K],V,leaf([0|K2], V2),
    stem(leaf(K2, V2), leaf(K, V))).

%this is how we add an element to a tree with more than 2 elements.
add2([0|K], V, stem(A, B), stem(S, B)) :-
    add2(K, V, A, S).
add2([1|K], V, stem(A, B), stem(A, S)) :-
    add2(K, V, B, S).
    
get(K, T, V) :-
    %use a key to look up a value.
    key2path(K, P),
    get2(P, T, V).
get2(P, leaf(P, V), V).
get2([0|P], stem(A, _), V) :-
    get2(P, A, V).
get2([1|P], stem(_, B), V) :-
    get2(P, B, V).

