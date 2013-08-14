:- use_module(library(delay)).

% define helper predicates here

:- use_module(library(tap)).

% add tests showing common usage
'atom_codes/2 forwards' :-
    delay(atom_codes(A,C)),
    A = foo,
    C == "foo".

'atom_codes/2 backwards' :-
    delay(atom_codes(A,C)),
    C = "foo",
    A == foo.


'length/2 forwards' :-
    delay(length(L, Len)),
    L = [H|T],  % partially constructing the list ...
    var(Len),   % ... does not trigger the coroutine
    H = a,
    var(Len),
    T = [rest,goes,here],   % fully constructing it ...
    Len =:= 4.              % ... does

'length/2 backwards' :-
    delay(length(L, Len)),
    Len = 3,
    L = [_,_,_].


'univ forwards' :-
    delay(univ(Term, Name, Args)),
    Term = a(b,c,d),
    Name == a,
    Args == [b,c,d].

'univ backwards' :-
    delay(univ(Term, Name, Args)),
    Name = a,
    var(Term),  % not enough info to instantiate yet
    Args = [b,c,d],
    Term == a(b,c,d).

'univ backwards: piecewise arguments' :-
    delay(univ(Term, Name, Args)),
    Name = a,
    var(Term),  % not enough info to instantiate yet
    Args = [b|Tail],
    var(Term),  % still not enough info
    Tail = [c,d],
    Term == a(b,c,d).
