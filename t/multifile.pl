:- use_module(library(delay)).

% provide mode information for a locally-defined predicate
:- multifile delay:mode/1.
delay:mode(user:repeat(ground,ground,_)).
delay:mode(user:repeat(ground,_,ground)).
repeat(N, Code, Atom) :-
    length(Codes, N),
    maplist(=(Code), Codes),
    atom_codes(Atom, Codes).

:- use_module(library(tap)).

'building an atom' :-
    delay(repeat(N,C,A)),
    N = 3,
    var(A),  % still unbound
    C = 0'x, % ' for syntax colors
    A == xxx.

'finding the character' :-
    N = 4,
    delay(repeat(N,C,A)),
    var(C),
    var(A),
    A = yyyy,
    C == 0'y. % ' for syntax colors
