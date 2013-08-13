:- use_module(library(delay)).

:- use_module(library(tap)).

% calling a delayed predicate in which all arguments are bound
% should behave the same as if the predicate were not delayed.
'statically false invocation' :-
    \+ delay(atom_codes(atom,"codes")).
