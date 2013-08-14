:- use_module(library(delay)).
:- use_module(library(dcg/basics)).

:- use_module(library(tap)).

parsing :-
    delay(phrase(Grammar,Codes)),
    Codes = "937",
    Grammar = integer(N),
    N =:= 937.

generating :-
    delay(phrase(Grammar,Codes)),
    Grammar = integer(N),
    N = 937,
    Codes == "937".
