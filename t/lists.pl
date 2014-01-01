:- use_module(library(delay)).

% define helper predicates here

:- use_module(library(tap)).

'reverse/2 forwards' :-
    delay(reverse(L,R)),
    L = [a,b,c],
    R == [c,b,a].

'reverse/2 backwards' :-
    delay(reverse(L,R)),
    R = [c,b,a],
    L == [a,b,c],
    !.  % trim choicepoint from reverse/2 in backwards mode


'same_length/2 forwards' :-
    delay(same_length(A,B)),
    A = [a,b,c,d],
    length(B,4).

'same_length/2 backwards' :-
    delay(same_length(A,B)),
    B = [a,b],
    length(A,2).
