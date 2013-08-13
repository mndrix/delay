:- module(delay, [ delay/1
                 , when_proper_list/2
                 ]).
:- use_module(library(when), [when/2]).

% define acceptable modes for each predicate.
mode(atom_codes(ground, _)).
mode(atom_codes(_, ground)).

mode(functor(nonvar,_,_)).
mode(functor(_,ground,ground)).

mode(number_codes(ground,_)).
mode(number_codes(_,ground)).

mode(phrase(ground,_)).
mode(phrase(_,ground)).

mode(phrase(ground,_,_)).
mode(phrase(_,ground,_)).

mode(plus(ground,ground,_)).
mode(plus(ground,_,ground)).
mode(plus(_,ground,ground)).

mode(succ(ground,_)).
mode(succ(_,ground)).


%%	delay(:Goal)
%
%   Like `call(Goal)` but postpones execution until Goal's arguments are
%   bound enough to avoid errors like: "Arguments are not sufficiently
%   instantiated". This is currently realized with attributed
%   variables and when/2, so execution timing is identical. For example,
%
%       t :-
%           delay(atom_codes(A,C)),
%           A = hello,
%           C == "hello".
%
%   does not throw an exception on the first line.
%   One is simply declaring that `A` and `C` have a given relationship
%   without stating when the predicate (atom_codes/2) will
%   execute. This declarative style is especially valuable when
%   different modes of a predicate require different goal order.
%
%   The following predicates are currently supported:
%
%     * atom_codes/2
%     * functor/3
%     * length/2
%     * number_codes/2
%     * phrase/2
%     * phrase/3
%     * plus/3
%     * succ/2
%     * univ/3
%
%   `delay(length(L,Len))` warrants additional explanation. length/2
%   doesn't throw instantiation exceptions. It simply iterates all
%   possible lists and their respective lengths. This isn't always
%   ideal. Using delay/1 with length/2 yields the same semantics but
%   performs much less backtracking.  It waits until either `L`
%   or `Len` is bound then length/2 evaluates without any choicepoints.
%   `L` must become a proper list to trigger, so incrementally binding
%   its head is OK.
%
%   `delay(univ(Term,Name,Args))` is like =../2 but it works when all
%   arguments are variables.
:- dynamic delay/1.
delay(length(L,Len)) :-
    var(L),
    var(Len),
    !,
    when( (nonvar(Len) ; nonvar(L)), delay(length(L,Len)) ).
delay(length(L,Len)) :-
    nonvar(Len),
    !,
    length(L,Len).
delay(length(L,Len)) :-
    % nonvar(L)
    !,  % cut choicepoints in clauses created by macro expansion
    when_proper_list(L, length(L,Len)).

delay(univ(Term, Name, Args)) :-
    var(Term),
    ( var(Name) ; var(Args) ),
    !,
    when( ( nonvar(Term)
          ; nonvar(Name), nonvar(Args)
          )
        , delay(univ(Term,Name,Args))
        ).
delay(univ(Term, Name, Args)) :-
    nonvar(Term),
    !,
    Term =.. [Name|Args].
delay(univ(Term,Name,Args)) :-
    % nonvar(Name),
    % nonvar(Args),
    !,  % cut choicepoints in clauses created by macro expansion
    when_proper_list(Args, Term=..[Name|Args]).

delay(Goal) :-
    % build a delay/1 clause to support Goal

    % generalize Goal into a clause Head
    functor(Goal, Name, Arity),
    functor(Head, Name, Arity),
    Head =.. [_|Args],

    % find all modes applicable to Head
    setof(Head, mode(Head), Modes),
    !,

    % convert Modes into a condition term that when/2 can use
    maplist(mode_to_condition(Args), Modes, Conditions),
    xfy_list(';', Condition, Conditions),

    % assert the new delay/1 clause, then call it
    asserta((
        delay(Head) :-
            when(Condition, Head),
            !
    )),
    delay(Goal).
delay(_Goal) :-
    throw('TODO instructions on making other goals delayable').


%%	mode_to_condition(+List, +Term, -WhenCondition)
%
%	Defines a relationship like this:
%
%	    mode_to_condition([X,Y], atom_codes(ground,_), ground(X))
mode_to_condition(HeadArgs, Mode, Condition) :-
    Mode =.. [_|ModeArgs],
    map_include(make_condition, ModeArgs, HeadArgs, Whens),
    xfy_list(',', Condition, Whens).


% convert a mode letter and argument variable into a when/2 condition
make_condition(X, _, _) :-
    var(X),
    !,
    fail.
make_condition(ground, X, ground(X)).
make_condition(nonvar, X, nonvar(X)).


%%	when_proper_list(List, Goal)
%
%   Delay executing Goal until List becomes a proper list. This
%   predicate is part of the internal implementation of delay/1 but it
%   may be useful to others so it's exported.
when_proper_list(List, Goal) :-
    var(List),
    !,
    when(nonvar(List), when_proper_list(List, Goal)).
when_proper_list([], Goal) :-
    call(Goal).
when_proper_list([_|T], Goal) :-
    when_proper_list(T, Goal).


% originally copied from library(list_util).
% I don't want this pack to depend on external libraries.
:- meta_predicate map_include(3, +, +, -).
:- meta_predicate map_include_(+,+,3,-).
map_include(F, La, Lb, L) :-
    map_include_(La, Lb, F, L).
map_include_([], [], _, []).
map_include_([Ha|Ta], [Hb|Tb], F, List0) :-
    ( call(F, Ha, Hb, H) ->
        List0 = [H|List]
    ; % otherwise ->
        List0 = List
    ),
    map_include_(Ta, Tb, F, List).


% originall copied from library(list_util).
% I don't want this pack to depend on external libraries.
xfy_list(Op, Term, [Left|List]) :-
    Term =.. [Op, Left, Right],
    xfy_list(Op, Right, List),
    !.
xfy_list(_, Term, [Term]).
