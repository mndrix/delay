:- module(delay, [ delay/1
                 , when_proper_list/2
                 ]).

:- dynamic mode/2.

% convert pleasant mode/1 facts into usable mode/2 facts
% from which code can generate delay/1 clauses.
term_expansion(mode(Head0), mode(Name/Arity, Modes)) :-
    functor(Head0, Name, Arity),
    Head0 =.. [Name|Modes].

% define acceptable modes for each predicate.
% 'g' means argument must be ground.
% 'n' means argument must be nonvar.
mode(atom_codes(g, _)).
mode(atom_codes(_, g)).

mode(functor(n,_,_)).
mode(functor(_,g,g)).

mode(number_codes(g,_)).
mode(number_codes(_,g)).

mode(phrase(g,_)).
mode(phrase(_,g)).

mode(phrase(g,_,_)).
mode(phrase(_,g,_)).

mode(plus(g,g,_)).
mode(plus(g,_,g)).
mode(plus(_,g,g)).

mode(succ(g,_)).
mode(succ(_,g)).


:- dynamic delay/1.
% TODO documentation
% TODO describe length/2 and univ/3 since they're different from the rest
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


% TODO documentation
when_proper_list(List, Goal) :-
    var(List),
    !,
    when(nonvar(List), when_proper_list(List, Goal)).
when_proper_list([], Goal) :-
    call(Goal).
when_proper_list([_|T], Goal) :-
    when_proper_list(T, Goal).


% convert a mode letter and argument variable into a when/2 condition
make_mode(X, _, _) :-
    var(X),
    !,
    fail.
make_mode(g, X, ground(X)).
make_mode(n, X, nonvar(X)).


% originally copied from library(list_util).
% I don't want this pack to depend on external libraries.
:- meta_predicate map_include(3, +, +, -).
:- meta_predicate delay:map_include_(+,+,3,-).
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


build_predicates :-
    setof(Name/Arity, C^mode(Name/Arity,C), Indicators),
    forall( member(Name/Arity, Indicators)
          , ( setof(Mode, mode(Name/Arity, Mode), Modes)
            , build_predicate(Name/Arity, Modes)
            )
          ),
    assertz((
        delay(_Goal) :-
            throw('TODO instructions on making other goals delayable')
    )),
    retractall(mode(_,_)),
    compile_predicates([delay/1, mode/2]).

build_predicate(Name/Arity, Modes) :-
    length(Args, Arity),
    Head =.. [Name|Args],
    maplist(build_condition(Args), Modes, ConditionList),
    xfy_list(';', Conditions, ConditionList),
    assertz((
        delay(Head) :-
            when(Conditions, Head), !
    )).

build_condition(Args, Mode, Condition) :-
    map_include(make_mode, Mode, Args, ConditionList),
    xfy_list(',', Condition, ConditionList).

:- build_predicates.
