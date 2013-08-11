:- module(delay, [ delay/1
                 , when_proper_list/2
                 ]).
:- use_module(library(when), [when/2]).

% TODO strip all macros from this code. i must work with mode/1 as it's
% written
%
% TODO replace 'g' and 'n' with 'ground' and 'nonvar', respectively
%
% TODO make sure that each delay/1 clause cuts before calling when/2.
% otherwise, when/2 can fail because all arguments are ground and the
% underlying goal fails. that would backtrack to the final delay/1
% clause and throw an exception. Be sure to write a failing test for
% this first, then make changes to make it pass. without a test, I'm
% sure I'll encounter this same corner case again.
%
% TODO have delay/1 consult mode/1 to build a when/2 goal then assert it
% as an additional clause on delay/1 (the assertion is an optimization)
%
% TODO change delay/1 into `meta_predicate delay(0)` and change mode/1
% so that each built-in predicate is prefaced with `system:`. Look
% at mode/1 with the module-qualified head. If there's no mode for
% that, call `predicate_property(Head, imported_from(Module))` to
% determine if this predicate is originally from another module.
% Perform the mode/1 look up using that module. If there's no mode/1
% declaration in either case, throw an error with instructions about how
% to add support for other predicates.
%
% TODO :- multifile delay/1.
%
% TODO at this point, one can just add `delay:mode(foo:bar(ground,_))`
% facts to support additional predicates.


% convert pleasant mode/1 facts into usable mode/2 facts
% from which code can generate delay/1 clauses.
term_expansion(mode(Head0), mode(Name/Arity, Modes)) :-
    functor(Head0, Name, Arity),
    Head0 =.. [Name|Modes].


% define acceptable modes for each predicate.
% 'g' means argument must be ground.
% 'n' means argument must be nonvar.
:- dynamic mode/2.
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
:- dynamic delay/1.  % let macro expansion add predicates
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
