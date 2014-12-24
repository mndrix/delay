:- use_module(library(delay)).

:- use_module(library(tap)).

'is_dict/1' :-
    delay(is_dict(D)),
    D = foo{}.

'is_dict/2 with dict' :-
    delay(is_dict(D,Tag)),
    D = foo{},
    Tag == foo.

'is_dict/2 with tag' :-
    delay(is_dict(D,Tag)),
    Tag = foo,
    var(D),
    D = foo{}.

'dict_pairs/3 with dict' :-
    delay(dict_pairs(Dict,Tag,Pairs)),
    Tag = foo,
    var(Dict),
    var(Pairs),
    Dict = foo{1: hi},
    Pairs == [1-hi].

'dict_pairs/3 with pairs' :-
    delay(dict_pairs(Dict,Tag,Pairs)),
    Tag = foo,
    var(Dict),
    var(Pairs),
    Pairs = [1-hi],
    Dict == foo{1: hi}.
