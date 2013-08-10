# Synopsis

    :- use_module(library(delay)).
    main :-
        % describe a relationship between variables.
        % none of them need values yet.
        delay(atom_codes(Atom,Codes)),
        
        % now we can bind a value...
        Codes = "hello",
        
        % and the other one receives a value too
        Atom == hello.

# Description

Many Prolog predicates (`succ/2`, `plus/3`, `atom_codes/2`, etc) describe a
relationship between values.  However, if called with variables whose values
are not yet known, they throw a "not sufficiently instantiated" exception.
Unfortunately, this imposes a strict order of execution and often requires one
to write additional clauses that differ only in goal order.

By wrapping these predicates in `delay/1`, they become purely declarative.
Their execution order no longer matters and happens as soon as it reasonably
can.

For example, imagine a simple DCG for saying hello.  We want the subject of
our greeting to be specified as an atom.  So we might write

    hello(Whom) -->
        { atom_codes(Whom, Codes) },
        "Hello, ",
        string(Codes).

That works fine if we run it forward

    ?- phrase(hello(john), X).
    X = "Hello, john".

but not if we run it backwards

    ?- phrase(hello(Whom), "Hello, john").
    ERROR: atom_codes/2: Arguments are not sufficiently instantiated

Instead of writing a second clause for `hello//1`, in which the goals are
reordered, we use `delay/1`

    hello(Whom) -->
        { delay(atom_codes(Whom, Codes)) },
        "Hello, ",
        string(Codes).

and now we get what we wanted

    ?- phrase(hello(Whom), "Hello, john").
    Whom = john ;
    false.

# Changes in this Version

  * Add a list of supported predicates

# Installation

Using SWI-Prolog 6.3 or later:

    ?- pack_install(delay).

# Contributing

Source code is available at http://github.com/mndrix/delay

To add support for additional built-in predicates, add clauses to the
mode/1 predicate in `prolog/delay.pl`.  There should be one clause
for each mode in which the built-in can operate. Submit your changes
as a pull request.


@author Michael Hendricks <michael@ndrix.org>
@license BSD
