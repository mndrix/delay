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

# Supporting Additional Predicates

`library(delay)` comes with support for some built-in predicates.  To
add support other predicates, define clauses for the multifile
predicate `delay:mode/1`.

For example, if the module `utils` exports `helpful/2` and that
predicate requires at least one of its arguments to be ground, you can
add delay support with

    :- multifile delay:mode/1.
    delay:mode(utils:helpful(ground,_)).
    delay:mode(utils:helpful(_,ground)).

`library(delay)` first looks for mode information under the calling
module's name.  If none is found, it looks for it under the exporting
module's name.  The example above defines mode information for all
users of `utils:helpful/2`.  If your module `mine` imports `helpful/2`
and you only want the mode declarations to have effect locally, you
can do this instead:

    :- multifile delay:mode/1.
    delay:mode(mine:helpful(ground,_)).
    delay:mode(mine:helpful(_,ground)).

Don't worry about adding `mode/1` declarations for predicates that
already have them.  Redundant mode declarations are ignored.

If you create mode declarations for built-in predicates, please
consider contributing them as a pull request to this library (see
below).  That way, other users can benefit too.

# Caution with Cuts

When using `delay/1`, be certain to think carefully about all
surrounding cuts.  Delaying over a cut changes the semantics.

This library might someday offer development-time warnings to bring
these to your attention. However, such a tool doesn't now exist and is
unlikely to discover all circumstances in which delay and cut interact
unexpectedly.

# See Also

SWI-Prolog's [SICSTUS compatibility
layer](http://www.swi-prolog.org/pldoc/doc/swi/library/dialect/sicstus.pl)
includes the `block/1` directive.  That directive fills a niche very similar to `delay/1`.

Here are some advantages of `delay/1` over `block/1`:

  * works with built-in predicates
  * works with foreign predicates
  * works with dynamic predicates
  * more flexible mode language

One disadvantage is that `delay/1` is not compatible with existing SICSTUS
code.

# Installation

Using SWI-Prolog 6.3 or later:

    ?- pack_install(delay).

This module uses [semantic versioning](http://semver.org/).

Source code is available at http://github.com/mndrix/delay


@author Michael Hendricks <michael@ndrix.org>
@license BSD
