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

# Changes in this Version

  * Initial public release

# Installation

Using SWI-Prolog 6.3 or later:

    ?- pack_install(delay).

Source code available and pull requests accepted at
http://github.com/mndrix/delay

@author Michael Hendricks <michael@ndrix.org>
@license BSD
