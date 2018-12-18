
:- use_module(library(must_sanity)).
:- use_module((attvar_reader)).

rtest:- rtrace,test.

:- read_attvars(false).

test(0):- deserialize_attvars(sk_in(avar([vn='Ex'], [sk='SKF-666'])),O),copy_term(O,OO,PP),display(O),nl,display(OO),nl,display(PP),nl.

:- read_attvars(true).

test:- forall(test(_),true).

:- listing(test(_)).
