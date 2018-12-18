
:- use_module(library(sanity_must)).
:- ensure_loaded(library(ansimesg)).

:- set_prolog_flag(must_saftey,3).
:- set_prolog_flag(must_debug,3).
:- set_prolog_flag(must_speed,3).

:- set_prolog_flag(must_type,keep_going).

test(0):- must(\+ fail).

test(1):- must_once(fail).

all_tests:- forall(test(_),true).

:- listing(test(_)).
