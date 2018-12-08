
```prolog
:- use_module(library(lmu/file_scope)).
:- set_prolog_flag_until_eof(access_level,system).
:- assert_until_eof(( term_expansion(.,.) :- .. )).

```


# Utilities to open various objects for read/write

```prolog
:- use_module(library(lmu/filestreams)).

```




# mass wildcard expansions
```prolog
:- use_module(library(lmu/filesystem)).

```
