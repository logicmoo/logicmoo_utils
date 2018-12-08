# with_thread_local
Call a Goal with local assertions


Installation using SWI-Prolog 7.1 or later:

    `?- pack_install('https://github.com/TeamSPoon/with_thread_local.git'). `


Source code available and pull requests accepted at http://github.com/logicmoo/with_thread_local


   locally_each( :Effect, :Call) is nondet.
 
   Temporally have :Effect (see locally/2)
 
   But Ensure Non-determism is respected (effect is undone between _each_ Redo)
 
   uses each_call_cleanup/3 instead of setup_call_cleanup/3 (slightly slower?)
 
  for example,
 
   locally_each/2 works (Does not throw)
```prolog
  ?- current_prolog_flag(xref,Was), 
      locally_each(set_prolog_flag(xref,true),
      assertion(current_prolog_flag(xref,true));assertion(current_prolog_flag(xref,true))),
      assertion(current_prolog_flag(xref,Was)),fail.
```
 
   locally/2 is little less carefull so it should _not_ work (it throws instead)
```prolog
?- current_prolog_flag(xref,Was), 
    locally(set_prolog_flag(xref,true),
    assertion(current_prolog_flag(xref,true));assertion(current_prolog_flag(xref,true))),
    assertion(current_prolog_flag(xref,Was)),fail.
```

   locally( :Effect, :Call) is nondet.
 
  Effect may be of type:
 
   set_prolog_flag -
      Temporarily change prolog flag
 
   op/3 - 
      change op
 
   $gvar=Value -
      set a global variable
 
   Temporally (thread_local) Assert some :Effect 
 
   use locally_each/3 if respecting Non-determism is important 
  (slightly slower?)
 
```prolog
  ?- current_prolog_flag(xref,Was), 
      locally(set_prolog_flag(xref,true),
      assertion(current_prolog_flag(xref,true))),
      assertion(current_prolog_flag(xref,Was)). 
```




# Some TODOs

Document this pack!

Write tests

Untangle the 'pack' install deps 
(Moving predicates over here from logicmoo_base)


# Not _obligated_ to maintain a git fork just to contribute

Dislike having tons of forks that are several commits behind the main git repo?

Be old school - Please ask to be added to TeamSPoon and Contribute directly !

Still, we wont stop you from doing it the Fork+PullRequest method

# [BSD 2-Clause License](LICENSE.md)

Copyright (c) 2017, 
TeamSPoon and Douglas Miles <logicmoo@gmail.com> 
All rights reserved.


