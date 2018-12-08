# Call A Goal On *Each* Redo


 Installation using SWI-Prolog 7.1 or later:

   `?- pack_install('https://github.com/TeamSPoon/each_call_cleanup.git').`



Source code available and pull requests accepted at
http://github.com/TeamSPoon/each_call_cleanup

Example usages: 

```prolog

 
 with_prolog_flag(Flag,Value,Goal):- 
    current_prolog_flag(Flag,Was),
     each_call_cleanup( 
	 set_prolog_flag(Flag,Value), 
	  Goal, 
	   set_prolog_flag(Flag,Was)). 
 
 

 % notrace/1 that is not like once/1 
 quietly(Goal):- 
    tracing ->
      each_call_cleanup(notrace,Goal,trace);
      Goal.
    
            
 

 % Trace non interactively 
 rtrace(Goal):- 
    ( tracing-> Undo=trace ; Undo = notrace ), 
    '$leash'(OldL, OldL), '$visible'(OldV, OldV), 
    each_call_cleanup( 
         (notrace,visible(+all),leash(-all),leash(+exception),trace), 
         Goal,
         (notrace,'$leash'(_, OldL),'$visible'(_, OldV),Undo)).


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

