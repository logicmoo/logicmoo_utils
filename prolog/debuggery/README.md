# Trace with your eyeballs instead of your fingers

This is a miniture part of a very large debugging library...
Some better parts of that library are not yet added. 

Here are current uses:

Wrap `quietly/1` over parts of your code you no longer need to stepped thru.
This is a *nondeterministic* version of notrace/1!


Wrap `must/1` over parts of your code you do not trust yet.
If your code fails.. it will rewind to your entry block (at the scope of this declaration) and invoke rtrace/1 .
If there are 50 steps to your code, it will save you from pushing `creep` 50 times.  
Instead it turns off the leash to allow you to trace with your eyeballs instead of your fingers


Wrap `sanity/1` over parts of your code you want to turn on/off that is only usefull for slow debugging


Wrap `nop/1` over parts of your code you do not want to quickly comment out yet not break syntax.



# Installation using SWI-Prolog 7.5.x or later (due to duplicate transitive deps failing in earlier versions):

    `?- pack_install('https://github.com/TeamSPoon/must_trace.git'). `


TODO Document all the preds!
TODO More Examples!


Source code available and pull requests accepted at
http://github.com/TeamSPoon/must_trace


```prolog

?- use_module(library(must_trace)).
true.

?- rtrace(member(X,[1,2,3])).
   Call: (9) lists:member(_8730, [1, 2, 3])
   Unify: (9) lists:member(_8730, [1, 2, 3])
   Exit: (9) lists:member(1, [1, 2, 3])
X = 1 ;
   Redo: (9) lists:member(_8730, [1, 2, 3])
   Exit: (9) lists:member(2, [1, 2, 3])
X = 2 ;
   Redo: (9) lists:member(_8730, [1, 2, 3])
   Exit: (9) lists:member(3, [1, 2, 3])
X = 3.

?-  rtrace(member(X,[1,2,3])),member(Y,[4,5]).
   Call: (10) lists:member(_10508, [1, 2, 3])
   Unify: (10) lists:member(_10508, [1, 2, 3])
   Exit: (10) lists:member(1, [1, 2, 3])
X = 1,
Y = 4 ;
X = 1,
Y = 5 ;
   Redo: (10) lists:member(_10508, [1, 2, 3])
   Exit: (10) lists:member(2, [1, 2, 3])
X = 2,
Y = 4 ;
X = 2,
Y = 5 ;
   Redo: (10) lists:member(_10508, [1, 2, 3])
   Exit: (10) lists:member(3, [1, 2, 3])
X = 3,
Y = 4 ;
X = 3,
Y = 5.

?- rtrace((member(X,[1,2,3]),member(Y,[4,5]))).
   Call: (10) lists:member(_11854, [1, 2, 3])
   Unify: (10) lists:member(_11854, [1, 2, 3])
   Exit: (10) lists:member(1, [1, 2, 3])
   Call: (10) lists:member(_11872, [4, 5])
   Unify: (10) lists:member(_11872, [4, 5])
   Exit: (10) lists:member(4, [4, 5])
X = 1,
Y = 4 ;
   Redo: (10) lists:member(_11872, [4, 5])
   Exit: (10) lists:member(5, [4, 5])
X = 1,
Y = 5 ;
   Redo: (10) lists:member(_11854, [1, 2, 3])
   Exit: (10) lists:member(2, [1, 2, 3])
   Call: (10) lists:member(_11872, [4, 5])
   Unify: (10) lists:member(_11872, [4, 5])
   Exit: (10) lists:member(4, [4, 5])
X = 2,
Y = 4 ;
   Redo: (10) lists:member(_11872, [4, 5])
   Exit: (10) lists:member(5, [4, 5])
X = 2,
Y = 5 ;
   Redo: (10) lists:member(_11854, [1, 2, 3])
   Exit: (10) lists:member(3, [1, 2, 3])
   Call: (10) lists:member(_11872, [4, 5])
   Unify: (10) lists:member(_11872, [4, 5])
   Exit: (10) lists:member(4, [4, 5])
X = 3,
Y = 4 ;
   Redo: (10) lists:member(_11872, [4, 5])
   Exit: (10) lists:member(5, [4, 5])
X = 3,
Y = 5.

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


