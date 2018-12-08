# no_repeats
New ways to avoid duplicate solutions


```prolog

:- pack_install('https://github.com/TeamSPoon/no_repeats.git').

```


```prolog

?- use_module(library(no_repeats)).

?- no_repeats( X , member(X-Y,[3-2,1-4,1-5,2-1])).
% X = 3, Y = 2 ;
% X = 1, Y = 4 ;
% X = 2, Y = 1.


 ?- no_repeats(member(X,[3,1,1,1,3,2])).
% X = 3 ;
% X = 1 ;
% X = 2.


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


