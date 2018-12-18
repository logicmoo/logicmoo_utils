
:- use_module(library(each_call_cleanup)).


test(0):- each_call_cleanup(writeln(start),(between(1,3,X),writeln(X)), writeln(end)),fail. 

% todo - fix so ref is bound
test(1):- each_call_cleanup(asserta(scce0,REF),(between(1,3,X),writeln(REF:X)),erase(REF)),fail. 


:- listing(test(_)).
