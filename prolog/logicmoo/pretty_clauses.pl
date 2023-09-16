/* Part of LogicMOO Base Logicmoo Debug Tools
% ===================================================================
% File '$FILENAME.pl'
% Purpose: An Implementation in SWI-Prolog of certain debugging tools
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: '$FILENAME.pl' 1.0.0
% Revision: $Revision: 1.1 $
% Revised At:  $Date: 2002/07/11 21:57:28 $
% Licience: LGPL
% ===================================================================
*/
% File: /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/util/logicmoo_util_filestreams.pl
:- module(pretty_clauses,
          [ pprint_tree/2,        % +Term, +Options
           bfly_term//2,          % +Term, +Options
           print_tree_nl/1,
           guess_pretty/1,
           term_is_ansi/1,
           write_keeping_ansi/1,
           make_pretty/2,
         %  term_varnames/2,
   color_format_maybe/3,print_tree_unit/1,print_as_tree/1,current_print_write_options/1,mort/1,
   print_tree_with_final/2,
   is_webui/0,
   print_tree_with_final/3]).

/*
:- multifile '$exported_op'/3. 
:- dynamic '$exported_op'/3. 
:- discontiguous '$exported_op'/3. 
'$exported_op'(_,_,_):- fail.
*/

:- multifile '$autoload'/3. 
:- discontiguous '$autoload'/3.
:- dynamic '$autoload'/3.
'$autoload'(_,_,_):- fail.

:- system:use_module(library(debuggery/bugger)).
%:- system:reexport(library(must_sanity)).
:- include(portray_vars).
:- include(butterfly_console).
/** <module> Pretty Print Prolog terms in plain or HTML

This file is primarily designed to   support running Prolog applications
over the web. It provides a   replacement for write_term/2 which renders
terms as structured HTML.

This module is a first  start  of   what  should  become a full-featured
pretty printer for Prolog  terms  with   many  options  and  parameters.
Eventually,  it  should  replace  portray_clause/1   and  various  other
special-purpose predicates.

@tbd This is just a quicky. We  need proper handling of portray/1, avoid
printing very long terms  multiple   times,  spacing (around operators),
etc.

@tbd Use a record for the option-processing.

@tbd The current approach is far too simple, often resulting in illegal
     terms.

@author Douglas R. Miles
@license LGPL

*/

:- define_into_module([
 our_pengine_output/1,
 pprint_tree/2,        % +Term, +Options
           bfly_term//2,          % +Term, +Options
           print_tree_nl/1,
           guess_pretty/1,
           term_is_ansi/1,
           write_keeping_ansi/1,
           make_pretty/2,
         %  term_varnames/2,
   color_format_maybe/3,print_tree_unit/1,print_as_tree/1,current_print_write_options/1,mort/1,
   print_tree_with_final/2,
   is_webui/0,
   print_tree_with_final/3]).

:- set_module(class(library)).

:- autoload(library(http/html_write),[html/3,print_html/1]).
%:- autoload(library(lynx/html_text),[html_text/2]).
:- autoload(library(option),
            [merge_options/3, select_option/3, select_option/4,
             option/2, option/3]).


:- use_module(library(http/html_write)).
:- use_module(library(option)).
:- use_module(library(error)).
:- use_module(library(debug)).
:- system:use_module(library(backcomp)).

:- multifile blob_rendering//3.              % +Type, +Blob, +Options
:- multifile portray//2.                     % +Term, +Options

:- predicate_options(pprint_tree/2, 2,
                     [ output(stream),
                       right_margin(integer),
                       left_margin(integer),
                       tab_width(integer),
                       indent_arguments(integer),
                       operators(boolean),
                       write_options(list)
                     ]).

:- use_module(library(option)).
%:- /*system:*/use_module(butterfly_term_html,[bfly_term//2]).
:- thread_local(t_l:print_mode/1).

:- export(with_pp/2).
:- export(in_pp/1).
:- export(pp_set/1).
:- export(is_pp_set/1).
:- export(toplevel_pp/1).
:- export(display_length/2).
:- export(in_bfly/2).
:- export(in_pp_html/1).

:- dynamic(pretty_clauses:pp_hook/3).
:- multifile(pretty_clauses:pp_hook/3).
:- module_transparent(pretty_clauses:pp_hook/3).
:- export(pretty_clauses:pp_hook/3).

%:- use_module(library(butterfly_console)).

%:- thread_local(pretty_tl:in_pretty_tree/0).
%:- thread_local(pretty_tl:in_pretty_tree_rec/0).

%prolog_pprint_tree(Term):- \+ pretty_tl:in_pretty_tree, !,
%  setup_call_cleanup(asserta(pretty_tl:in_pretty_tree, Ref), print_tree_unit(Term), erase(Ref)).
%prolog_pprint_tree(Term):- \+ pretty_tl:in_pretty_tree_rec, !,
%  setup_call_cleanup(asserta(pretty_tl:in_pretty_tree_rec, Ref), prolog_pprint(Term, [portray_goal(print_tree_unit)]), erase(Ref)).
prolog_pprint_tree_term(Term):-  prolog_pprint(Term), !.


user:test_pp:- 
  make,
  print_tree(a(a{ a:b, = : -1 })),
  %print_tree(a(a{ a:b, = : -1 })),
  %bfly_tests,
  %retractall(bfly_tl:bfly_setting(_,_)),
  % abolish(bfly_tl:bfly_setting,2),
  thread_local(bfly_tl:bfly_setting/2),
  test_print_tree.

test_print_tree:- 
  
  predicate_property(test_print_tree1(_),number_of_clauses(N)),
  forall((between(1,N,X),
     nth_clause(test_print_tree1(_),N,Ref),clause(_,Body,Ref),
      format('~N%=% ?- ~q.~n',[test_pp_each(Body)])),
     test_pp_each(on_xf_ignore(test_print_tree(X)))).
%  forall(clause(test_print_tree1(N),_Body),call((nop(test_print_tree1(N)),call_test_print_tree(N)))).

test_print_tree(N):- integer(N), nth_clause(test_print_tree1(_),N,Ref),clause(_,Body,Ref),!,
   call(Body).
% test_print_tree(N):- forall(test_print_tree1(N),true).

:- meta_predicate(on_xf_ignore(0)).
%on_xf_ignore(G):- \+ thread_self(main), !, notrace(ignore(on_x_fail(catch(G,E,wdmsg(G->E))))),!.
%on_xf_ignore(G):- on_x_fail(G),!. 
%on_xf_ignore(G):- call(G),!.
%on_xf_ignore(G):- dmsg(failed(G)),!.
on_xf_ignore(G):- notrace(ignore(catch(G,_,fail))).
:- export(on_xf_ignore/1).

test_pp(PP,Goal):-
  with_pp(PP,test_pp_desc(PP,Goal)).
test_pp_desc(PP,Goal):- 
  write('%====================================================\n'),
  format('% ?- ~p. ~n',[test_pp(PP,Goal)]),
  format('% ?- ~@. ~n',[print_tree_no_nl(test_pp(PP,Goal))]),
  format('% ?- ~@. ~n',[print_tree(test_pp(PP,Goal))]),
  format('% ?- ~@ ~n', [print_tree_with_final(test_pp(PP,Goal),'.')]),
  write('%==================START====================\n==>\n'),
  ignore(with_pp(PP,\+ \+ Goal)),
  write('<==\n%==================END========================\n'),
   !.

test_pp_each(G):- 
 ttyflush,
 maplist(on_xf_ignore,
 [test_pp(ansi,G),
  ttyflush,
  wots(SS,test_pp(http,G)),our_pengine_output(SS),
  ttyflush,
  wots(S,test_pp(bfly,G)),our_pengine_output(S),
  ttyflush,
  %test_pp(swish,G),
  ttyflush,
  !]).


test_print_tree1:- test_print_tree.

test_print_tree1(1):-   print_tree(( e2c_lexical_segs =   [  w(is,[pos(aux),loc(1),lnks(1),txt("is"),link(1,'S',r('S',seg(1,10)))]),      w(there,[pos(ex),loc(2),lnks(2),txt("there"),link(1,'NP',r('NP',seg(2,2))),link(2,'S',r('S',seg(1,10)))]),      w(a,         [  pos(dt),            loc(3),            lnks(3),            txt("a"),            link(1,'NP',r('NP',seg(3,4))),            link(2,'NP',r('NP',seg(3,9))),            link(3,'S',r('S',seg(1,10)))  ]),      w(the,         [  pos(dt),            loc(7),            lnks(6),            txt("the"),            link(1,'NP',r('NP',seg(7,9))),            link(2,'VP',r('VP',seg(6,9))),            link(3,'S',r('S',seg(6,9))),            link(4,'SBAR',r('SBAR',seg(5,9))),            link(5,'NP',r('NP',seg(3,9))),            link(6,'S',r('S',seg(1,10)))  ]),      w(greatest,         [  pos(jjs),            loc(8),            lnks(6),            txt("greatest"),            link(1,'NP',r('NP',seg(7,9))),            link(2,'VP',r('VP',seg(6,9))),            link(3,'S',r('S',seg(6,9))),            link(4,'SBAR',r('SBAR',seg(5,9))),            link(5,'NP',r('NP',seg(3,9))),            link(6,'S',r('S',seg(1,10)))  ]),      span( [  seg(6,9),               phrase('VP'),               size(4),               lnks(4),               #(r('VP',seg(6,9))),               txt(["becomes","the","greatest","tenor"]),               childs(1),               child(1,'NP',r('NP',seg(7,9))),               link(1,'S',r('S',seg(6,9))),               link(2,'SBAR',r('SBAR',seg(5,9))),               link(3,'NP',r('NP',seg(3,9))),               link(4,'S',r('S',seg(1,10)))  ]),      span( [  seg(1,10),               phrase('S'),               size(10),               lnks(0),               #(r('S',seg(1,10))),               txt(["is","there","a","man","who","becomes","the","greatest","tenor","?"]),               childs(2),               child(1,'NP',r('NP',seg(2,2))),               child(2,'NP',r('NP',seg(3,9)))  ])  ] )).

%test_print_tree1(2):- nl,nl, test_rok,!.

test_print_tree1(2):- 
 print_tree_with_final( a(b(c(e(7
     %,M.len() := Len :-Len is sqrt(M.x**2 + X.y**2)
    ),f), 
   print_tree(a(b(c(e(7),f),d))), print_tree(a(b(c, X = point{x:1,y:2}.C1 , (e(X),f),d))),
    [print_tree,a,_BE,'$VAR'('SEE'),C1,e,1.3,-7,`abcd`,"abcd",['a','b','c'],f,d, print_tree,a,b,c,e,7,f,d],
    print_tree(a(b(c(e(7),f),d)),a)),x,y),'.').

test_print_tree1(3):- 
 print_tree((print_tree_with_final( a(b(c(e(E7))))):- 
 print_tree_with_final( point{x:1,y:2}, a(b(c(e(E7
     %,M.len() := Len :-Len is sqrt(M.x**2 + X.y**2)
    ),f), 
   print_tree(a(b(c(e(7),f),d))), print_tree(a(b(c, X = point{x:1,y:2}.C1 , (e(X),f),d))),
    [print_tree,a,_BE,'$VAR'('SEE'),C1,e,1.3,-7,`abcd`,"abcd",['a','b','c'],f,d, print_tree,a,b,c,e,7,f,d],
    print_tree(a(b(c(e(7),f),d)))),x,y),'.'),!)),!.

test_print_tree1(4):- forall(sample_pp_term(X), (nl,print_tree(X),nl)).

%test_print_tree1(b):- forall(sample_pp_term(X), print_tree_cmt('hi',red,X)).

:- style_check(-singleton).

:- op(700,'yfx','&').
sample_pp_term((asserted(
   q( exists,
      Exists8,
      q( exists,
         Walked18,
         q( exists,
            Exists7,
            q( exists,
               Exists,
               ( info(
                    'XVAR_NP_John_1_1',
                    [ loc(1),
                      pos('NP'),
                      equals('XVAR_NP_John_1_1'),
                      seg(1,1),
                      phrase('NP'),
                      size(1),
                      lnks(2),
                      #(r('NP',seg(1,1))),
                      txt(["john"]),
                      childs(0),
                      link(1,'S',r('S',seg(1,5))),
                      link(2,'CORENLP',r('CORENLP',seg(1,5)))]) &
                 info(
                    'XVAR_NP_The_Fountain_4_5',
                    [ loc(4),
                      pos('NP'),
                      equals('XVAR_NP_The_Fountain_4_5'),
                      seg(4,5),
                      phrase('NP'),
                      size(2),
                      lnks(4),
                      #(r('NP',seg(4,5))),
                      txt(["the","fountain"]),
                      childs(0),
                      link(1,'PP',r('PP',seg(3,5))),
                      link(2,'VP',r('VP',seg(2,5))),
                      link(3,'S',r('S',seg(1,5))),
                      link(4,'CORENLP',r('CORENLP',seg(1,5)))]) &
                 span([
                    seg(1,5),
                    phrase('S'),
                    size(5),
                    lnks(1),
                    #(r('S',seg(1,5))),
                    txt(["john","walked","to","the","fountain"]),
                    childs(2),
                    child(1,'NP',r('NP',seg(1,1))),
                    child(2,'VP',r('VP',seg(2,5))),
                    link(1,'CORENLP',r('CORENLP',seg(1,5)))]) &
                 span([
                    seg(1,5),
                    phrase('CORENLP'),
                    size(5),
                    lnks(0),
                    #(r('CORENLP',seg(1,5))),
                    txt(["john","walked","to","the","fountain"]),
                    childs(1),
                    child(1,'S',r('S',seg(1,5)))]) &
                 span([
                    seg(2,5),
                    phrase('VP'),
                    size(4),
                    lnks(2),
                    #(r('VP',seg(2,5))),
                    txt(["walked","to","the","fountain"]),
                    childs(1),
                    child(1,'PP',r('PP',seg(3,5))),
                    link(1,'S',r('S',seg(1,5))),
                    link(2,'CORENLP',r('CORENLP',seg(1,5)))]) &
                 span([
                    seg(3,5),
                    phrase('PP'),
                    size(3),
                    lnks(3),
                    #(r('PP',seg(3,5))),
                    txt(["to","the","fountain"]),
                    childs(1),
                    child(1,'NP',r('NP',seg(4,5))),
                    link(1,'VP',r('VP',seg(2,5))),
                    link(2,'S',r('S',seg(1,5))),
                    link(3,'CORENLP',r('CORENLP',seg(1,5)))]) &
                 p(c(walk,to),C,P) &
                 iza(Walked18,actWalking) &
                 doer(Walked18,Doer_Walked182) &
                 objectWalked(Walked18,ObjectWalked_Walked183) &
                 iza(Walked18,timeFn(vPast)) &
                 equalsVar(XVAR_NP_The_Fountain_4_5,'XVAR_NP_The_Fountain_4_5') &
                 equalsVar(XVAR_NP_John_1_1,'XVAR_NP_John_1_1'))))))))).

sample_pp_term(( e2c_lexical_segs =   [  w(is,[pos(aux),loc(1),lnks(1),txt("is"),link(1,'S',r('S',seg(1,10)))]),      w(there,[pos(ex),loc(2),lnks(2),txt("there"),link(1,'NP',r('NP',seg(2,2))),link(2,'S',r('S',seg(1,10)))]),      w(a,         [  pos(dt),            loc(3),            lnks(3),            txt("a"),            link(1,'NP',r('NP',seg(3,4))),            link(2,'NP',r('NP',seg(3,9))),            link(3,'S',r('S',seg(1,10)))  ]),      w(the,         [  pos(dt),            loc(7),            lnks(6),            txt("the"),            link(1,'NP',r('NP',seg(7,9))),            link(2,'VP',r('VP',seg(6,9))),            link(3,'S',r('S',seg(6,9))),            link(4,'SBAR',r('SBAR',seg(5,9))),            link(5,'NP',r('NP',seg(3,9))),            link(6,'S',r('S',seg(1,10)))  ]),      w(greatest,         [  pos(jjs),            loc(8),            lnks(6),            txt("greatest"),            link(1,'NP',r('NP',seg(7,9))),            link(2,'VP',r('VP',seg(6,9))),            link(3,'S',r('S',seg(6,9))),            link(4,'SBAR',r('SBAR',seg(5,9))),            link(5,'NP',r('NP',seg(3,9))),            link(6,'S',r('S',seg(1,10)))  ]),      span( [  seg(6,9),               phrase('VP'),               size(4),               lnks(4),               #(r('VP',seg(6,9))),               txt(["becomes","the","greatest","tenor"]),               childs(1),               child(1,'NP',r('NP',seg(7,9))),               link(1,'S',r('S',seg(6,9))),               link(2,'SBAR',r('SBAR',seg(5,9))),               link(3,'NP',r('NP',seg(3,9))),               link(4,'S',r('S',seg(1,10)))  ]),      span( [  seg(1,10),               phrase('S'),               size(10),               lnks(0),               #(r('S',seg(1,10))),               txt(["is","there","a","man","who","becomes","the","greatest","tenor","?"]),               childs(2),               child(1,'NP',r('NP',seg(2,2))),               child(2,'NP',r('NP',seg(3,9)))  ])  ] )).

sample_pp_term((  a(b(c(e(7
     %,M.len() := Len :-Len is sqrt(M.x**2 + X.y**2)
    ),f), 
   print_tree(a(b(c(e(7),f),d))), print_tree(a(b(c, X = point{x:1,y:2}.C1 , (e(X),f),d))),
    [print_tree,a,_BE,'$VAR'('SEE'),C1,e,1.3,-7,`abcd`,"abcd",['a','b','c'],f,d, print_tree,a,b,c,e,7,f,d],
    print_tree(a(b(c(e(7),f),d)))),x,y))).

sample_pp_term(((print_tree_with_final( a(b(c(e(E7))))):- 
 print_tree_with_final( a(b(c(e(E7
     , M.len() := Len :-Len is sqrt(M.x**2 + X.y**2)
    ),f), 
   print_tree(a(b(c(e(7),f),d))), print_tree(a(b(c, X = point{x:1,y:2}.C1 , (e(X),f),d))),
    [print_tree,a,_BE,'$VAR'('SEE'),C1,e,1.3,-7,`abcd`,"abcd",['a','b','c'],f,d, print_tree,a,b,c,e,7,f,d],
    print_tree(a(b(c(e(7),f),d)))),x,y),'.'),!))).

sample_pp_term(( point{x:1,y:2})).

:- style_check(+singleton).
/*
sample_pp_term(( ( a( M.len() := Len :-Len is sqrt(M.x**2 + M.y**2))))).
sample_pp_term(( X = point{x:1,y:2}.X)).
sample_pp_term((  _X = point{x:1,y:2}.hypn())).
sample_pp_term((  X = a(X) )).
sample_pp_term((  X.X )).
sample_pp_term((  X|X )).
sample_pp_term(X):- world_snap(X).
*/




:- export(prolog_pprint/1).
prolog_pprint(Term):- prolog_pprint(Term, []).
:- export(prolog_pprint/2).
prolog_pprint(Term, Options):- ground(Term),
     \+ \+ (mort((prolog_pprint_0(Term, Options)))), !.
prolog_pprint(Term, Options):- \+ ground(Term),
   \+ \+ (mort((pretty_numbervars(Term, Term2),
     prolog_pprint_0(Term2, Options)))), !.


% prolog_pprint_0(Term, Options):- Options ==[], pprint_ecp_cmt(blue, Term), !.

% prolog_pprint_0(Term, Options):- memberchk(portray(true), Options), \+ is_list(Term), \+ memberchk(portray_goal(_), Options), print_tree(Term, Options), !.
prolog_pprint_0(Term, Options):- \+ memberchk(right_margin(_), Options), !, prolog_pprint_0(Term, [right_margin(0)|Options]).
prolog_pprint_0(Term, Options):- \+ memberchk(portray(_), Options), !, prolog_pprint_0(Term, [portray(true)|Options]).
prolog_pprint_0(Term, Options):- \+ memberchk(quoted(_), Options), !, prolog_pprint_0(Term, [quoted(true)|Options]).
prolog_pprint_0(Term, Options):- %fail,
  mort((guess_pretty(Term), pretty_clauses:pprint_tree(Term, [output(current_output)|Options]))).

prolog_pretty_pprint_tree(A,Options):- 
  my_merge_options(Options,[portray(true), quoted(true), output(current_output)], OptionsNew),
  pretty_clauses:pprint_tree(A, OptionsNew).


str_repl(F,R,S,O):- if_string_repl(S,F,R,O),!.
str_repl(_,_,S,S).

replcterm(F,R,S,O):- subst(S,F,R,O),!.

if_string_repl(T, B, A, NewT):-   
   atomics_to_string(List, B, T), List=[_,_|_], !,
   atomics_to_string(List, A, NewT). 

get_operators(P,[]):- \+ compound_gt(P, 0), !.
get_operators([H|T],Ops):- !, get_operators(H,L),get_operators(T,R),append(L,R,Ops).
get_operators(P,Ops):- P=..[F|List],get_operators(List,More),
  (is_operator(F)->Ops=[F|More];Ops=More).

is_operator('<->').
is_operator('->').
is_operator('-->').
is_operator('<-').
is_operator(F):- current_op(N,_,F),N>800.


get_op_restore(OP,Restore):- 
   findall(op(E,YF,OP),(member(YF,[xfx,xfy,yfx,fy,fx,xf,yf]),current_op(E,YF,OP)),List),
   Restore = maplist(call,List).
get_op_zero(OP,Zero):- 
   findall(op(0,YF,OP),(member(YF,[xfx,xfy,yfx,fy,fx,xf,yf])),List),
   Zero = maplist(call,List).

with_op_cleanup(_NewP,_YF,_OP,Goal):- !, Goal.
with_op_cleanup(NewP,YF,OP,Goal):-
   (current_op(OldP,YF,OP);OldP=0) -> 
   get_op_restore(OP,Restore),
   get_op_zero(OP,Zero),
   Setup = (Zero,op(NewP,YF,OP)),
   Cleanup = (op(OldP,YF,OP),Restore),
   scce_orig(Setup,Goal,Cleanup).


mid_pipe(In,[H|T],Out):- !,mid_pipe(In,H,Mid),mid_pipe(Mid,T,Out).
mid_pipe(In,[],In):-!.
mid_pipe(In,H,Out):- !, call(H,In,Out).

trim_stop(S,O):- sub_string(S, N, 1, 0, Last), 
  (Last = "." -> sub_string(S, 0, N, 1, O); 
     ((Last="\n";Last="\r";Last=" ") -> (sub_string(S, 0, N, 1, Before),trim_stop(Before,O)) ; S=O)).

get_print_opts(_Term, PrintOpts):- 
 get_varname_list(Vs), 
 PrintOpts = 
[portrayed(true),
 portray(true), partial(true),
     %spacing(next_argument),
     character_escapes(true),
     variable_names(Vs)
     %numbervars(true),
     %singletons(false),
     %nl(false),fullstop(false)
     ].

clause_to_string_et(T,S):-
 get_print_opts(T,PrintOpts),
 print_et_to_string(T,S0,PrintOpts),!,
 notrace(trim_stop(S0,S)),!.

clause_to_string(T,S):-  
 get_print_opts(T,PrintOpts),
 print_et_to_string(T,S0,PrintOpts),!,
 notrace(trim_stop(S0,S)),!.
/*
clause_to_string(T,S):-  
 get_print_opts(T,PrintOpts),
 wots((S0), prolog_listing:portray_clause(current_output,T,PrintOpts)),
 notrace(trim_stop(S0,S)).
*/


:- export(compound_gt/2).
compound_gt(P,GT):- notrace((compound(P), compound_name_arity(P, _, N), N > GT)).

print_e_to_string_b(H, S):- 
  compound_gt(H, 0), H=..[F,_,_], 
  current_op(_,_,F),
  print_e_to_string(H, S0),
  mid_pipe(S0,[str_repl('\n',' \n')],S1),
  sformat(S, '(~s)',[S1]),!.

print_e_to_string_b(H, HS):- print_e_to_string(H, HS),!.

% print_e_to_string(T, _Ops, S):- wots(S,print_tree_with_final(T,'')),!.

print_e_to_string(T,_Ops, S):- string(T),!,S=T.
print_e_to_string(T, Ops, S):- member(Infix,['<-']), member(Infix, Ops), !, 
   subst(T,Infix,(':-'),T0), 
   clause_to_string(T0,S0), !,
   mid_pipe(S0,str_repl(':-',Infix),S).

print_e_to_string(T, Ops, S):- Pos=['<-','->','<->',':-'],  
   member(Infix,Pos), select(Infix,Ops,Rest), member(Infix2, Pos),
    \+ member(Infix2,Rest), !, 
   subst(T,Infix,(':-'),T0), 
   clause_to_string(T0,S0), !,
   mid_pipe(S0,str_repl(':-',Infix),S).

print_e_to_string(T, Ops, S):- member(E, Ops),member(E,[':-',',','not','-->']), !, clause_to_string(T,S).

print_e_to_string(exists(Vars,H), _, S):-
  print_e_to_string(H, HS),
  sformat(S, 'exists(~p,\n ~s)',[Vars, HS]).

print_e_to_string(T, Ops, S):- Ops \== [],
   member(EQUIV-IF,[('->'-'<->'),(if-equiv)]),
   (member(IF, Ops);member(EQUIV, Ops)),

   mid_pipe(T, [replcterm((EQUIV),(':-')), replcterm((IF),('-->'))],T0),
   clause_to_string(T0,S0),!,
   mid_pipe(S0, [str_repl(':-',EQUIV),str_repl('-->',IF)],S).


print_e_to_string(T, Ops, S):-  member('<->', Ops), sformat(S0, '~p',[T]),
  mid_pipe(S0,str_repl('<->','<->\n  '),S).

print_e_to_string(axiom(H,B), _, S):-
  print_e_to_string((H-->B), S0),  
  mid_pipe(S0,[str_repl(' \n','\n'),str_repl(' -->',','),str_repl('\n\n','\n')],S1),
  sformat(S,'axiom(~s)',[S1]).

print_e_to_string(B, [Op|_], S):- ((Op== ';') ; Op==','), !,
  print_e_to_string((:- B), S0),  
  mid_pipe(S0,[str_repl(':-','')],S).  

print_e_to_string(B, _, S):- is_list(B),  !,
  print_e_to_string((:- B), S0),  
  mid_pipe(S0,[str_repl(':-','')],S).  

print_e_to_string(T, _Ops, S):-  is_list(T), print_et_to_string(T,S,[right_margin(200)]),!.
print_e_to_string(T, _Ops, S):-  must(print_et_to_string(T,S,[])).

print_et_to_string(T,S,Options):-
  get_varname_list(Vs),
  numbervars_using_vs(T,TT,Vs),
  ttyflush,
   Old = [%numbervars(true),
                  quoted(true),
                  ignore_ops(false),
                  no_lists(false),
                  %spacing(next_argument),
                  portray(false)],
   notrace(my_merge_options(Old,Options,WriteOpts)),
   PrintOpts = [output(current_output)|Options],                                
  sformat(S, '~@', [(sys:plpp(TT,WriteOpts,PrintOpts), ttyflush)]).

% sys:plpp(T):- !, print(T).
sys:plpp(T):- sys:plpp(T,[]).

sys:plpp(T, Opts):- notrace(sys:plpp(T, Opts)).

plpp0(T, Opts):- 
 get_varname_list(Vs),
  numbervars_using_vs(T,TT,Vs),
   Old = [% numbervars(true),
                  quoted(true), ignore_ops(false), no_lists(false), 
                  %spacing(next_argument), %portray(false),
                  portray_goal(print_tree_plpp)],
   notrace(my_merge_options(Old,Options,WriteOpts1)),
   notrace(my_merge_options(WriteOpts1,Opts,WriteOpts)),
   PrintOpts = [output(current_output)|Options],                                
  sys:plpp(TT,WriteOpts,PrintOpts).
%sys:plpp(TT,WriteOpts,PrintOpts):- !,
%   pprint_tree(TT, [write_options(WriteOpts)|PrintOpts]).
sys:plpp(TT,WriteOpts,PrintOpts):- 
   \+ \+ pprint_tree(TT, 
             [   %left_margin(1),
                 %operators(true),
                 %tab_width(2),
                 %max_length(120),
                 %indent_arguments(auto),                  
                 write_options(WriteOpts)|PrintOpts]).

   
print_tree_plpp(Term,Opts):- notrace(print_tree_loop(Term,Opts)).
% print_tree_loop(Term):- current_print_write_options(Options), print_tree_loop(Term,Options).
 
print_tree_loop(Term,Options):- \+ pretty_tl:in_pretty,!,
  setup_call_cleanup(asserta(pretty_tl:in_pretty,Ref),
    print_tree_unit(Term,Options),
    erase(Ref)).
print_tree_loop(Term, Options):- 
  with_current_line_position(simple_write_term(Term, Options)).


to_ansi(A,B):- to_ansi0(A,B),!.
to_ansi0(e,[bold,fg(yellow)]).
to_ansi0(ec,[bold,fg(green)]).
to_ansi0(pl,[bold,fg(cyan)]).
to_ansi0(pink,[bold,fg('#FF69B4')]).
to_ansi0([H|T],[H|T]).
to_ansi0(C, [bold,hfg(C)]):- assertion(nonvar(C)), is_ansi_color(C),!.
to_ansi0(H,[H]).

is_ansi_color(white). is_ansi_color(black). is_ansi_color(yellow). is_ansi_color(cyan). 
is_ansi_color(blue). is_ansi_color(red). is_ansi_color(green). is_ansi_color(magenta).


is_output_lang(Lang):- atom(Lang), Lang \==[],
 \+ is_ansi_color(Lang), nb_current('$output_lang',E),E\==[], !, memberchk(Lang,E).
is_output_lang(_).
  
%:- export(pprint_ec/2).
%pprint_ec(C, P):- pprint_ec_and_f(C, P, '~n').

:- export(duplicate_nat/2).
duplicate_nat(P0,P1):- copy_term_nat(P0,P),duplicate_term(P,P1).

:- export(pprint_ecp_cmt/2).
pprint_ecp_cmt(C, P):- 
 notrace((mort((echo_newline_if_needed,  
  print_e_to_string(P, S0),
  into_space_cmt(S0,S),
  to_ansi(C, C0),
  real_ansi_format(C0, '~s', [S]))))).

:- export(pprint_ecp/2).
pprint_ecp(C, P):- \+ is_output_lang(C), !, pprint_ecp_cmt(C, P).
pprint_ecp(C, P):-
  maybe_mention_s_l(1),
  echo_newline_if_needed,  
  maybe_bfly_html(pprint_ec_and_f(C, P, '.~n')).

pprint_ec_and_f(C, P, AndF):-
 mort((
  maybe_mention_s_l(2),
  pprint_ec_no_newline(C, P), 
  echo_format(AndF))), !,
  ttyflush.


/*
without_ec_portray_hook(Goal):-
   setup_call_cleanup(current_prolog_flag(debug, Was),
     (set_prolog_flag(debug, true),Goal),
     set_prolog_flag(debug, Was)).

*/

exact_ec_portray_hook(Val,Goal):-
   setup_call_cleanup(flag('$ec_portray', N, Val), 
     Goal, flag('$ec_portray',_, N)),!.

with_ec_portray_hook(Goal):- exact_ec_portray_hook(0,Goal).
without_ec_portray_hook(Goal):- exact_ec_portray_hook(1000,Goal).

%pc_portray(Term):- Term==[], !, color_format_maybe(hfg(blue),'~q',[[]]).
%pc_portray(Term):- notrace(tracing),!,ec_portray_hook(Term).
%pc_portray(X):- is_list(X),print_tree_unit(X).

pc_portray(Term):- var(Term),!,fail.
pc_portray(Term):- atom(Term), exists_file_safe(Term),public_file_link(Term,Public),write_q(Public).
pc_portray(Term:L):- integer(L),atom(Term), exists_file_safe(Term),public_file_link(Term:L,Public),write_q(Public).
pc_portray(mfl4(M,F,Term,L)):- integer(L),atom(Term), exists_file_safe(Term),public_file_link(Term:L,Public),write_q(mfl4(M,F,Public,L)).
pc_portray(Term):- 
  \+ ( nb_current('$inprint_message', Messages), Messages\==[] ), 
  % (tracing->dumpST;true),
  \+ tracing,!,
  % dont screw up SWISH or PLDoc
  \+ toplevel_pp(swish), \+ toplevel_pp(http), % is_pp_set(_),
  ec_portray_hook(Term).

ec_portray_hook(Term):-  
 setup_call_cleanup(flag('$ec_portray', N, N+1), 
  ec_portray(N, Term),
  flag('$ec_portray',_, N)).

color_format_maybe(_,F,A):- format(F,A),!.

:- export(write_q/1). 
write_q(S):- maybe_pp_hook(write_q, S),!.
write_q(X):- in_pp(bfly),!,print_html_term(X).
write_q(O):- maybe_special_printing(O),!.
write_q(X):- writeq(X).


ec_portray(_,X):- as_is_cmpd(X),!,without_ec_portray_hook(write_q(X)).
ec_portray(_,X):- atom(X),ansi_ansi,!,without_ec_portray_hook(write_q(X)).
ec_portray(N,_):- N > 3,!,fail.
ec_portray(_,Term):- (\+ compound(Term);Term='$VAR'(_)),!, ec_portray_now(Term).
ec_portray(N,List):- N<2, is_list(List),!,print_tree_unit(List).
%ec_portray(_,Term):- notrace(is_list(Term)),!,Term\==[], fail, notrace(catch(text_to_string(Term,Str),_,fail)),!,format('"~s"',[Str]).
ec_portray(_,Term):- compound(Term), compound_name_arity(Term,F,A), uses_op(F,A), !, fail.
%ec_portray(_,Term):- compound(Term),compound_name_arity(Term, F, 0), !,color_format([bold,hfg(red)],'~q()',[F]),!.
ec_portray(N,Term):- N > -1, N < 3, \+ is_dict(Term), ec_portray_now(Term).

ec_portray_now(Var):- var(Var), !, get_var_name(Var,Name), color_format_maybe(fg(green),'~w',[Name]),!.
ec_portray_now('$VAR'(Atomic)):- integer(Atomic), !, color_format_maybe(fg(yellow),'~w',['$VAR'(Atomic)]).

ec_portray_now('$VAR'(Atomic)):- !, 
  ((atom(Atomic), name(Atomic,[C|_]),code_type(C,prolog_var_start))->
     color_format_maybe(fg(yellow),'~w',[Atomic]);
     color_format_maybe(fg(red),"'$VAR'(~q)",[Atomic])).
ec_portray_now(Term):- if_defined(rok_linkable(Term),fail),!, write_atom_link(Term).
ec_portray_now(Term):- atom(Term),!,color_format_maybe(hfg(blue),'~q',[Term]).
ec_portray_now(Term):- \+ compound(Term),!, color_format_maybe(hfg(cyan),'~q',[Term]).
%ec_portray_now(Term):- is_list(Term)
%ec_portray_now(Term):- catch(print_tree_unit(Term),_,fail),!.
%ec_portray_now(Term):- N =0, \+ ansi_ansi,!, print_tree_unit(Term), !.
%ec_portray_now(Term):- catch(pprint_ec_no_newline(green, Term),_,fail),!.

will_need_space(_):- fail.

uses_op(F,A):- functor([_|_],FF,A),FF=F.
uses_op(F,A):- current_op(_,XFY,F),once((name(XFY,[_|Len]),length(Len,L))),L=A.

/*pprint_ec_no_newline(_C, P):-
  print_e_to_string(P, S),
  format('~s', [S]),!.
*/
pprint_ec_no_newline(C, P):-
 must_det_l((
  print_e_to_string(P, S),
  to_ansi(C, C0),
  real_ansi_format(C0, '~s', [S]))).
  

print_e_to_string(P, S):-  notrace(with_output_to(string(S),print_tree_unit(P))),!.
print_e_to_string(P, S):- 
  quietly(( must_det_l((
   pretty_numbervars(P, T),
   get_operators(T, Ops))),!,
   % maybe_bfly_html
   print_e_to_string(T, Ops, S))),!.
/*
print_e_to_string(P, S):- 
   get_operators(P, Ops),
   must(pretty_numbervars(P, T)), 
   with_op_cleanup(1200,xfx,(<->),
     with_op_cleanup(1200,xfx,(->),
       with_op_cleanup(1200,xfy,(<-),
          print_e_to_string(T, Ops, S)))).
 
*/

pretty_trim_message(A,C):- replace_in_string(['\n\n\n'='\n\n'],A,B),A\==B,!,pretty_trim_message(B,C).
pretty_trim_message(A,C):- replace_in_string(['\n\n'='\n'],A,B),A\==B,!,pretty_trim_message(B,C).
pretty_trim_message(A,C):- replace_in_string(['\n          \n'='\n'],A,B),A\==B,!,pretty_trim_message(B,C).
pretty_trim_message(A,C):- \+ string(A),!,any_to_string(A,S),pretty_trim_message(S,C).
pretty_trim_message(A,C):- split_string(A, "", "`\s\t\n", [B]), A\==B,!,pretty_trim_message(B,C).
pretty_trim_message(A,A).


into_space_cmt(S00,O):- 
  pretty_trim_message(S00,S0),
  %normalize_space(string(S1),S0),
  str_repl('\n','\n     ',S0, S),
  (S0==S -> sformat(O, '~N %  ~s.~n', [S]); 
    (maybe_mention_s_l(1),sformat(O, '~n /*  ~s.~n */~n', [S]))).

% in_space_cmt(Goal):- call_cleanup(prepend_each_line(' % ', Goal), echo_newline_if_needed).
%in_space_cmt(Goal):- setup_call_cleanup((echo_newline_if_needed, echo_format('/*\n ', []), Goal, echo_newline_if_needed, echo_format(' */~n', [])).
in_space_cmt(Goal):- 
   wots((S0),Goal),
   into_space_cmt(S0,S),
   real_format('~s', [S]), !.

in_space_cmt(Goal):- setup_call_cleanup((echo_newline_if_needed,echo_format('/*\n ', [])), Goal, (echo_newline_if_needed,echo_format(' */~n', []))).



read_line_to_string_echo(S, String):- read_line_to_string(S, String), ttyflush, real_ansi_format([bold, hfg(black)], '~s~n',[String]),
  ttyflush.
  
echo_flush:- ttyflush.
:- export(echo_format/1).
echo_format(S):- echo_flush, echo_format(S, []),!.
:- export(echo_format/2).


:- thread_local(t_l:each_file_term/1).
:- thread_local(t_l:quit_processing_stream/1).
:- thread_local(t_l:block_comment_mode/1).
:- thread_local(t_l:echo_mode/1).

echo_format(_Fmt, _Args):- t_l:block_comment_mode(Was), Was==invisible, !.
echo_format(Fmt, Args):- t_l:block_comment_mode(_), t_l:echo_mode(echo_file), !, real_format(Fmt, Args), ttyflush.
echo_format(Fmt, Args):- t_l:echo_mode(echo_file), !, real_format(Fmt, Args), ttyflush.
echo_format(_Fmt, _Args):- t_l:echo_mode(skip(_)), !.
echo_format(Fmt, Args):- real_format(Fmt, Args), ttyflush, !.
%echo_format(_Fmt, _Args).

echo_newline_if_needed:- tracing,!.
echo_newline_if_needed:- echo_format('~N').


is_outputing_to_file:- nb_current('$ec_output_stream',Outs),is_stream(Outs), (stream_property_s(Outs,file_name(_));current_output(Outs)), !.
%is_outputing_to_file:- nb_current('$ec_output_stream',Outs),
is_outputing_to_file:- 
  current_output(S),
  stream_property_s(S,file_name(_)).

stream_property_s(S,P):- on_x_fail(stream_property(S,P)).

get_ansi_dest(S):- \+ is_outputing_to_file,!,current_output(S).
get_ansi_dest(S):- S = user_output, !.
get_ansi_dest(S):- S = user_error, !.

with_output_to_ansi_dest(Goal):- 
  maybe_bfly_html((get_ansi_dest(AnsiDest),stream_property_s(AnsiDest,output),
  with_output_to(AnsiDest,(Goal,ttyflush)),ttyflush)).
  

put_out(Char):- put(Char),
  (is_outputing_to_file -> with_output_to_ansi_dest(put(Char)) ; true),!.
  

real_format(Fmt, Args):- listify(Args,ArgsL), real_ansi_format([hfg(magenta)], Fmt, ArgsL).

real_ansi_format(Ansi, Fmt, Args):- listify(Args,ArgsL), real_ansi_format0(Ansi, Fmt, ArgsL).
real_ansi_format0(Ansi, Fmt, Args) :-  \+ is_outputing_to_file, !, maybe_bfly_html(color_format_maybe(Ansi, Fmt, Args)).
real_ansi_format0(_Ansi, Fmt, Args) :-  format(Fmt, Args), !.
%real_ansi_format0(Ansi, Fmt, Args) :-  with_output_to_ansi_dest(color_format_maybe(Ansi, Fmt, Args)),!.

%flush_channel_output_buffer

%s_l(F,L):- source_location(F,L),!.

:- thread_local(etmp:last_s_l/2).
%:- dynamic(etmp:last_s_l/2).
%:- volatile(etmp:last_s_l/2).

:- export(maybe_mention_s_l/1).
maybe_mention_s_l(N):- etmp:last_s_l(B,L), LLL is L+N,  s_l(BB,LL), B==BB, !, (LLL<LL -> mention_s_l; (N==1->mention_o_s_l;true)).
maybe_mention_s_l(_):- mention_s_l.

:- export(mention_s_l/0).
mention_s_l:- 
  s_l(F,L), % real_ansi_format([fg(green)], '~N% From ~w~n', [F:L]),
  (o_s_l_diff->mention_o_s_l;true),
  retractall(etmp:last_s_l(F,_)),
  asserta(etmp:last_s_l(F,L)).


%:- dynamic(ec_reader:o_s_l/2).
:- thread_local(ec_reader:o_s_l/2).
%:- volatile(ec_reader:o_s_l/2).

o_s_l_diff:- s_l(F2,L2), ec_reader:o_s_l(F1,L1), (F1 \= F2; ( Diff is abs(L1-L2), Diff > 0)), !.

maybe_o_s_l:- \+ o_s_l_diff, !.
maybe_o_s_l:- notrace(e_source_location(F,L)),retractall(ec_reader:o_s_l(_,_)),asserta(ec_reader:o_s_l(F,L)),!.
maybe_o_s_l.

output_line_count(L):- nb_current('$ec_output_stream',Outs),is_stream(Outs),on_x_fail(line_count(Outs,L)), !.
output_line_count(L):- line_count(current_output,L).

with_current_line_position(Goal):- !, call(Goal).
with_current_line_position(Goal):-
  setup_call_cleanup(current_output_line_position(L),
     Goal,
     reset_line_pos(L)).

reset_line_pos(L):- current_output_line_position(New),reset_line_pos(New,L).
reset_line_pos(New,Old):- New=Old,!.
reset_line_pos(New,Old):- New>Old, !, nl, prefix_spaces(Old).
reset_line_pos(New,Old):- New<Old, !, Extra is Old-New, prefix_spaces(Extra).

current_output_line_position(L):- nb_current('$ec_output_stream',Outs),is_stream(Outs),on_x_fail(line_position(Outs,L)), !.
current_output_line_position(L):- line_position(current_output,L).

%:- dynamic(ec_reader:last_output_lc/3).
:- thread_local(ec_reader:last_output_lc/3).
%:- volatile(ec_reader:last_output_lc/3).
% ec_reader:last_output_lc(0,foo,bar).

mention_o_s_l:- ignore(do_mention_o_s_l),!.
do_mention_o_s_l:- ec_reader:o_s_l(F,L),!,out_o_s_l_1(F,L).
do_mention_o_s_l:- s_l(F,L), was_s_l(F,L),! .



out_o_s_l_1(F,L):- ec_reader:last_output_lc(Was,F,L),
  output_line_count(OLC),
  Diff is abs(Was-OLC), Diff<6,!.
out_o_s_l_1(F,L):- out_o_s_l_2(F,L),!.
out_o_s_l_2(F,L):- User_error = current_output,
      retractall(ec_reader:last_output_lc(_,_,_)),
      output_line_count(OLC),
      asserta(ec_reader:last_output_lc(OLC,F,L)),
      (is_outputing_to_file -> 
        (format('~N~q.~n', [:- was_s_l(F,L)]), 
           with_output_to(User_error,(public_file_link(F:L,FL),color_format_maybe([fg(green)], '~N% FRom ~w~n', [FL]),ttyflush)))
         ; nop((public_file_link(F:L,FL),color_format_maybe([fg(green)], '~N% FroM ~w~n', [FL]),ttyflush))),!.

:- export(was_s_l/2).
was_s_l(B,L):- retractall(ec_reader:o_s_l(_,_)),asserta(ec_reader:o_s_l(B,L)), out_o_s_l_2(B,L).
  

e_source_location(F,L):- nb_current('$ec_input_stream',Ins), any_line_count(Ins,L), any_stream(F,Ins),!.
e_source_location(F,L):- nb_current('$ec_input_file',FS), absolute_file_name(FS,F), any_stream(F,Ins), any_line_count(Ins,L),!.
e_source_location(F,L):- current_stream(F, read, S), atom(F), atom_concat_safety(_,'.e',F), any_line_count(S,L),!.
e_source_location(F,L):- stream_property_s(S, file_name(F)),stream_property_s(S, input), atom_concat_safety(_,'.e',F), any_line_count(S,L),!.
e_source_location(F,L):- stream_property_s(S, file_name(F)),atom_concat_safety(_,'.e',F), any_line_count(S,L),!.

:- export(s_l/2).
s_l(F,L):- notrace(on_x_fail(e_source_location(B,L2))), !, L is L2-1, absolute_file_name(B,F).
s_l(F,L):- source_location(F,L2), !, L is L2-1.
% s_l(F,L):- ec_reader:o_s_l(F,L). 
s_l(F,L):- any_stream(F,S), any_line_count(S,L),any_line_count(_,L), !.
s_l(unknown,0).

any_stream(F,S):- is_stream(F),var(S),!,F=S.
any_stream(F,S):- stream_property_s(S, file_name(F)),stream_property_s(S, input).
any_stream(F,S):- current_stream(F, read, S), atom(F).
any_stream(F,S):- stream_property_s(S, file_name(F)).
any_stream(F,S):- current_stream(F, _, S), atom(F).

any_line_count(_,L):- nonvar(L),!.
any_line_count(F,L):- nonvar(F), \+ is_stream(F), any_stream(F,S), any_line_count(S,L),!.
any_line_count(S,L):- on_x_fail(line_count(S, L)),!.
any_line_count(S,L):- on_x_fail(character_count(S, C)), L is C * -1,!.
any_line_count(S,L):- on_x_fail(line_or_char_count(S, L)),!.
any_line_count(_,0).

:- fixup_exports.



/*

 _________________________________________________________________________
|	Copyright (C) 1982						  |
|									  |
|	David Warren,							  |
|		SRI International, 333 Ravenswood Ave., Menlo Park,	  |
|		California 94025, USA;					  |
|									  |
|	Fernando Pereira,						  |
|		Dept. of Architecture, University of Edinburgh,		  |
|		20 Chambers St., Edinburgh EH1 1JZ, Scotland		  |
|									  |
|	This program may be used, copied, altered or included in other	  |
|	programs only for academic purposes and provided that the	  |
|	authorship of the initial program is aknowledged.		  |
|	Use for commercial purposes without the previous written 	  |
|	agreement of the authors is forbidden.				  |
|_________________________________________________________________________|

*/

/* Print term as a tree */

:- export(print_tree/1).
:- export(print_tree/2).
:- export(prefix_spaces/1).


:- export(print_tree_cmt/3).
print_tree_cmt(Mesg,C,P):-
 ensure_pp((
 mention_o_s_l,!,
 quietly((echo_newline_if_needed,  
  in_cmt(
    in_color(C,
    (format('~N~w: \n\n',[Mesg]),
     print_tree_unit(P),
     echo_newline_if_needed))))))).

:- export(in_color/2).
%in_color(Ctrl,Goal):- ansicall(Ctrl,Goal),!.
in_color(C,P):-
 ensure_pp(quietly(( to_ansi(C, C0), ansicall(C0,P)))).


%pt_nl:- nl.

%:- dynamic(pretty_clauses:goal_expansion/2).
% pretty_clauses:goal_expansion(pt_nl,(pformat(S:L),nl)):- source_location(S,L).

maybe_special_printing(O):- \+ compound(O),!,fail.
maybe_special_printing(O):- \+ (sub_term(E,O),never_as_is(E)),!,fail.
%maybe_special_printing(O):- nl,nl,portray_with_vars(O,[]),nl,nl,!.
maybe_special_printing(O):- nl,nl,nl,nl, print(O),!.

write_simple(A):- maybe_special_printing(A),!.
write_simple(A):- write_simple(A,[]).
write_simple(S,_):- term_is_ansi(S), !, write_keeping_ansi(S).
write_simple(A,Options):- get_portrayal_vars(Vs), 
  my_merge_options(Options,[quoted(true), portrayed(true), variable_names(Vs)],OptionsNew),
  without_ec_portray_hook((
   setup_call_cleanup(asserta(pretty_tl:in_pretty,Ref),    
     simple_write_term(A,OptionsNew),
     erase(Ref)))).

portray_with_vars(A):- maybe_special_printing(A),!.
portray_with_vars(A):- portray_with_vars(A,[]),!.

portray_with_vars(A,Options):- 
   Ing = A+final,
   once(nb_current('$in_portray_with_vars',P);P=[]),
   \+ (member(E,P),E=@=Ing), !,
   setup_call_cleanup(
   nb_setval('$in_portray_with_vars',[Ing|P]),
   maybe_bfly_html(portray_with_vars1(A,Options)),
   nb_setval('$in_portray_with_vars',P)),!.

% portray_with_vars(A,Options):- dumpST, break, throw(looped(portray_with_vars(A,Options))).

:- export(portray_with_vars1/2).
portray_with_vars1(A,Options):-
  get_portrayal_vars(Vs),
  my_merge_options(Options,[quoted(true), portrayed(true), variable_names(Vs)],OptionsNew),
 without_ec_portray_hook(( must_or_rtrace(simple_write_term(A,OptionsNew)))),!.


%my_portray_clause(current_output,A,Options):-  prolog_listing:portray_body(A, 0, indent, 1199, current_output, Options).

:- thread_local(pretty_tl:in_pretty/0).

prolog_pretty_print_term(A,Options):- 
  my_merge_options(Options,[portray(true),quoted(true), output(current_output)], OptionsNew),
  \+ \+ pprint_tree(A, OptionsNew).

%simple_write_term(A):- compound(A),compound_name_arity(A,_,0),write_q(A),!.
%simple_write_term(A):- atomic(A), \+ atom(A), \+ string(A), !, write_q(A).
% @TODO comment out the next line
%simple_write_term(A):- !, with_no_hrefs(t,(if_defined(rok_writeq(A),write_q(A)))),!.

system:simple_write_term(S):- maybe_special_printing(S),!.
system:simple_write_term(S):- maybe_pp_hook(simple_write_term, S),!.
system:simple_write_term(A):- in_pp(bfly),!,print_html_term(A).
system:simple_write_term(A):- 
 current_print_write_options(Options),
   without_ec_portray_hook(\+ \+ write_term(A,Options)),!.
system:simple_write_term(A):- write_q(A),!.

system:simple_write_term(A,Options):- maplist(fix_svar_names,Options,Options2),
  with_write_options(Options2,simple_write_term(A)).

fix_1svar_name(N=V,NN=V):- atom_to_varname(N,NN),!.
atom_to_varname(N,NN):- \+ notrace(catch(with_output_to(string(_),write_term(A,[variable_names([N=A])])),_,fail)),
  term_hash(N,HC), with_output_to(atom(NN),format('UHC_~w',[HC])),!.
atom_to_varname(N,N):-!.
atom_to_varname(N,NN):- atom_codes(N,[C1|Codes]),fix_varcodes_u(C1,C2),maplist(fix_varcodes,Codes,NCodes),atom_codes(NN,[C2|NCodes]).
fix_varcodes_u(C,N):- C<65,C>90, N is (C rem 25)+65.
fix_varcodes(C,N):- C<65,!, N is (C rem 25)+65.
fix_varcodes(C,C).
fix_svar_names(variable_names(Vs),variable_names(VsO)):- maplist(fix_1svar_name,Vs,VsO),!.
fix_svar_names(X,X).

:- fixup_exports.
%simple_write_term(A,Options):- write_term(A,[portray_goal(pretty_clauses:pprint_tree)|Options]).

get_portrayal_vars(Vs):- nb_current('$variable_names',Vs)-> true ; Vs=[].


system_portray(Term):- current_output_line_position(L), system_portray(L,Term).

system_portray(Tab, Term, Options):-   
   with_write_options(Options,system_portray(Tab,Term)).


system_portray(Tab,Term):- recalc_tab(Tab, NewTab), !, system_portray(NewTab,Term).
system_portray(Tab,Term):- maybe_pp_hook(system_portray(Tab), Term),!.
system_portray(_Tab, S) :- term_is_ansi(S), !, write_keeping_ansi(S).
%system_portray(Tab,Term,_Options) :-  ground(Term), Term = [tag(_,N),M], prefix_spaces(Tab),write([N,M]),!.
%system_portray(Tab,Term,_Options) :-  ground(Term), Term = tag(_,N), prefix_spaces(Tab),write(N),!.
system_portray(Tab,Term):- 
   Ing = Term,
   once(nb_current('$in_system_portray',P);P=[]),
   \+ (member(E,P),E=@=Ing), !,
   nb_setval('$in_system_portray',[Ing|P]),
     prefix_spaces(Tab), print_tree_with_final(Term, ''),
   nb_setval('$in_system_portray',P).
system_portray(Tab,Term):- prefix_spaces(Tab), portray_with_vars(Term), !.



:- thread_local(pretty_tl:write_opts_local/1).
current_print_write_options(Options):- 
 (pretty_tl:write_opts_local(Additions)->true;Additions=[]),
  current_prolog_flag(print_write_options,OptionsDefault),
  get_portrayal_vars(Vs), my_merge_options(OptionsDefault,[variable_names(Vs)|Additions],Options),!.


% merge options
with_merged_write_options(Options,Goal):-
  current_print_write_options(OldOptions), 
  my_merge_options(OldOptions,Options,NewOptions),
   setup_call_cleanup(asserta(pretty_tl:write_opts_local(Options),Ref),
                      with_write_options(NewOptions,Goal),
                      erase(Ref)).

with_write_options(NewOptions,Goal):-
  current_prolog_flag(print_write_options, OldOptions),
  (NewOptions==OldOptions -> Goal ;
    (setup_call_cleanup(set_prolog_flag(print_write_options,NewOptions),
                      Goal,
                      set_prolog_flag(print_write_options,OldOptions)))).


trim_ending_ws(S,O):- is_html_white_r(W),string_concat(L,W,S),!,trim_ending_ws(L,O).
trim_ending_ws(S,O):- last_tag(S,Tag),!,string_concat(L,Tag,S),trim_ending_ws(L,M),string_concat(M,Tag,O).
trim_ending_ws(S,S).
ending_tag('</span>').
last_tag(S,Tag):- ending_tag(Tag),string_concat(_,Tag,S).

print_as_tree(Term):- print_tree_unit(Term).

ansi_ansi:- notrace((once(is_pp_set(ansi);\+ is_pp_set(_)),toplevel_pp(ansi))).

tabbed_print(Pos,Goal):-
 wots(S,Goal),
 trim_ending_ws(S,SS),
 with_output_to(string(White),print_spaces(Pos)),
 atomics_to_string(L,'\n',SS),
 print_each_prepended(White,L).

maybe_reset_spaces(Pos):- ignore((current_output_line_position(PosNew), PosNew>Pos,  prefix_spaces(Pos))).



maybe_pp_hook(Why,S):- 
   current_print_write_options(Options),
   current_output_line_position(Pos),
   with_write_options(Options, pretty_clauses:pp_hook(Why, Pos, S)).


%print_tree(Term):- print_html_term_tree(Term).
print_tree(Term):- ansi_ansi,current_output_line_position(Pos),!,print_tree_with_final(Term,''), maybe_reset_spaces(Pos).
print_tree(Term):- ansi_ansi,!,print_tree_nl(Term).
print_tree(Term):-  
  wots(S,with_pp(http,print_tree_unit(Term))),write_html(S).
/*
print_html_term_tree(Term):- 
 current_print_write_options(Options),
 must_or_rtrace(phrase(bfly_term(Term,[right_margin(60),left_margin(0),indent(1),nl(true),full_stop(true)]),Tokens)),!,
 must_or_rtrace(print_html_term_tree_st(Tokens)),!.

print_html_term_tree_st(['<',html,'>'|Tokens]):-!,remove_if_last(Tokens,['</',html,'>'],TokensLeft),print_html_term_tree_st_1(TokensLeft).
print_html_term_tree_st(Tokens):- print_html_term_tree_st_1(Tokens).
print_html_term_tree_st_1([nl(1)|Tokens]):-!,remove_if_last(Tokens,[nl(1)],TokensLeft),print_html_term_tree_st(TokensLeft).
print_html_term_tree_st_1(Tokens):- with_output_to(string(HTMLString), (write('<pre>'), html_write:print_html(Tokens),write('</pre>'))),
  write_html(HTMLString).
*/

print_tree_unit(S):- maybe_pp_hook(print_tree_unit,S),!.

print_tree_unit(Term):-
 current_output_line_position(Pos),
 ensure_pp((
  tabbed_print(Pos, print_tree_with_final(Term, '', 
  [ partial(true), numbervars(true), character_escapes(true),fullstop(false)])))).

print_tree_unit(Term, Options):-
 current_output_line_position(Pos),
 ensure_pp((
  tabbed_print(Pos, print_tree_with_final(Term, '', 
  [ partial(true), numbervars(true), character_escapes(true),fullstop(false)|Options])))).

print_tree_nl(Term):- print_tree_with_final(Term,'.\n').

/*
print_tree_nl(Term):-
 current_output_line_position(Pos),
 ensure_pp(( 
  tabbed_print(Pos, print_tree_with_final(Term, ' ', 
  [ partial(true), numbervars(true), character_escapes(true),nl(true),fullstop(true)])))).
*/

print_tree_no_nl(Term):-
 current_output_line_position(Pos),
 ensure_pp(( 
  tabbed_print(Pos, print_tree_with_final(Term, '', 
  [ partial(true), numbervars(true), character_escapes(true),nl(false),fullstop(false)])))).


print_tree(Term, Options) :- select(fullstop(true),Options,OptionsNew), !, print_tree_with_final(Term, '.', [fullstop(false)|OptionsNew]).
print_tree(Term, Options) :- print_tree_with_final(Term, '', Options).




print_each_prepended(_White,[L]):- !, write(L).
print_each_prepended(White,[L|More]):- write(L),!,nl,write(White),
  print_each_prepended(White,More).


print_tree_with_final(Term, Final):-
    locally(set_prolog_flag(no_pretty,false),print_tree_with_final(Term, Final, [fullstop(false)])).


:-export(print_tree_with_final/3).
print_tree_with_final(Term, Final, Options):- 
  select(variable_names(Vs),Options,NewOptions),!,
  nb_current('$variable_names',Was),
  setup_call_cleanup(
    b_setval('$variable_names',Vs),
    once(print_tree_with_final(Term, Final, NewOptions)),
    nb_setval('$variable_names',Was)).

print_tree_with_final(Term, Final, Options):- select(max_depth(N),Options,OptionsNew), in_pp(bfly), !,
 with_folding_depth(N, print_tree_with_final(Term, Final, OptionsNew)).

print_tree_with_final(Term, Final, Options):- select(html_depth(N),Options,OptionsNew),!,
 with_folding_depth(N, print_tree_with_final(Term, Final, OptionsNew)).

print_tree_with_final(Term, Final, Options):- 
  \+ \+ (member(numbervars(true),Options),
  pretty_numbervars(Term,Term2), 
  print_tree_with_final_real(Term2, Final, Options)),!.

print_tree_with_final(Term, Final, Options):-
  print_tree_with_final_real(Term, Final, Options).



print_tree_with_final_real(Term, Final, Options):-  
  current_output_line_position(Tab),
  print_tree_with_final_real(Tab, Term, Final, Options).

print_tree_with_final_real(Tab, Term, Final, Options):- fail,
  member(left_margin(N),Options), N > Tab, !, 
  print_tree_with_final_real(N, Term, Final, Options).

print_tree_with_final_real(Tab, Term, Final, Options):- 
   with_merged_write_options([fullstop(false)|Options],
     ensure_pp((print_tab_term(Tab, Term),pformat(Final)))).


print_tab_term(Term):- print_tab_term(0, Term).
print_tab_term(Tab, Term):- without_ec_portray_hook(print_tab_term(Tab,[], Term)),!.
print_tab_term(Tab,FS,Term) :- prefix_spaces(Tab),pt1(FS,Tab,Term).

use_new.



%:- abolish(bfly_tl:bfly_setting,2).
:- thread_local(bfly_tl:bfly_setting/2).
%:- retractall(bfly_tl:bfly_setting(_,_)).

:- export(ensure_pp/1).
:- meta_predicate(ensure_pp(0)).
ensure_pp(Goal):-  is_pp_set(Where), !, with_pp(Where,Goal).
ensure_pp(Goal):-  toplevel_pp(Where), !, with_pp(Where,Goal).

should_print_mode_be_html(_):- toplevel_pp(ansi),!,fail.
should_print_mode_be_html(_):- current_predicate(inside_bfly_html_esc/0), inside_bfly_html_esc.
should_print_mode_be_html(ansi):- !, fail.
should_print_mode_be_html(_).


% with_pp(swish,Goal):- !,locally_tl(print_mode(html),with_pp(bfly,Goal)).
%with_pp(swish,Goal):- toplevel_pp(http),!,with_pp(bfly,Goal).
%with_pp(swish,Goal):- toplevel_pp(swish),!,with_pp(bfly,Goal).
%with_pp(http,Goal):- toplevel_pp(swish),!,with_pp(bfly,Goal).

:- meta_predicate(with_pp(+,0)).
with_pp(plain,Goal):- !, with_pp(ansi,locally_tl(print_mode(plain),Goal)).
with_pp(Mode,Goal):- quietly(with_pp0(Mode,Goal)).

:- meta_predicate(with_pp0(+,0)).
with_pp0(bfly,Goal):- in_pp(swish),!,with_pp0(swish,Goal).
with_pp0(ansi,Goal):- \+ t_l:print_mode(plain), !, locally_tl(print_mode(plain),with_pp0(ansi,Goal)).
with_pp0(Mode,Goal):- \+ t_l:print_mode(html), 
  should_print_mode_be_html(Mode),!, 
  locally_tl(print_mode(html),with_pp0(Mode,Goal)).

with_pp0(Where,Goal):- \+ is_pp_set(Where), !,
    setup_call_cleanup(
      asserta(bfly_tl:bfly_setting(pp_output,Where),Ref),
      with_pp0(Where,Goal),
      erase(Ref)),!.

with_pp0(Where,Goal):- toplevel_pp(Real), ttyflush, with_real_pp(Real,Where,Goal), ttyflush.

write_bfly_html(S):- empty_str(S),!.
write_bfly_html(S):- split_string(S, "", "\s\t\n",L),atomics_to_string(L,LL),LL\==S,!,write_bfly_html(LL).
write_bfly_html(S):- split_string(S,"\n\r\0","",LS),atomics_to_string(LS,'\n',W),write_bfly_html_0(W).

write_bfly_html_0(S):- empty_str(S),!.
write_bfly_html_0(S):- split_string(S, "", "\s\t\n",L),atomics_to_string(L,LL),LL\==S,!,write_bfly_html_0(LL).
write_bfly_html_0(S):- bfly_html_goal(write(S)).

% actually_bfly(Goal):- flush_output, bfly_html_goal(Goal).
actually_bfly(Goal):- bfly_html_goal((wots(S,set_pp(swish,Goal)),write_bfly_html_0(S))).
:- export(actually_bfly/1).
set_pp(Where,Goal):- 
   \+ in_pp(Where) 
   -> setup_call_cleanup(
      asserta(bfly_tl:bfly_setting(pp_output,Where),Ref),
      Goal,
      erase(Ref)) 
   ; call(Goal).

with_real_pp(ansi,ansi,Goal):- in_bfly(f,Goal).
with_real_pp(ansi,bfly,Goal):- in_bfly(t,Goal).
with_real_pp(ansi,http,Goal):- in_bfly(f,Goal).
with_real_pp(ansi,swish,Goal):- wots(S,Goal), sformat(SO,'<pre class="ansi_swish">~w</pre>',[S]),our_pengine_output(SO).
%wots(S,in_bfly(t,bfly_html_goal(Goal))), ttyflush, format('~s',[S]).

with_real_pp(bfly,ansi,Goal):- bfly_out_in(in_bfly(f,Goal)).

with_real_pp(bfly,http,Goal):- wants_html,!,call(Goal).

with_real_pp(bfly,http,Goal):- ttyflush,format('<http>'),ttyflush, actually_bfly(Goal), ttyflush, format('</http>',[]).
with_real_pp(bfly,bfly,Goal):- bfly_html_goal(in_bfly(t,Goal)).
with_real_pp(bfly,swish,Goal):- ttyflush,format('<swish>'),ttyflush, actually_bfly(Goal), ttyflush, format('</swish>',[]).

with_real_pp(http,ansi,Goal):- wots(SO,in_bfly(f,Goal)),format('<pre class="http_ansi">~s</pre>',[SO]).
with_real_pp(http,bfly,Goal):- in_bfly(t,Goal).
with_real_pp(http,http,Goal):- in_bfly(t,Goal).
with_real_pp(http,swish,Goal):- wots(SO,in_bfly(t,Goal)),our_pengine_output(SO).

with_real_pp(swish,ansi,Goal):- wots(SO,in_bfly(f,Goal)),our_pengine_output(SO).
with_real_pp(swish,bfly,Goal):- wots(SO,in_bfly(t,Goal)),our_pengine_output(SO).
with_real_pp(swish,http,Goal):- wots(SO,in_bfly(t,Goal)),our_pengine_output(SO).
with_real_pp(swish,swish,Goal):-wots(SO,in_bfly(t,Goal)),our_pengine_output(SO).

our_pengine_output(Codes):- catch(text_to_string(Codes,Str),_,fail),Codes\==Str,!,our_pengine_output(Str).
%our_pengine_output(SO):- toplevel_pp(http),!,format('<span class="swish">~w</span>',[SO]).
%our_pengine_output(SO):- toplevel_pp(bfly),!,bfly_html_goal((sformat(S,'<pre>~w</pre>',[SO]),print_raw_html_page(S))).
%our_pengine_output(SO):- \+ atom(SO), catch(text_to_atom(SO,Atom),_,fail),SO\==Atom,!,our_pengine_output(Atom).
our_pengine_output(SO):- toplevel_pp(swish),!,pengines:pengine_output(SO),!.
our_pengine_output(SO):- toplevel_pp(http),!,write(SO).
%our_pengine_output(SO):- toplevel_pp(bfly),!,write(SO).
%our_pengine_output(SO):- toplevel_pp(bfly),!,(inside_bfly_html_esc->write(SO); bfly_write_hs(SO)).
our_pengine_output(SO):- in_pp(ansi),!,write(SO).
our_pengine_output(SO):- bfly_write_hs(SO).
%our_pengine_output(SO):- setup_call_cleanup((bfly_title("+HtmlMode"),write(SO),bfly_title("-HtmlMode"),flush_output),true,true),!.



%write_html(HTML):- phrase(html(HTML), Tokens), html_write:print_html(Out, Tokens))).
% output_html(html([div([id('cp-menu'), class(menu)], cp_skin: cp_logo_and_menu)]))
output_html(Var):- var(Var),!,term_to_atom(Var,Atom),output_html(pre([Atom])).
%output_html(html(HTML)):- !,output_html(HTML). 
output_html(HTML):- atomic(HTML),!,write_html( \ [HTML]). 
%output_html(HTML):- is_list(HTML),send_tokens(HTML).
output_html(HTML):- html_write:phrase(html(HTML), Tokens,[]),!,send_tokens(Tokens).


%  our_pengine_output('<pre>hi</pre>').
%  our_html_output_old('<pre>hi</pre>').
%our_pengine_output(SO):- ttyflush,format('our_pengine_output\n{~w}',[SO]),nl.
%our_pengine_output(SO):- 

%:- nb_setval(isHtmlMode,nil).


is_webui:- notrace(once(toplevel_pp(http);toplevel_pp(swish);wants_html;in_pp(swish);get_print_mode(html))).

%in_bfly_esc:- !, current_predicate(in_bfly_style/2), in_bfly_style(style,'html_esc'), !.
in_pp(X):- notrace(in_pp0(X)).

in_pp0(X):- nonvar(X), in_pp0(Y), !, X==Y.
in_pp0(X):- is_pp_set(X),!.
in_pp0(Guess):- toplevel_pp(Guess).

pp_set(X):- bfly_set(pp_output,X).

is_pp_set(X):- bfly_tl:bfly_setting(pp_output,X),!.

set_toplevel_pp(PP):- nb_setval('$fake_toplevel_pp',PP), retractall(bfly_tl:bfly_setting(pp_output,_)).
with_toplevel_pp(PP,Goal):- locally(nb_setval('$fake_toplevel_pp',PP),Goal).

toplevel_pp(X):- nonvar(X), toplevel_pp(Y), !, X==Y.
toplevel_pp(PP):- nb_current('$fake_toplevel_pp',PP),PP\==[],!.
toplevel_pp(swish):- \+ thread_self(main), on_x_log_fail(nb_current('$pp_swish',t);(pengines:pengine_self(_Self))),!.
toplevel_pp(http):- \+ thread_self(main), on_x_log_fail(httpd_wrapper:http_current_request(_)),!.
% Fake only for testing between bfly/ansi
toplevel_pp(bfly):- getenv('TERM','xterm-256color'),!.
toplevel_pp(ansi):- getenv('TERM','xterm'),!.
toplevel_pp(bfly):- current_predicate(bfly_get/2), bfly_get(butterfly,t),!.
toplevel_pp(ansi).

%toplevel_pp(html_pre):- 
%in_pp(html_pre):- on_x_log_fail(httpd_wrapper:http_current_request(_)).

%display_length(X,L):- wots(S,display(X)),atom_length(S,L),!.
display_length(S,L):- string(S),!, atom_length(S,L).
display_length(S,L):- atom(S),!, atom_length(S,L).
display_length(I,L):- with_output_to(string(S),display(I)),!,atom_length(S,L).



%:- use_module(pretty_clauses).


%pformat(S,Fmt,Args):- with_output_to(S,pformat(Fmt,Args)).
%pformat(Fmt,Args):-  format(Fmt,Args).
:- export(pformat/1).


pformat(pre(Fmt)):- nonvar(Fmt), !, pformat_string(Fmt,S), pformat_write(S).
pformat(Fmt):- pformat_std(pformat,Fmt), !.
pformat(Fmt):- wants_html, !,pformat_html(pre(Fmt)).
pformat(Fmt):- pformat_write(Fmt).

pformat_html(_):- ansi_ansi,!.
pformat_html(Fmt):- var(Fmt),!,format('~w',[Fmt]).
pformat_html(PREC):- PREC == pre(:), !, write(':').
%pformat_html(pre(Fmt)):- pformat_string(Fmt,S), !, into_attribute(S,Attr),write(Attr). % print_html(['<pre>',S,'</pre>']).
pformat_html(pre(Fmt)):- pformat_string(Fmt,S), !, into_attribute(S,Attr),write(Attr). % print_html(['<pre>',S,'</pre>']).
%pformat_html(pre(Fmt)):- pformat_string(Fmt,S), !, atom_length(S,Len), ignore((Len>0, print_html(['<pre class="pre_html">',S,'</pre>']))),!.
% WANT this? 
%pformat_html(pre(Fmt)):- pformat_string(Fmt,S), phrase(pretty_clauses:html(S), Tokens), print_html(Tokens).
pformat_html(Fmt):- pformat_std(pformat_html,Fmt), !.
pformat_html(Fmt):- atomic(Fmt),!,bfly_html_goal(pformat_write(Fmt)).
pformat_html(Fmt):- phrase(pretty_clauses:html(Fmt), Tokens), print_html(Tokens).


:- export(pformat_string/2).
pformat_string(Fmt,S):- \+ compound(Fmt),!,any_to_string(Fmt,S).
pformat_string(Fmt,S):- wots(S,pformat(Fmt)).

:- export(pformat_write/1).
pformat_write(Codes):- catch(text_to_string(Codes,Str),_,fail),!,write(Str).
pformat_write(Str):- write(Str).

:- export(pformat_std/2).
pformat_std(_,List):- is_codelist(List),string_codes(Str,List),!,pformat_write(Str).
pformat_std(P,List):- is_list(List),!,maplist(P,List).
pformat_std(_,Fmt):- (Fmt=='';Fmt==[]),!.
pformat_std(_,Fmt):- (var(Fmt);Fmt=='.'),!,format('~w',[Fmt]).
pformat_std(P,Fmt):- (var(Fmt);Fmt=='.'),!,term_to_atom(Fmt,T),call(P,T).
pformat_std(_,w(Fmt)):- !, pformat_write(Fmt).
pformat_std(_,html(Fmt)):- !, pformat_html(Fmt).
pformat_std(_,pformat(Fmt)):- !, pformat(Fmt).
pformat_std(P,format(Fmt,Args)):- !, sformat(S,Fmt,Args),!,call(P,S).
pformat_std(P,'-'(Fmt,Args)):- !, sformat(S,Fmt,Args),!,call(P,S).
pformat_std(_,html(Fmt,Args)):- sformat(S,Fmt,Args), !, pformat_html(w(S)).
pformat_std(_,call(Goal)):- !, ignore(call(Goal)).
pformat_std(P,eval(Fmt)):- pformat_string(call(Fmt),S),call(P,S).
pformat_std(_,ps(Spaces)):- !, prefix_spaces(Spaces).
pformat_std(_,Fmt):- Fmt=='\n',!,pformat_newline.
pformat_std(_,Fmt):- Fmt== ' ',!,pformat_space.

print_spaces(N):- var(N),!.
print_spaces(N):- N<1, !.
print_spaces(Need):- pformat_space,M1 is Need -1,print_spaces(M1).

wants_html:-  notrace(wants_html0).
%wants_html0:- never_webui, !, fail.
wants_html0:- in_pp(bfly),!.
wants_html0:- in_pp(http),!.
wants_html0:- toplevel_pp(http),!.
wants_html0:- toplevel_pp(swish),!.
wants_html0:- in_pp(swish),!,fail.
%wants_html0:- is_http,!.

never_webui:- 
  \+ current_prolog_flag(use_arc_www,true),
  \+ current_prolog_flag(use_arc_swish,true),
  \+ current_prolog_flag(use_arc_bfly,true),
  \+ current_prolog_flag(no_pretty,true).


%pformat_space:- wants_html,!,write('&nbsp;').
pformat_space:- write(' ').

%pformat_newline:- !,nl.
pformat_newline:- ansi_ansi,!,nl.
%pformat_newline:- in_pp(bfly),!,write(' <br/>'),nl.
pformat_newline:- in_pp(bfly),!,nl.
pformat_newline:- in_pp(html_pre),!,write('\n'),nl.
pformat_newline:- wants_html,!,write(' <p/>\n').
pformat_newline:- in_pp(swish),!,our_pengine_output(' <p/>\n').
pformat_newline:-!.
pformat_newline:- ignore((on_x_log_fail(httpd_wrapper:http_current_request(_)),nl)),nop((write(' <br/>'))).

prefix_spaces_exact(Tab):- notrace(prefix_spaces0(Tab)).
prefix_spaces(Tab):- !,prefix_spaces_exact(Tab).
prefix_spaces(Tab):- notrace(prefix_spaces1(Tab)).
% prefix_spaces0(_Tab).

prefix_spaces0(Tab):- float(Tab),!.
prefix_spaces0(Tab):- \+ number(Tab), !, ignore(( recalc_tab(Tab,   NewTab),!, NewTab\==Tab, prefix_spaces0(NewTab))).
prefix_spaces0(Tab):- Tab < 1, !.
%prefix_spaces0(Tab):- Tab2 is Tab,  print_tree_width(W120),  Tab2 > W120,!, Floor is floor(Tab/2)+1, prefix_spaces0(Floor).
prefix_spaces0(Tab):- ansi_ansi, Tab2 is Tab,  print_tree_width(W120),  Tab2 > W120,!, Floor is Tab-W120, prefix_spaces0(Floor).
prefix_spaces0(Tab):- current_output_line_position(Now), prefix_spaces0(Now,Tab),!.
prefix_spaces0(Now,Tab):- Now > Tab, !, pformat_newline ,  print_spaces(Tab).
prefix_spaces0(Now,Tab):- Need is Tab - Now,!, print_spaces(Need).

prefix_spaces1(Tab):- \+ integer(Tab), recalc_tab(Tab,   NewTab),!, prefix_spaces1(NewTab).
prefix_spaces1(Tab):- Floor is floor(Tab/2)+1, prefix_spaces0(Floor).

:- export(ansi/0).
:- system:import(ansi/0).
:- export(bfly/0).
:- system:import(bfly/0).
ansi:- bfly_set(butterfly,f),set_pp(ansi),set_toplevel_pp(ansi).
bfly:- bfly_set(butterfly,t),set_pp(bfly),bflyw,set_toplevel_pp([]).

az_ansi(Goal):- toplevel_pp(ansi),!,call(Goal).
az_ansi(Goal):- toplevel_pp(bfly),in_pp(bfly),!,wots(S,setup_call_cleanup(ansi,Goal,bfly)), bfly_write_pre(S).
az_ansi(Goal):- toplevel_pp(http),in_pp(bfly),!,setup_call_cleanup(ansi,Goal,bfly).
az_ansi(Goal):- call(Goal).

pl_span_c(_Class):- no_more_folding,!.
pl_span_c(Class):- pformat(html('<span class="pl-~w">',Class)).
pl_span_e:- no_more_folding,!.
pl_span_e:- pformat(html('</span>')).

pl_span_s(Class, Goal):- pl_span_goal(Class, Goal).

pl_span_goal(_, Goal):- ansi_ansi,!,call(incr_term_depth(Goal)).
pl_span_goal(Class, Goal):- setup_call_cleanup(pl_span_c(Class),incr_term_depth(Goal),pl_span_e).
pt_s_e(S, Goal, E):- setup_call_cleanup(pformat(S),incr_term_depth(Goal),pformat(E)).

:- fixup_exports.

% :- bfly.
/*

prefix_spaces0(Tab):- \+ integer(Tab), recalc_tab(Tab,   NewTab),!,prefix_spaces(NewTab).

prefix_spaces0(Tab):- current_output_line_position(Now), Need is Tab - Now, Need > 1,print_spaces(Need),!.
prefix_spaces0(Tab):- current_output_line_position(Now), Now > Tab, !, pformat_newline ,  print_spaces(Tab).
prefix_spaces0(_Tab):- pformat_newline.
%prefix_spaces0(Tab):- current_output_line_position(Now), Need is Tab - Now,!, print_spaces(Need).

prefix_spaces1(Tab):- \+ integer(Tab), recalc_tab(Tab,   NewTab),!,prefix_spaces1(NewTab).
prefix_spaces1(Tab):- Floor is floor(Tab/2)+1, prefix_spaces0(Floor),!.

*/
using_folding_depth:- \+ ansi_ansi, nb_current('$use_folding',t).

fold_this_round:- using_folding_depth, flag('$fold_this_round',N,N), N=1.

%do_fold_this_round(Goal):- flag('$fold_this_round',N,N),N<0,!,call(Goal). 
do_fold_this_round(Goal):- !, call(Goal).
do_fold_this_round(Goal):-
  setup_call_cleanup(flag('$fold_this_round',N,2),
  with_folding(t,Goal),
  flag('$fold_this_round',_,N)).

with_nb_var(Var,TF,Goal):- 
    (nb_current(Var,WAS);WAS=f),
    setup_call_cleanup(b_setval(Var,TF),
      Goal,
      nb_setval(Var,WAS)).


increase_print_depth(Goal):- 
  \+ using_folding_depth 
  -> incr_term_depth(Goal) 
  ; setup_call_cleanup(flag('$fold_this_round',N,N-1),
      Goal,
      flag('$fold_this_round',_,N)).

with_folding(TF,Goal):-
  with_nb_var('$use_folding',TF,Goal).

%with_no_hrefs(_, Goal):- !, Goal. % ignore next line
with_no_hrefs(TF,Goal):-
  with_nb_var('$no_hrefs',TF,Goal).

with_folding_depth(0,Goal):-!,with_folding(f,Goal).
with_folding_depth(Depth,Goal):- 
 setup_call_cleanup(flag('$fold_this_round',N, Depth + 1),
      with_folding(t,Goal),
      flag('$fold_this_round',_,N)).

no_more_folding:- flag('$term_depth',N, N), N>0.

incr_term_depth(Goal):- 
 setup_call_cleanup(flag('$term_depth',N, N + 1),
      Goal,
      flag('$term_depth',_,N)).

pformat_e_args(E, Goal):- using_folding_depth, !, 
  increase_print_depth(( 
          pformat_ellipsis(E),  
          (fold_this_round -> with_folding(f,pl_span_goal('args fold',Goal)) ; pl_span_goal('args',Goal)))),!.

pformat_e_args(E, Goal):- pformat_ellipsis(E), !, pl_span_goal('args',Goal),!.

pformat_functor(F):- pl_span_goal('functor',pformat(F)).
%pformat_functor(F,_):- \+ is_webui, !, pformat_functor(F).

pformat_ellipsis(_):- ansi_ansi,!.
pformat_ellipsis(E):- fold_this_round, !, pl_span_goal('ellipsis clickprev',ellipsis_html(E)),!.
pformat_ellipsis(E):- pl_span_goal('ellipsis clickprev fold',ellipsis_html(E)),!.

ellipsis_html(E):- ignore(pformat_html(pre(call(write_ellipsis(E))))).

write_ellipsis(T):- \+ compound(T),!,write_ellipsis_0(T).
write_ellipsis([T]):- !,write_ellipsis(T).
write_ellipsis(T):- findall(E,
  ((sub_term(E,T), (atom(E);string(E)));
   (sub_term(E,T), \+compound(E));
   (sub_term(C,T), compound(C), \+ is_list(C), compound_name_arity(C,E,_))),L),list_to_set(L,Set),
 wots(S, forall(member(A,Set),(write(A),write('.')))), write_ellipsis_0(S),!.
write_ellipsis(T):- write_ellipsis_0(T).

write_ellipsis_0([T]):- nonvar(T),!,write_ellipsis_0(T).
write_ellipsis_0(_):-!.
write_ellipsis_0(T):- wots(S, (write('.'),write_term(T,[max_depth(4)]),write('...'))),
 trim_to_len(S,30,SO),write('<span class="pl-ellipsis-hidden"> /*'),write(SO),write('*/ </span>').

trim_to_len(A,L,S):- sub_atom(A, 1, L , _, S).
trim_to_len(S,_,S).

wotss(S,Goal):- call(Goal),S="".

is_list_functor(F):- F == lf.

write_using_pprint_recurse(_):- \+ current_module(mu),!,fail.
write_using_pprint_recurse(Term):- write_using_pprint(Term),!,fail.
write_using_pprint_recurse(Term):- is_list(Term),!, \+ (member(T,Term), \+ atomic(T)).
write_using_pprint_recurse(Term):- compound(Term),!, \+ (arg(_,Term,T), \+ atomic(T)).

pair_to_colon(P,C):- P=..[_,K,V],C=..[':',K,V],!.

mu_prolog_pprint(Term,Options):- current_output_line_position(Tab), mu_prolog_pprint(Tab,Term,Options).
mu_prolog_pprint(Tab,Term,Options):- mu:prolog_pprint(Term,[
  left_margin(Tab)|Options]).

is_simple_list(Term):- is_list(Term),!, \+ (member(T,Term), \+ atomic(T)).

write_using_pprint(_):- \+ current_module(mu),!,fail.
write_using_pprint(Term):- is_list(Term), !, member(L, Term), L\==[], is_list(L),!.
write_using_pprint(Term):- compound(Term), compound_name_arity(Term,_,1),!, arg(1,Term,Arg), \+ is_simple_list(Arg).
%write_using_pprint(Term):- is_list(Term), arg(_,Term, L), contains_list(L),!.

contains_list(Term):- \+ \+ ((compound(Term),arg(_,Term, Arg), sub_term(T,Arg), is_list(T),T\==[])).
list_contains_sub_list(Term):- compound(Term),arg(_,Term, Arg),
  sub_term(T,Arg),T\==Arg,is_list(T),T\==[],contains_list(T).
 

inperent([F|_],TTs,Term,Ts):- fail, \+ is_list_functor(F),
      TTs=..[F,Term,Ts], 
      functor(TTsS,F,2),     
     ((nonvar(Term), Term=TTsS);(nonvar(Ts), Ts=TTsS)).


recalc_tab(Tab,     _):- integer(Tab), !, fail.
recalc_tab(AB, Tab):- !, recalc_tab1(AB,   Tab).

recalc_tab1(A+B,   Tab):- !, recalc_tab1(A, AA), recalc_tab1(B, BB), Tab is AA+BB.
recalc_tab1(A-B,   Tab):- !, recalc_tab1(A, AA), recalc_tab1(B, BB), Tab is AA-BB.
recalc_tab1(now,   Tab):- !, current_output_line_position(Tab).
recalc_tab1(TabC,  Tab):- Tab is TabC.

max_output(Tab,W120,T):- display_length(T,L), LL is Tab+L, on_x_ignore(LL<W120),!.

print_atomf(F):- with_folding(f,print_tree_no_nl(F)).
print_functor(F):- with_folding(f,print_tree_unit(F)).

pt_list_juncts(Tab,_OP,[T]):- print_tab_term(Tab,T).
pt_list_juncts(_Tab,_OP,[]):- !.
pt_list_juncts(Tab,OP,[T|List]):- 
    print_tab_term(Tab,T), pformat(' '),pformat(OP),pformat(' '),
    pt_list_juncts(Tab,OP,List).


print_tree_width( RM ):- current_print_write_options(Options), memberchk(right_margin(RM), Options),!.
print_tree_width(W120):- W120=120.

maybe_prefix_spaces(V,Tab):- ignore(( \+ as_is(V),prefix_spaces(Tab) )).
maybe_print_tab_term(Tab,V):- maybe_prefix_spaces(V,Tab), print_tree_no_nl( V ).

print_indented_str(S):- split_string(S,"\n\r\0","",LS), current_output_line_position(Pos), print_indented_str(Pos,LS).

print_indented_str(_,[]):-!.
print_indented_str(_Pos,[S]):- write(S).
print_indented_str(Pos,[H|S]):- write(H),maybe_prefix_spaces(S,Pos), print_indented_str(Pos,S).

write_keeping_ansi(S):- wots(SS,write_keeping_ansi0(S)),!,print_indented_str(SS).

write_keeping_ansi0(S):- string(S),!, write('"'),write(S),write('"').
write_keeping_ansi0(S):- atom(S),!, write("'"),write(S),write("'").
write_keeping_ansi0(S):- \+ atom(S),!, write(S).
write_keeping_ansi0(S):- is_ansi_color(S), !, real_ansi_format([bold, hfg(S)], '~q',[S]).
%write_keeping_ansi(S):- write('\''),write(S),write('\'').
write_keeping_ansi0(S):- write(S).


term_is_ansi(S):- compound(S),!,fail.
term_is_ansi(S):- string(S),!,sub_string(S,_,_,_,"\x1B"),!.
term_is_ansi(S):- \+ atom(S),!,fail.
term_is_ansi(S):- sub_string(S,_,_,_,"\x1B"),!.
term_is_ansi(S):- is_ansi_color(S).


:- export(maybe_write_atom_link/1).
maybe_write_atom_link(Term):-
  with_no_hrefs(t,(if_defined(rok_linkable(Term),fail), !,
  write_atom_link(Term))),!.

term_contains_ansi(S):- \+ compound(S),!,term_is_ansi(S).
term_contains_ansi(S):- arg(_,S,E),term_contains_ansi(E),!.
:- export(term_contains_ansi/1).

:- thread_local(t_l:printing_dict/0).



is_row_list(Lst):- is_list(Lst), \+ (member(E,Lst),( \+ is_ftVar(E),(compound(E);E==[]))).

is_simple_2x2(List):- is_list(List), List\==[], maplist(is_row_list,List).

print_simple_2x2:- print_simple_2x2([ [    bg   ,   bg   ,   bg ], [    bg   ,   bg   ,   bgggg]] ).

print_simple_2x2(List):- print_simple_2x2(print,List).
print_simple_2x2(PS2,List):- current_output_line_position(Tab),print_simple_2x2(PS2,Tab,List).
print_simple_2x2(PS2,Tab,List):- compound(Tab), TabN is Tab, !,print_simple_2x2(PS2,TabN,List).
print_simple_2x2(PS2,Tab,[E|List]):- Tab>1, length(E,Len), Len>23,write('\n\r'),!,print_simple_2x2(PS2,1,[E|List]).
print_simple_2x2(PS2,Tab,[E|List]):- 
  print_to_string11(PS2,0,[E|List],[ES|Str],Pad1),Pad is Pad1,
  prefix_spaces(Tab),pformat_functor('[ '), 
      pt11(Pad, ES), maplist(pt111(Tab+2,Pad),Str),
   write(']').

pt111(Tab, Pad, List):- 
  format(',~N'), prefix_spaces(Tab), pt11(Pad, List).

pt11(Pad, [E|List]):-
  pformat_functor('[ '), print_using_up(Pad,'',E), maplist(print_using_up(Pad,','),List), write(']'),!.

%be_extra(Goal):- ignore(call(Goal)).
be_extra(_DONT).

print_to_string11(_,Max,VarOrNill,VarOrNill,Max):- (var(VarOrNill);VarOrNill==[]),!.
print_to_string11(PS2,I,[H|T],[HH|TT],O):- !, print_to_string11(PS2,I,H,HH,M),print_to_string11(PS2,M,T,TT,O).
print_to_string11(PS2,Cur,H,HH,Max):- wots(HH,call(PS2,H)), display_length(HH,Len), Max is max(Cur,Len).

print_using_up(Total,Comma,S):- display_length(S,Len),Used is Total-Len, write(Comma),print_s_up(S,Used).

print_s_up(S,Used):- Used>0, Need1 is floor(Used/2),Need2 is round(Used/2),
  print_n_sp(Need1),write(S),print_n_sp(Need2),!.
print_s_up(S,_):- write(S),!.

print_n_sp(N):- N>0,!,write(' '),Nm1 is N -1,print_n_sp(Nm1).
print_n_sp(_).

is_infix_op(OP):- current_op(_,XFY,OP), (yfx==XFY ; xfx==XFY ; xfy==XFY ).

operator_is_basic_print(T):- \+ compound(T),!,fail.
operator_is_basic_print(T):- compound_name_arity(T,OP, 1), !, current_op(_Pri,XFY,OP), atom_length(XFY,2),!.
operator_is_basic_print(T):- compound_name_arity(T,OP, 2), not_list_op(OP), is_infix_op(OP),!.
not_list_op(OP):- OP \== '|', OP \== '[|]', OP \== '.'.


print_lc_tab_term(LC,Tab,T):- write(LC),print_tab_term(Tab,T).

pt1(FS,TpN,Term):- recalc_tab(TpN, New), TpN\==New, !, pt1(FS,New,Term).



pt1(_FS,Tab,_S) :- prefix_spaces(Tab), fail.
pt1(_FS,_Tab, S) :- maybe_special_printing(S),!.


pt1(FS, _Tab,S) :- maybe_pp_hook(pt1(FS),S),!.

% pt1(_FS,_Tab,Term) :- is_dict(Term), ansi_ansi,!,sys:plpp(Term),!.
pt1(_FS,Tab,Term) :- is_dict(Term), !,  prefix_spaces(Tab), system_portray(Tab,Term).
pt1(FS,Tab,Term) :- 
   is_dict(Term),
   dict_pairs(Term, Tag, Pairs), maplist(pair_to_colon,Pairs,Colons),!,
   prefix_spaces(Tab), pl_span_goal('functor',( print_tree_no_nl(Tag), pformat('{ '))),
   locally(t_l:printing_dict,pt_args_arglist([dict|FS],Tab+2,'','@','}',Colons)),!.


pt1(_FS,Tab,List) :- is_simple_2x2(List), print_simple_2x2(print,Tab,List),!.

pt1(_, _Tab,Term) :- var(Term),format('~w',[Term]),!.
%t_l:printing_dict
%pt1(_FS,_Tab,(NPV)) :- compound(NPV), NPV=..[OP,V,N], OP==(:), atomic(N),
%  write_q(N), pformat(' '), pformat(OP),pformat(' '), print_tree_unit(V),!.

pt1(_FS,_Tab,S) :- term_is_ansi(S), !, write_keeping_ansi(S).

pt1(_, _Tab,Term) :- atom(Term),maybe_write_atom_link(Term),!.

pt1(_FS,Tab,[H|T]) :- is_codelist([H|T]), !,  
   sformat(S, '`~s`', [[H|T]]),
   pformat([ps(Tab),S]).

pt1(_, Tab,Term) :-
   use_system_portray(Term), !, 
   system_portray(Tab,Term).

pt1(_,_Tab,Term) :- Term=ref(_), !, write_q(Term),!.
pt1(_,_Tab,Term) :- Term=element(_,_,List),List==[], !, write_q(Term),!.


pt1(_, Tab,Term) :- fail,
   as_is(Term), !,
   system_portray(Tab,Term).

pt1(_FS,Tab,T) :- % fail,
   %print_tree_width(W120),
   W120 = 120,
   as_is(T),
   max_output(Tab,W120,T),!,
   prefix_spaces(Tab), write_q(T).
   %system_portray(Tab,T),!.


pt1(_FS,_Tab,N-V) :- print(N-V),!.
pt1(_FS,_Tab,N-V) :- !, print(N),write(' - '),print(V).  



pt1(FS,Tab,List) :- List=[_|_], !,
  prefix_spaces(Tab),pformat_functor('[ '),
  pt_args_arglist([lf|FS],Tab+2,'',' | ',']',List),!.

%pt1(_FS,Tab,T) :- operator_is_basic_print(T),!,prefix_spaces(Tab),print(T).



% H:-B
pt1(_FS,Tab, H:- T) :- 
  OP = (','),
  compound(T),
  compound_name_arity(T,OP, 2),      
  pred_juncts_to_list(OP,T,List),!,
  prefix_spaces(Tab), print_tree_no_nl(H),  
  pl_span_goal('functor', (pformat(' :- '))),
  pformat_e_args(List, pt_list_juncts(Tab+2,OP,List)).
      
%pt1(_FS,_Tab,(NPV)) :- NPV=..[OP,_,_], OP=':', is_colon_mark(OP), ansi_ansi, !, print(NPV).



% xfy/yfx/xfx
pt1(_FS,Tab,T) :- 
  compound_name_arity(T,OP, 2),  
  (current_op(Pri,YFX,OP),is_infix_op(OP)),
  Pri >= 400,
  pred_juncts_to_list(OP,T,List), List=[H,R,E|ST], REST = [R,E|ST],!,
  prefix_spaces(Tab),pl_span_goal('functor', (
    pformat('( '), be_extra((pformat('/*e'),pformat(YFX),pformat('*/'))),print_tree_no_nl(H),pformat('  '), pformat(OP))),
    pformat_e_args(REST, (
       pt_list_juncts(Tab+2,OP,REST))), 
   pformat(')'),!.   


pt1(FS,Tab,(NPV)) :- NPV=..[OP,N,V], is_colon_mark(OP), current_op(_,yfx,OP), !, YFX=yfx,
    print_tab_term(Tab,[op(OP,2)|FS], N),
    format(' '), pformat(OP), pformat(' '),be_extra(( pformat('/*a'),pformat(YFX),pformat('*/'))),
    print_tab_term(Tab+2,V).

%t_l:printing_dict
pt1(_FS,_Tab,(NPV)) :- NPV=..[OP,N,V], is_colon_mark(OP), atomic(N),current_op(_,YFX,OP), !, 
  write_q(N), be_extra((pformat('/*d*'),pformat(YFX),pformat('*/'))), pformat(OP),pformat(' '), print_tree_unit(V),!.

    
pt1(FS,Tab,(NPV)) :- NPV=..[OP,N,V], is_colon_mark(OP),
  print_tab_term(Tab,[op(OP,2)|FS], N),
  pl_span_goal('functor', (
    pformat(' '), pformat(OP), pformat('  '))),
    (ansi_ansi->true; (pformat_ellipsis(V),prefix_spaces(Tab+5))),
    pl_span_goal('args', (prefix_spaces(Tab+2), print_tree_unit( V ))),!.
    

pt1(_FS,Tab,T) :- % fail,
   print_tree_width(W120),
   max_output(Tab,W120,T),!,
   prefix_spaces(Tab), write_q(T).
   %system_portray(Tab,T),!.

pt1(FS,Tab,{Prolog}) :- 
  pred_juncts_to_list(',',Prolog,LProlog),!,
  prefix_spaces(Tab),pformat_functor('{ '),
   pt_args_arglist(['{}'|FS],Tab+2,'',' | ',' }',LProlog),!.


pt1(FS,Tab,q(E,V,G)):- atom(E), !, T=..[E,V,G],!, pt1(FS,Tab,T).


% xf/yf
pt1(_FS,Tab,T1) :-
  %max_output(Tab,300,T),
  compound_name_arguments(T1,OP, [T]),  
  (current_op(Pri,yf,OP);current_op(Pri,xf,OP)),
  Pri >= 400,
  prefix_spaces(Tab),pformat_functor('( ( '),
  pformat_e_args(T,
   system_portray(Tab+3,T,[right_margin(100)])),
  pformat([') ',OP,' )']),!.

% fx/fy
pt1(_FS,Tab,T1) :-
  %max_output(Tab,300,T),
  compound_name_arguments(T1,OP, [T]),  
  (current_op(Pri,fy,OP);current_op(Pri,fx,OP)),
  Pri >= 400,
  prefix_spaces(Tab), pformat('( '), print_atomf(OP), pformat_functor(' ( '),
  pformat_e_args(T,
   system_portray(Tab+3,T,[right_margin(100)])),
  pformat(') )'), !.

pt1(_FS,Tab,T) :- 
   print_tree_width(W120), \+ using_folding_depth,
   max_output(Tab,W120,T),!,
   system_portray(Tab,T),!.

pt1(FS,Tab,(NPV)) :- use_new, NPV=..[OP,N],
    prefix_spaces(Tab), pformat(OP), pformat('( '), 
    print_tree_no_nl(N), pformat(')'),!,
    ignore(((FS=[Fst|_],Fst=op(_,N),N>1,nl))).

% yfx
pt1(_FS,Tab,T) :-
  compound_name_arity(T,OP, 2),  
  (current_op(Pri,yfx,OP)),
  Pri >= 400,
  pred_juncts_to_list(OP,T,[Y,X]),!,
  prefix_spaces(Tab), pformat_functor('(/*1*/ '),  
    pformat_e_args(T, 
       pt_list_juncts(Tab+2,OP,[Y,X])), 
   pformat(')'),!.
% xfy/xfx
pt1(_FS,Tab,T) :-
  compound_name_arity(T,OP, 2),  
  (current_op(Pri,xfy,OP);current_op(Pri,xfx,OP)),
  Pri >= 400,
  pred_juncts_to_list(OP,T,List),!,
  prefix_spaces(Tab), pformat_functor('(/*2*/ '),  
    pformat_e_args(T, 
       pt_list_juncts(Tab+2,OP,List)), 
   pformat(')'),!.

pt1(FS,Tab,Term) :- 
   compound_name_arguments(Term,F,[Arg]), nonvar(Arg), Arg = [A|Args],
   is_arity_lt1(A), !,
   prefix_spaces(Tab), print_atomf(F), pformat_functor('([ '),
   pt_args_arglist([op(F,1)|FS],Tab+3,'','|','',[A|Args]), !,
   pformat('])').

pt1(FS,Tab,Term) :- 
   compound_name_arguments(Term,F,[Arg]), nonvar(Arg), Arg = [A|Args],
   is_arity_lt1(A),
   prefix_spaces(Tab), print_atomf(F), pformat_functor(format('([ ~p, ',[A])),
   pt_args_arglist([op(F,1)|FS],Tab+3,'','|','',Args), !,
   pformat('])').

pt1(FS,Tab,Term) :- 
   compound_name_arguments(Term,F,[Args]), nonvar(Args), Args = [_|_],
   prefix_spaces(Tab), print_atomf(F), pformat_functor('([ '),
   pt_args_arglist([op(F,1)|FS],Tab+3,'','|','',Args), !,
   pformat('])').

pt1(_FS,Tab,(NPV)) :- NPV=..[OP,N|Args], Args=[Arg], as_is(N), compound(Arg), compound_name_arity(Arg,_,3),!,
  prefix_spaces(Tab), print_atomf(OP),
   pformat('( '), print_tree_no_nl(N), pformat(', '),  
    prefix_spaces(Tab+2),print_tree_no_nl(Arg),pformat(')').

 % include arg1
pt1(_FS,Tab,(NPV)) :- NPV=..[OP,N|Args], as_is(N), % \+ compound_gt(N,0),  
  Args=[Arg], is_list(Arg), is_simple_2x2(Arg),!,
  prefix_spaces(Tab), print_atomf(OP), 
  pformat('( '), print_tree_no_nl(N), pformat_functor(', '),  
 %do_fold_this_round
  current_output_line_position(L), print_simple_2x2(print,L+1,Arg), write(')').

 % include arg1
pt1(FS,Tab,(NPV)) :- NPV=..[OP,N|Args], as_is(N), % \+ compound_gt(N,0),  
  Args=[Arg], is_list(Arg),
  prefix_spaces(Tab), print_atomf(OP), 
  pformat('( '), print_tree_no_nl(N), pformat_functor(', ['),  
 %do_fold_this_round
  pt_args_arglist([op(OP,2)|FS],Tab+2,'','@','])',Arg),!.

 % include arg1
pt1(FS,Tab,(NPV)) :- NPV=..[OP,N|Args], as_is(N), % \+ compound_gt(N,0),  
  Args=[_Arg],
  prefix_spaces(Tab), print_atomf(OP),
  pl_span_goal('functor', ( pformat('( '), print_tree_no_nl(N), pformat(', '))),  
 %do_fold_this_round
  pt_args_arglist([op(OP,2)|FS],Tab+2,'','@',')',Args),!.

% include arg1
pt1(FS,Tab,(NPV)) :- NPV=..[OP,N|Args], as_is(N), % \+ compound_gt(N,0),  
  prefix_spaces(Tab), print_atomf(OP),
  pl_span_goal('functor', ( pformat('( '), print_tree_no_nl(N), pformat(', '))),  
 %do_fold_this_round
  pt_args_arglist([op(OP,2)|FS],Tab+2,'','@',')',Args),!.

pt1(FS,Tab,Term) :- 
   compound_name_arguments(Term,F,Args), length(Args,Arity),
   prefix_spaces(Tab), print_atomf(F), pformat_functor('( '),
   pt_args_arglist([op(F,Arity)|FS],Tab+3,'','@',')',Args), !.



is_colon_mark('=').
is_colon_mark('-').
is_colon_mark(':').
is_colon_mark(':-').
is_colon_mark('-->').
is_colon_mark('->').
is_colon_mark('::').
is_colon_mark('::::').


major_conj(F):-  (F == ',';F == ';' /*;F=='&'*/),!.

splice_off([A0,A|As],[A0|Left],[R|Rest]):- 
   is_arity_lt1(A0), append(Left,[R|Rest],[A|As]), 
    Rest\==[] , %  is_list(Rest),
   ( (\+ is_arity_lt1(R)) ; (length(Left,Len),Len>=6)),!.



pt_args_arglist( _, _, S,_,E,[]):- pt_s_e(S, (pl_span_goal('ellipsis clickprev fold',true),pl_span_goal('args',true)),E).

pt_args_arglist(FS,Tab,S,M,E,[H|TT]):- is_list(T), append(T,[Last],TT), never_as_is(Last),
 Sep = (', '),
 pt_s_e(S,  
  pformat_e_args([H|T],      
    ( prefix_spaces(Tab),
     print_tree_no_nl(H), pt_cont_args(Sep, Tab,Sep, M, FS,T),
     write(Sep),  write(' '),
       pt1(FS,Tab-3,Last)
       % prefix_spaces(Tab-1),print(Last)
      )),E).

pt_args_arglist(FS,Tab,S,M,E,[H|T]):- 
 pt_s_e(S,  
  pformat_e_args([H|T],      
    ( prefix_spaces(Tab),
     print_tree_no_nl(H), pt_cont_args(', ', Tab,', ', M, FS,T))),E).


write_ar_simple(Sep1, _Tab,Sep,[A|R]):- 
 pformat(Sep1),
 ( (wots(S,write_q([A|R])),atom_concat_safety('[',MR,S),atom_concat_safety(M,']',MR), write(M))->true
 ; (write_simple(A), write_simple_each(Sep,R))).

%%	between_down(+Start, ?Count, +End) is nondet.
%
%	Similar to between/3, but can count down if Start > End.

between_down(Start, End, Count) :-
	Start =< End, !,
	between(Start, End, Count).
between_down(Start, End, Count) :-
	nonvar(Count), !,
	between(End, Start, Count).
between_down(Start, End, Count) :-
	Range is Start-End,
	between(0, Range, X),
	Count is Start-X.

rev_append(L,R,LR):- is_list(LR),!, reverse(LR,RL),append(L1,R1,RL),reverse(L1,R),reverse(R1,L).
rev_append(L,R,LR):- append(LL,RR,LR), (var(RR);RR \= [_|_]), !, rev_append(L,R1,LL),append(R1,RR,R).

slice_eq(A, RL , [],Right):- (var(RL);RL\=[_|_];(RL=[E|_],\+ call(A,E))),!,Right=RL.
slice_eq(A,[E|R],[E|List],Right):- slice_eq(A,R,List,Right).


first_n(_,RL,_,_):- (var(RL);RL\=[_|_]),!,fail.
first_n(N,RL,[],RL):- N<1,!.
first_n(N,[E|R],[E|List],Right):- NN is N-1, first_n(NN,R,List,Right).

pt_cont_args(_Sep1, _Ab,_Sep,_Mid,_In, Nil) :- Nil==[], !.
pt_cont_args(_Sep1, Tab,_Sep, Mid, FS, A) :- (var(A) ; A \= [_|_]), !,  pformat(Mid), print_tab_term(Tab,FS,A), !.
%pt_cont_args(Sep1, Tab,_Sep,_Mid, FS,[A|R]) :- R==[], compound(A), pformat(Sep1),!,print_tab_term(Tab-1,FS,A), !.
pt_cont_args(Sep1, Tab,_Sep,_Mid, FS,[A|R]) :- R==[], pformat(Sep1), !, print_tab_term(Tab,FS,A), !.

pt_cont_args(Sep1, Tab,Sep, Mid, FS, RL):-  is_list(RL), last(RL,Last), \+ never_as_is(Last), fail_when_arc_pp,
  wots(S,pt_cont_args_s(Sep1, Tab,Sep, Mid, FS, RL)), write(S),!.
pt_cont_args(Sep1, Tab,Sep, Mid, FS,[A|As]) :- !,  
   pformat(Sep1), print_tab_term(Tab,[lf|FS],A),
   pt_cont_args(Sep, Tab,Sep, Mid,[lf|FS],As).


first_right_not_short(List,[FR|_]):- last(List,Last), display_length(Last,LL),display_length(FR,RL),RL<LL, !, fail.
first_right_not_short(_,_Right):- fail_when_arc_pp, !.

fail_when_arc_pp:- true.
never_as_is(_):-!,fail.
never_as_is(V):- var(V),!,fail.
never_as_is(rhs(_)).
never_as_is(lhs(_)).
never_as_is(edit(_)).
never_as_is(copy(_)).
never_as_is(edit_copy(_,_,_,_,_)).

pt_cont_args_s(Sep1, Tab, Sep, Mid,FS,RL) :- 
    rev_append(List,Right,RL), fail_when_arc_pp,
   length(List,L), L>1, maplist(not_is_list_local,List), max_output(Tab,80,List), 
   first_right_not_short(List,Right), !,
   write_ar_simple(Sep1,Tab,Sep,List),    
   ignore(( Right\==[], write(Sep), nl, prefix_spaces(Tab), pt_cont_args('', Tab,Sep, Mid, FS, Right))).

pt_cont_args_s(Sep1, Tab,Sep, Mid, FS, RL) :- RL=[A|_], is_arity_lt1(A), slice_eq(==(A),RL,List,Right), List\= [_], fail_when_arc_pp,
  first_right_not_short(List,Right), !,
  write_ar_simple(Sep1, Tab,Sep,List),
   ignore(( Right\==[], write(Sep), nl, prefix_spaces(Tab), pt_cont_args('', Tab,Sep, Mid, FS, Right))).

pt_cont_args_s(Sep1, Tab, Sep, Mid, FS, RL) :- first_n(6, RL, List,Right),List\= [_],  max_output(Tab,80,List), fail_when_arc_pp,
   first_right_not_short(List,Right), !,
   write_ar_simple(Sep1, Tab,Sep,List),
   ignore(( Right\==[], write(Sep), nl, prefix_spaces(Tab), pt_cont_args('', Tab,Sep, Mid, FS, Right))).

pt_cont_args_s(Sep1, Tab, Sep,_Mid,_FS, List) :- % ground(List),   
   fail_when_arc_pp,
   is_list(List), length(List,Len),Len>1, Len<6, maplist(is_arity_lt1,List), 
   first_right_not_short([A],R), !,
   pformat(Sep1), notrace(prefix_spaces(Tab)),pformat(' '), List=[A|R], write_simple(A), write_simple_each(Sep,R),!.


:- export(print_tab_term/2).
:- export(print_tab_term/3).

is_arity_lt1(V) :- is_dict(V), !, fail.
is_arity_lt1(S):- notrace(is_arity_lt10(S)).
is_arity_lt1(V):- term_contains_ansi(V),!,fail.
is_arity_lt10(A) :- \+ compound(A),!.
is_arity_lt10(A) :- compound_name_arity(A,_,0),!.
is_arity_lt10(A) :- functor(A,'$VAR',_),!.
is_arity_lt10(S) :- is_charlist(S),!.
is_arity_lt10(S) :- is_codelist(S),!.

not_is_list_local(X):- \+ is_list(X).

on_x_ignore(G):- catch(G,E,(dumpST,write_q(E=on_x_ignore(G)))).

as_is_cmpd(Term) :- \+ compound(Term),!,fail.
as_is_cmpd(Term) :- \+ ground(Term),!,fail.
as_is_cmpd(Term) :- Term=ref(_),!.
as_is_cmpd(Term) :- Term=element(_,_,List),List==[],!.

use_system_portray(Term):- (( \+ compound(Term)); is_arity_lt1(Term); functor(Term,'$VAR',_); \+ compound_gt(Term, 0)),!.
use_system_portray(A=B):- use_system_portray(A),use_system_portray(B),!. 


as_is(V):- as_is0(V),!.

not_string(V):- \+ string(V).


%never_as_is(iz(_)).

as_is0(V):- var(V).
as_is0(A):- never_as_is(A),!,fail.
as_is0(A) :- is_arity_lt1(A), !.
as_is0(V) :- is_dict(V), !, fail.
as_is0(V):- term_contains_ansi(V),!,fail.
as_is0(A) :- functor(A,F,_), simple_f(F), !.
as_is0(A) :- ground(A), A = [ tag(_,_), Atom],atomic(Atom),!.
as_is0(A) :- ground(A), A =  tag(_,_),!.
as_is0(A) :- is_list(A),length(A,L),L>4,!,fail.
as_is0(A) :- is_list(A), maplist(is_arity_lt1,A),maplist(not_string,A),!.
%as_is0([A]) :- is_list(A),length(A,L),on_x_ignore(L<2),!.
as_is0([A|L]) :- L==[],!, as_is0(A).

as_is0(P):- \+ is_list(P), compound_name_arguments(P,N,[A,B]),current_op(_,_,N),as_is0(A),as_is0(B),!.
/*
as_is0(A&B) :- as_is0(A),as_is0(B),!.
as_is0(A:B) :- as_is0(A),as_is0(B),!.
as_is0(A=B) :- as_is0(A),as_is0(B),!.
as_is0(A-B) :- as_is0(A),as_is0(B),!.
as_is0(A/B) :- as_is0(A),as_is0(B),!.
as_is0(A*B) :- as_is0(A),as_is0(B),!.
as_is0(A+B) :- as_is0(A),as_is0(B),!.
*/
as_is0(A) :- functor(A,F,2), simple_fs(F),arg(2,A,One),atomic(One),!.
as_is0(A):- \+ is_list(A), compound_name_arguments(A,_,L),as_is0(L),!.
as_is0('_'(_)) :- !.
as_is0(Q) :- is_quoted_pt(Q).

as_is0(not(A)) :- !,as_is0(A).
as_is0(A) :- A=..[_|S], maplist(is_arity_lt1,S),length(S,SL),SL<5, !.
as_is0(A) :- compound_name_arguments(A,PlusMinus,List),member(PlusMinus,[(+),(-)]),maplist(as_is0,List).
as_is0(A) :- A=..[_,B|S], fail, as_is0(B), maplist(is_arity_lt1,S), !.
% as_is(F):- simple_arg(F), !.

is_quoted_pt(Q):- nonvar(Q), fail, catch(call(call,quote80(Q)),_,fail),!.

simple_fs(:).

simple_f(A):- \+ atom(A),!,fail.
simple_f(denotableBy).
simple_f(iza).
simple_f(c).
simple_f(ip).
simple_f(p).
simple_f(h).
simple_f(sub__examine).
simple_f(isa).
simple_f(has_rel).
simple_f(HasSpace):- atom_contains(HasSpace,' ').

simple_arg(V):- term_contains_ansi(V),!,fail.
simple_arg(S):- (nvar(S) ; \+ compound(S)),!.
%simple_arg(S):- S=[_,A], simple_arg(A), !.
simple_arg(S):- \+ (arg(_,S,Var), compound(Var), \+ nvar(Var)).

nvar(S):- \+ is_arity_lt1(S)-> functor(S,'$VAR',_); var(S).


write_simple_each(_Sep,[]).
write_simple_each(Sep,[A0|Left]):-  pformat(Sep), write_simple(A0), write_simple_each(Sep,Left).


:- export(canonicalise_defaults/2).
canonicalise_defaults(Dict, Out) :- is_dict(Dict), !, dict_pairs(Dict, _, Pairs), canonicalise_defaults2(Pairs, Out).
canonicalise_defaults(In, Out) :- canonicalise_defaults2(In, Out).

canonicalise_defaults2([], []).
canonicalise_defaults2([H0|T0], [H|T]) :- canonicalise_default(H0, H), canonicalise_defaults2(T0, T).
canonicalise_defaults2(H,[O]):- canonicalise_default(H,O).

canonicalise_default(Name=Value, Name=Value) :- !.
canonicalise_default(Name-Value, Name=Value) :- !.
canonicalise_default(NameValue, Name=Value) :- compound(NameValue), compound_name_arguments(NameValue,Name,[Value]),!.
canonicalise_default(Name, Name=_).

:- export(my_merge_options/3).
my_merge_options(N,O,MO):-
  merge_defaults(N,O,M),!,
  swi_option:merge_options([],M,MO).

wots_test(S,G):-freeze(S,(dumpST,break)),wots((SS),G),!,
  set_prolog_flag(access_level,system),trace,ignore((get_attrs(S,Atts))),ignore((get_attrs(SS,Atts))),display(SS=S),ignore(SS=S).

:- export(merge_defaults/3).
merge_defaults([], Old, Merged) :- !, canonicalise_defaults(Old, Merged).
merge_defaults(New, [], Merged) :- !, canonicalise_defaults(New, Merged).
merge_defaults(New, Old, Merged) :- 
    canonicalise_defaults(New, NCanonical),
    canonicalise_defaults(Old, OCanonical),
    merge_canonical_defaults(NCanonical,OCanonical,Merged).

merge_canonical_defaults([],O,O):-!.
merge_canonical_defaults([N=V|T],Old,O):- select(N=_,T,NewT),!,
  merge_canonical_defaults([N=V|NewT],Old,O).
merge_canonical_defaults([N=V|T],Old,O):- select(N=_,Old,NewOld),!,
  merge_canonical_defaults([N=V|T],NewOld,O).
merge_canonical_defaults([N=V|T],Old,[N=V|O]):- 
  merge_canonical_defaults(T,Old,O).
merge_canonical_defaults(O,[],O):-!.


:- system:use_module(library(logicmoo_startup)).


:- fixup_module_exports_into(baseKB).
:- fixup_module_exports_into(system).

% user:portray(Term):- in_pp(swish), print_tree_unit(Term).

% user:portray(Term):- pc_portray(Term),!.



/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2014-2020, University of Amsterdam
                              VU University Amsterdam
                              CWI, Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

%!  pprint_tree(+Term, +Options) is det.
%
%   Pretty print a Prolog term. The following options are processed:
%
%     * output(+Stream)
%     Define the output stream.  Default is =user_output=
%     * right_margin(+Integer)
%     Width of a line.  Default is 72 characters.
%     * left_margin(+Integer)
%     Left margin for continuation lines.  Default is 0.
%     * tab_width(+Integer)
%     Distance between tab-stops.  Default is 8 characters.
%     * indent_arguments(+Spec)
%     Defines how arguments of compound terms are placed.  Defined
%     values are:
%       $ =false= :
%       Simply place them left to right (no line-breaks)
%       $ =true= :
%       Place them vertically, aligned with the open bracket (not
%       implemented)
%       $ =auto= (default) :
%       As horizontal if line-width is not exceeded, vertical
%       otherwise.
%       $ An integer :
%       Place them vertically aligned, <N> spaces to the right of
%       the beginning of the head.
%     * operators(+Boolean)
%     This is the inverse of the write_term/3 option =ignore_ops=.
%     Default is to respect them.
%     * write_options(+List)
%     List of options passed to write_term/3 for terms that are
%     not further processed.  Default:
%       ==
%           [ numbervars(true),
%             quoted(true),
%             portray(true)
%           ]
%       ==

saneify_vars(Term,TermO):- \+ compound(Term),!,Term=TermO.
saneify_vars('VAR$'(Term),'VAR$'(TermO)):- !, to_sane_varname(Term,TermO).
saneify_vars(Term,TermO):- compound_name_arguments(Term,F,Args), maplist(saneify_vars,Args,ArgsO), compound_name_arguments(TermO,F,ArgsO).

to_sane_varname(Term,TermO):- var(Term),!,term_to_atom(Term,TermM),to_sane_varname(TermM,TermO).
to_sane_varname(Term,TermO):- \+ compound(Term),!,toPropercase(Term,TermO).
to_sane_varname(N=V,NO=V):- !, to_sane_varname(N,NO).
to_sane_varname(Term,Term).

pprint_tree(Term, Options) :- select('variable_names'(Vs),Options,OptionsM),!,
  saneify_vars(Term,TermO), maplist(to_sane_varname,Vs,VsO),
  pprint_tree_1(TermO,['variable_names'(VsO)|OptionsM]).
pprint_tree(Term, Options) :-  saneify_vars(Term,TermO), pprint_tree_1(TermO, Options).

%pprint_tree_1(Term, Options) :- prolog_pretty_print:pprint_tree_2(Term, Options).
pprint_tree_1(Term, Options) :- pprint_tree_2(Term, Options).
%pprint_tree(Term, Options) :- \+ \+ pprint_tree_2(Term, Options).

pprint_tree_2(Term, Options0) :-
    prepare_term(Term, Template, Cycles, Constraints),
    defaults(Defs0),
    select_option(write_options(WrtDefs), Defs0, Defs),
    select_option(write_options(WrtUser), Options0, Options1, []),
    merge_options(WrtUser, WrtDefs, WrtOpts),
    merge_options(Options1, Defs, Options2),
    option(max_depth(MaxDepth), WrtOpts, infinite),
    Options = [write_options(WrtOpts)|Options2],

    dict_create(Context, #, [max_depth(MaxDepth)|Options]),
    pp(Template, Context, Options),
    print_extra(Cycles, Context, 'where', Options),
    print_extra(Constraints, Context, 'with constraints', Options).

print_extra([], _, _, _) :- !.
print_extra(List, Context, Comment, Options) :-
    option(output(Out), Options),
    format(Out, ', % ~w', [Comment]),
    modify_context(Context, [indent=4], Context1),
    print_extra_2(List, Context1, Options).

print_extra_2([H|T], Context, Options) :-
    option(output(Out), Options),
    context(Context, indent, Indent),
    indent(Out, Indent, Options),
    pp(H, Context, Options),
    (   T == []
    ->  true
    ;   format(Out, ',', []),
        print_extra_2(T, Context, Options)
    ).


%!  prepare_term(+Term, -Template, -Cycles, -Constraints)
%
%   Prepare a term, possibly  holding   cycles  and  constraints for
%   printing.

prepare_term(Term, Template, Cycles, Constraints) :-
    term_attvars(Term, []),
    !,
    Constraints = [],
    '$factorize_term'(Term, Template, Factors),
    bind_non_cycles(Factors, 1, Cycles),
    numbervars(Template+Cycles+Constraints, 0, _,
               [singletons(true)]).
prepare_term(Term, Template, Cycles, Constraints) :-
    copy_term(Term, Copy, Constraints),
    !,
    '$factorize_term'(Copy, Template, Factors),
    bind_non_cycles(Factors, 1, Cycles),
    numbervars(Template+Cycles+Constraints, 0, _,
               [singletons(true)]).


bind_non_cycles([], _, []).
bind_non_cycles([V=Term|T], I, L) :-
    unify_with_occurs_check(V, Term),
    !,
    bind_non_cycles(T, I, L).
bind_non_cycles([H|T0], I, [H|T]) :-
    H = ('$VAR'(Name)=_),
    atom_concat_safety('_S', I, Name),
    I2 is I + 1,
    bind_non_cycles(T0, I2, T).


defaults([ output(user_output),
           left_margin(0),
           right_margin(172),
           depth(0),
           fold_depth(0),
           indent(0),
           indent_arguments(auto),
           operators(true),
           write_options([ quoted(true),
                           numbervars(true),
                           portray(true),
                           attributes(portray)
                         ]),
           priority(1200)
         ]).


                 /*******************************
                 *             CONTEXT          *
                 *******************************/

context(Ctx, Name, Value) :-
    get_dict(Name, Ctx, Value).

modify_context(Ctx0, Mapping, Ctx) :-
    Ctx = Ctx0.put(Mapping).

dec_depth(Ctx, Ctx) :-
    context(Ctx, max_depth, infinite),
    !.
dec_depth(Ctx0, Ctx) :-
    ND is Ctx0.max_depth - 1,
    Ctx = Ctx0.put(max_depth, ND).


                 /*******************************
                 *              PP              *
                 *******************************/

pp(Primitive, Ctx, Options) :-
    (   atomic(Primitive)
    ;   var(Primitive)
    ;   Primitive = '$VAR'(Var),
        (   integer(Var)
        ;   atom(Var)
        )
    ),
    !,
    pprint(Primitive, Ctx, Options).

pp(AsIs, _Ctx, Options)  :- as_is(AsIs),
   option(output(Out), Options), !,
   with_output_to(Out, write_q(AsIs)),!.


:- if(current_predicate(is_dict/1)).
pp(Dict, Ctx, Options) :-
    is_dict(Dict),
    !,
    dict_pairs(Dict, Tag, Pairs),
    option(output(Out), Options),
    option(indent_arguments(IndentStyle), Options),
    context(Ctx, indent, Indent),
    (   IndentStyle == false ; Pairs == []
    ->  pprint(Dict, Ctx, Options)
    ;   IndentStyle == auto,
        print_width(Dict, Width, Options),
        option(right_margin(RM), Options),
        Indent + Width < RM         % fits on a line, simply write
    ->  pprint(Dict, Ctx, Options)
    ;   format(atom(Buf2), '~q{ ', [Tag]),
        write(Out, Buf2),
        atom_length(Buf2, FunctorIndent),
        (   integer(IndentStyle)
        ->  Nindent is Indent + IndentStyle,
            (   FunctorIndent > IndentStyle
            ->  indent(Out, Nindent, Options)
            ;   true
            )
        ;   Nindent is Indent + FunctorIndent
        ),
        context(Ctx, depth, Depth),
        NDepth is Depth + 1,
        modify_context(Ctx, [indent=Nindent, depth=NDepth], NCtx0),
        dec_depth(NCtx0, NCtx),
        pp_dict_args(Pairs, NCtx, Options),
        BraceIndent is Nindent - 2,         % '{ '
        indent(Out, BraceIndent, Options),
        write(Out, '}')
    ).
:- endif.

pp(Portray, _Ctx, Options) :- 
    option(write_options(WriteOptions), Options),
    option(portray(true), WriteOptions),
    option(output(Out), Options),
    with_output_to(Out, user:portray(Portray)),
    !.

pp(List, Ctx, Options) :-
    List = [_|_],
    !,
    context(Ctx, indent, Indent),
    context(Ctx, depth, Depth),
    option(output(Out), Options),
    option(indent_arguments(IndentStyle), Options),
    (   (   IndentStyle == false
        ->  true
        ;   IndentStyle == auto,
            print_width(List, Width, Options),
            option(right_margin(RM), Options),
            Indent + Width < RM
        )
    ->  pprint(List, Ctx, Options)
    ;   format(Out, '[ ', []),
        Nindent is Indent + 2,
        NDepth is Depth + 1,
        modify_context(Ctx, [indent=Nindent, depth=NDepth], NCtx),
        pp_list_elements(List, NCtx, Options),
        indent(Out, Indent, Options),
        format(Out, ']', [])
    ).

pp(Term, Ctx, Options) :-               % handle operators
    compound(Term),
    compound_name_arity(Term, Name, Arity),
    current_op(Prec, Type, Name),
    match_op(Type, Arity, Kind, Prec, Left, Right),
    option(operators(true), Options),
    !,
    quoted_op(Name, QName),
    option(output(Out), Options),
    context(Ctx, indent, Indent),
    context(Ctx, depth, Depth),
    context(Ctx, priority, CPrec),
    NDepth is Depth + 1,
    modify_context(Ctx, [depth=NDepth], Ctx1),
    dec_depth(Ctx1, Ctx2),
    LeftOptions  = Ctx2.put(priority, Left),
    FuncOptions  = Ctx2.put(embrace, never),
    RightOptions = Ctx2.put(priority, Right),
    (   Kind == prefix
    ->  arg(1, Term, Arg),
        (   (   space_op(Name)
            ;   need_space(Name, Arg, FuncOptions, RightOptions)
            )
        ->  Space = ' '
        ;   Space = ''
        ),
        (   CPrec >= Prec
        ->  format(atom(Buf), '~w~w', [QName, Space]),
            atom_length(Buf, AL),
            NIndent is Indent + AL,
            write(Out, Buf),
            modify_context(Ctx2, [indent=NIndent, priority=Right], Ctx3),
            pp(Arg, Ctx3, Options)
        ;   format(atom(Buf), '(~w~w', [QName,Space]),
            atom_length(Buf, AL),
            NIndent is Indent + AL,
            write(Out, Buf),
            modify_context(Ctx2, [indent=NIndent, priority=Right], Ctx3),
            pp(Arg, Ctx3, Options),
            format(Out, ')', [])
        )
    ;   Kind == postfix
    ->  arg(1, Term, Arg),
        (   (   space_op(Name)
            ;   need_space(Name, Arg, FuncOptions, LeftOptions)
            )
        ->  Space = ' '
        ;   Space = ''
        ),
        (   CPrec >= Prec
        ->  modify_context(Ctx2, [priority=Left], Ctx3),
            pp(Arg, Ctx3, Options),
            format(Out, '~w~w', [Space,QName])
        ;   format(Out, '(', []),
            NIndent is Indent + 1,
            modify_context(Ctx2, [indent=NIndent, priority=Left], Ctx3),
            pp(Arg, Ctx3, Options),
            format(Out, '~w~w)', [Space,QName])
        )
    ;   arg(1, Term, Arg1),
        arg(2, Term, Arg2),
        (   (   space_op(Name)
            ;   need_space(Arg1, Name, LeftOptions, FuncOptions)
            ;   need_space(Name, Arg2, FuncOptions, RightOptions)
            )
        ->  Space = ' '
        ;   Space = ''
        ),
        (   CPrec >= Prec
        ->  modify_context(Ctx2, [priority=Left], Ctx3),
            pp(Arg1, Ctx3, Options),
            format(Out, '~w~w~w', [Space,QName,Space]),
            modify_context(Ctx2, [priority=Right], Ctx4),
            pp(Arg2, Ctx4, Options)
        ;   format(Out, '(', []),
            NIndent is Indent + 1,
            modify_context(Ctx2, [indent=NIndent, priority=Left], Ctx3),
            pp(Arg1, Ctx3, Options),
            format(Out, '~w~w~w', [Space,QName,Space]),
            modify_context(Ctx2, [priority=Right], Ctx4),
            pp(Arg2, Ctx4, Options),
            format(Out, ')', [])
        )
    ).
pp(Term, Ctx, Options) :-               % compound
    option(output(Out), Options),
    option(indent_arguments(IndentStyle), Options),
    context(Ctx, indent, Indent),
    (   IndentStyle == false
    ->  pprint(Term, Ctx, Options)
    ;   IndentStyle == auto,
        print_width(Term, Width, Options),
        option(right_margin(RM), Options),
        Indent + Width < RM         % fits on a line, simply write
    ->  pprint(Term, Ctx, Options)
    ;   compound_name_arguments(Term, Name, Args),
        format(atom(Buf2), '~q(', [Name]),
        write(Out, Buf2),
        atom_length(Buf2, FunctorIndent),
        (   integer(IndentStyle)
        ->  Nindent is Indent + IndentStyle,
            (   FunctorIndent > IndentStyle
            ->  indent(Out, Nindent, Options)
            ;   true
            )
        ;   Nindent is Indent + FunctorIndent
        ),
        context(Ctx, depth, Depth),
        NDepth is Depth + 1,
        modify_context(Ctx, [indent=Nindent, depth=NDepth], NCtx0),
        dec_depth(NCtx0, NCtx),
        pp_compound_args(Args, NCtx, Options),
        write(Out, ')')
    ).


quoted_op(Op, Atom) :-
    is_solo(Op),
    !,
    Atom = Op.
quoted_op(Op, Q) :-
    format(atom(Q), '~q', [Op]).

pp_list_elements(_, Ctx, Options) :-
    context(Ctx, max_depth, 0),
    !,
    option(output(Out), Options),
    write(Out, '...').
pp_list_elements([H|T], Ctx0, Options) :-
    dec_depth(Ctx0, Ctx),
    pp(H, Ctx, Options),
    (   T == []
    ->  true
    ;   nonvar(T),
        T = [_|_]
    ->  option(output(Out), Options),
        write(Out, ','),
        context(Ctx, indent, Indent),
        indent(Out, Indent, Options),
        pp_list_elements(T, Ctx, Options)
    ;   option(output(Out), Options),
        context(Ctx, indent, Indent),
        indent(Out, Indent-2, Options),
        write(Out, '| '),
        pp(T, Ctx, Options)
    ).


pp_compound_args([], _, _).
pp_compound_args([H|T], Ctx, Options) :-
    pp(H, Ctx, Options),
    (   T == []
    ->  true
    ;   T = [_|_]
    ->  option(output(Out), Options),
        write(Out, ','),
        context(Ctx, indent, Indent),
        indent(Out, Indent, Options),
        pp_compound_args(T, Ctx, Options)
    ;   option(output(Out), Options),
        context(Ctx, indent, Indent),
        indent(Out, Indent-2, Options),
        write(Out, '| '),
        pp(T, Ctx, Options)
    ).


:- if(current_predicate(is_dict/1)).
pp_dict_args([Name-Value|T], Ctx, Options) :-
    option(output(Out), Options),
    line_position(Out, Pos0),
    pp(Name, Ctx, Options),
    write(Out, ':'),
    line_position(Out, Pos1),
    context(Ctx, indent, Indent),
    Indent2 is Indent + Pos1-Pos0,
    modify_context(Ctx, [indent=Indent2], Ctx2),
    pp(Value, Ctx2, Options),
    (   T == []
    ->  true
    ;   option(output(Out), Options),
        write(Out, ','),
        indent(Out, Indent, Options),
        pp_dict_args(T, Ctx, Options)
    ).
:- endif.

%       match_op(+Type, +Arity, +Precedence, -LeftPrec, -RightPrec

match_op(fx,    1, prefix,  P, _, R) :- R is P - 1.
match_op(fy,    1, prefix,  P, _, P).
match_op(xf,    1, postfix, P, _, L) :- L is P - 1.
match_op(yf,    1, postfix, P, P, _).
match_op(xfx,   2, infix,   P, A, A) :- A is P - 1.
match_op(xfy,   2, infix,   P, L, P) :- L is P - 1.
match_op(yfx,   2, infix,   P, P, R) :- R is P - 1.


%!  indent(+Out, +Indent, +Options)
%
%   Newline and indent to the indicated  column. Respects the option
%   =tab_width=.  Default  is  4/8.  If  the  tab-width  equals  zero,
%   indentation is emitted using spaces.

indent(Out, Indent, Options) :-
    option(tab_width(TW), Options, 4),
    nl(Out),
    (   TW =:= 0
    ->  tab(Out, Indent)
    ;   Tabs is Indent // TW,
        Spaces is Indent mod TW,
        forall(between(1, Tabs, _), put(Out, 9)),
        tab(Out, Spaces)
    ).

%!  print_width(+Term, -W, +Options) is det.
%
%   Width required when printing `normally' left-to-right.

print_width(Term, W, Options) :-
    option(right_margin(RM), Options),
    (   write_length(Term, W, [max_length(RM)|Options])
    ->  true
    ;   W = RM
    ).

%!  pprint(+Term, +Context, +Options)
%
%   The bottom-line print-routine.

pprint(Term, Ctx, Options) :-
    option(output(Out), Options),
    pprint(Out, Term, Ctx, Options).

pprint(Out, Term, Ctx, Options) :-
    option(write_options(WriteOptions), Options),
    context(Ctx, max_depth, MaxDepth),
    (   MaxDepth == infinite
    ->  write_term(Out, Term, WriteOptions)
    ;   MaxDepth =< 0
    ->  format(Out, '...', [])
    ;   write_term(Out, Term, [max_depth(MaxDepth)|WriteOptions])
    ).


space_op(':-').
% space_op(':').



		 /*******************************
		 *    SHARED WITH term_html.pl	*
		 *******************************/

%!  term(@Term, +Options)// is det.
%
%   Render a Prolog term as  a   structured  HTML  tree. Options are
%   passed to write_term/3. In addition,   the following options are
%   processed:
%
%     - format(+Format)
%     Used for atomic values.  Typically this is used to
%     render a single value.
%     - float_format(+Format)
%     If a float is rendered, it is rendered using
%     `format(string(S), Format, [Float])`
%
%   @tbd    Cyclic terms.
%   @tbd    Attributed terms.
%   @tbd    Portray
%   @tbd    Test with Ulrich's write test set.
%   @tbd    Deal with numbervars and canonical.

bfly_term(Term, Options) -->
    { must_be(acyclic, Term),
      merge_options(Options,
                    [ priority(1200),
                      %max_depth(1 000 000 000),
                       max_depth(3),
                      depth(0)
                    ],
                    Options1),
      dict_options(Dict, Options1)
    },
    html_any(Term, Dict),
     finalize_term(Term, Dict).


html_any(_, Options) -->
    { Options.depth >= Options.max_depth },
    !,
    html(span(class('pl-ellipsis'), '...')).    
html_any(Term, Options) -->
    (   {   nonvar(Term)
        ;   attvar(Term)
        }
    ->  portray(Term, Options)
    ),
    !.    
html_any(Term, Options) -->
    { primitive(Term, Class0),
      !,
      quote_atomic(Term, S, Options),
      primitive_class(Class0, Term, S, Class)
    },
    html(span(class(Class), S)).
html_any(Term, Options) -->
    { blob(Term,Type), Term \== [] },
    !,
    (   blob_rendering(Type,Term,Options)
    ->  []
    ;   html(span(class('pl-blob'),['<',Type,'>']))
    ).
html_any(Term, Options) -->
    { is_dict(Term), !
    },
    html_dict(Term, Options).
html_any(Term, Options) -->
    { assertion((compound(Term);Term==[]))
    },
    html_compound(Term, Options).

%!  html_compound(+Compound, +Options)// is det.
%
%   Process a compound term.



html_compound('$VAR'(Var), Options) -->
    { nop((Options.get(numbervars) == true)),
      !,
      format(string(S), '~W', ['$VAR'(Var), [numbervars(true)]]),
      (   S == "_"
      ->  Class = 'pl-anon'
      ;   Class = 'pl-var'
      )
    },
    html(span(class(Class), S)).
html_compound(List, Options) -->
    { (   List == []
      ;   List = [_|_]                              % May have unbound tail
      ),
      !,
      arg_options(Options, _{priority:999}, ArgOptions)
    },
    html_list(List, ArgOptions).
html_compound({X}, Options) -->
    !,
    { arg_options(Options, _{priority:1200}, ArgOptions) },
    html(span(class('pl-curl'), [ '{', \html_any(X, ArgOptions), '}' ])).

html_compound(OpTerm, Options) -->
    { compound_name_arity(OpTerm, Name, 1),
      is_op1(Name, Type, Pri, ArgPri, Options),
      \+ Options.get(ignore_ops) == true
    },
    !,
    op1(Type, Pri, OpTerm, ArgPri, Options).
html_compound(OpTerm, Options) -->
    { compound_name_arity(OpTerm, Name, 2),
      is_op2(Name, LeftPri, Pri, RightPri, Options),
      \+ Options.get(ignore_ops) == true
    },
    !,
    op2(Pri, OpTerm, LeftPri, RightPri, Options).


html_compound(Compound, Options) -->
    { compound_name_arity(Compound, Name, 0),
      quote_atomic(Name, S, Options.put(embrace, never)),
      extra_classes(Classes, Options)
    },
    html(span(class(['pl-compound'|Classes]),
              [ span(class('pl-functor'), S),
                '(',
                ')'
              ])).


html_compound(Compound, Options) -->
    { compound_name_arity(Compound, Name, Arity),
      quote_atomic(Name, S, Options.put(embrace, never)),
      arg_options(Options, _{priority:999}, ArgOptions),
      extra_classes(Classes, Options)
    },
    html(span(class(['pl-compound'|Classes]),
              [ span(class('pl-functor'), S),
                '(',
                \ html_args(0, Arity, Compound, ArgOptions),
                ')'
              ])).

extra_classes(['pl-level-0'], Options) :-
    Options.depth == 0,
    !.
extra_classes([], _).

html_raw(S) --> [S].
:- export(html_raw/3).
%!  arg_options(+Options, -OptionsOut) is det.
%!  arg_options(+Options, +Extra, -OptionsOut) is det.
%
%   Increment depth in Options.

arg_options(Options, Options.put(depth, NewDepth)) :-
    NewDepth is Options.depth+1.
arg_options(Options, Extra, Options.put(depth, NewDepth).put(Extra)) :-
    NewDepth is Options.depth+1.

%!  html_args(+Arg0, +Arity, +Compound, +Options)//
%
%   Emit arguments of a compound term.

html_args(I, Arity, Compound, ArgOptions) -->
   html(span(class(['pl-args']),
              [ \ args_each(I, Arity, Compound, ArgOptions)
              ])).

args_each(Arity, Arity, _, _) --> !.
args_each(I, Arity, Compound, ArgOptions) -->
    { NI is I + 1,
      arg(NI, Compound, Arg)
    },
    html_any(Arg, ArgOptions),
    (   {NI == Arity}
    ->  []
    ;   html(', '),
        args_each(NI, Arity, Compound, ArgOptions)
    ).

%!  html_list(+List, +Options)//
%
%   Emit a html_list.  The List may have an unbound tail.

html_list(List, _Options) --> {List== []},!, html('[]').
html_list(List, Options) -->
       html(span(class(['pl-list']),
              [ 
              span(class('pl-functor'), ' [ '),
               span(class(['pl-args']), [ \list_content(List, Options)]),
              ']'
              ])
       ),!.

html_list(List, Options) -->
    html(span(class('pl-list'),
              ['[', \list_content(List, Options),
               ']'
              ])).

list_content([], _Options) -->
    !,
    [].

list_content([H|T], Options) -->
    !,
    { arg_options(Options, ArgOptions)
    },
    html_any(H, Options),
    (   {T == []}
    ->  []
    ;   { Options.depth + 1 >= Options.max_depth }
    ->  html(['|',span(class('pl-ellipsis'), '...')])
    ;   {var(T) ; \+ T = [_|_]}
    ->  html('|'),
        tail(T, ArgOptions)
    ;   html(', '),
        list_content(T, ArgOptions)
    ).

tail(Value, Options) -->
    {   var(Value)
    ->  Class = 'pl-var-tail'
    ;   Class = 'pl-nonvar-tail'
    },
    html(span(class(Class), \ html_any(Value, Options))).

%!  is_op1(+Name, -Type, -Priority, -ArgPriority, +Options) is semidet.
%
%   True if Name is an operator taking one argument of Type.

is_op1(Name, Type, Pri, ArgPri, Options) :-
    operator_module(Module, Options),
    current_op(Pri, OpType, Module:Name),
    argpri(OpType, Type, Pri, ArgPri),
    !.

argpri(fx, prefix,  Pri0, Pri) :- Pri is Pri0 - 1.
argpri(fy, prefix,  Pri,  Pri).
argpri(xf, postfix, Pri0, Pri) :- Pri is Pri0 - 1.
argpri(yf, postfix, Pri,  Pri).

%!  is_op2(+Name, -LeftPri, -Pri, -RightPri, +Options) is semidet.
%
%   True if Name is an operator taking two arguments of Type.

is_op2(Name, LeftPri, Pri, RightPri, Options) :-
    operator_module(Module, Options),
    current_op(Pri, Type, Module:Name),
    infix_argpri(Type, LeftPri, Pri, RightPri),
    !.

infix_argpri(xfx, ArgPri, Pri, ArgPri) :- ArgPri is Pri - 1.
infix_argpri(yfx, Pri, Pri, ArgPri) :- ArgPri is Pri - 1.
infix_argpri(xfy, ArgPri, Pri, Pri) :- ArgPri is Pri - 1.

%!  operator_module(-Module, +Options) is det.
%
%   Find the module for evaluating operators.

operator_module(Module, Options) :-
    Module = Options.get(module),
    !.
operator_module(TypeIn, _) :-
    '$module'(TypeIn, TypeIn).

%!  op1(+Type, +Pri, +Term, +ArgPri, +Options)// is det.

op1(Type, Pri, Term, ArgPri, Options) -->
    { Pri > Options.priority },
    !,
    html(['(', \op1(Type, Term, ArgPri, Options), ')']).
op1(Type, _, Term, ArgPri, Options) -->
    op1(Type, Term, ArgPri, Options).

op1(prefix, Term, ArgPri, Options) -->
    { Term =.. [Functor,Arg],
      arg_options(Options, DepthOptions),
      FuncOptions = DepthOptions.put(embrace, never),
      ArgOptions  = DepthOptions.put(priority, ArgPri),
      quote_atomic(Functor, S, FuncOptions),
      extra_classes(Classes, Options)
    },
    html(span(class(['pl-compound'|Classes]),
              [ span(class('pl-prefix'), S),
                \space(Functor, Arg, FuncOptions, ArgOptions),
                \html_any(Arg, ArgOptions)
              ])).
op1(postfix, Term, ArgPri, Options) -->
    { Term =.. [Functor,Arg],
      arg_options(Options, DepthOptions),
      ArgOptions = DepthOptions.put(priority, ArgPri),
      FuncOptions = DepthOptions.put(embrace, never),
      quote_atomic(Functor, S, FuncOptions),
      extra_classes(Classes, Options)
    },
    html(span(class(['pl-compound'|Classes]),
              [ \ html_any(Arg, ArgOptions),
                \ space(Arg, Functor, ArgOptions, FuncOptions),
                span(class('pl-postfix'), S)
              ])).

%!  op2(+Pri, +Term, +LeftPri, +RightPri, +Options)// is det.

op2(Pri, Term, LeftPri, RightPri, Options) -->
    { Pri > Options.priority },
    !,
    html(['(', \op2(Term, LeftPri, RightPri, Options), ')']).
op2(_, Term, LeftPri, RightPri, Options) -->
    op2(Term, LeftPri, RightPri, Options).

op2(Term, LeftPri, RightPri, Options) -->
    { Term =.. [Functor,Left,Right],
      arg_options(Options, DepthOptions),
      LeftOptions  = DepthOptions.put(priority, LeftPri),
      FuncOptions  = DepthOptions.put(embrace, never),
      RightOptions = DepthOptions.put(priority, RightPri),
      (   (   need_space(Left, Functor, LeftOptions, FuncOptions)
          ;   need_space(Functor, Right, FuncOptions, RightOptions)
          )
      ->  Space = ' '
      ;   Space = ''
      ),
      quote_op(Functor, S, Options),
      extra_classes(Classes, Options)
    },
    html(span(class(['pl-compound'|Classes]),
              [ \html_any(Left, LeftOptions),
                Space,
                span(class('pl-infix'), S),
                Space,
                \html_any(Right, RightOptions)
              ])).

%!  space(@T1, @T2, +Options)//
%
%   Emit a space if omitting a space   between T1 and T2 would cause
%   the two terms to join.

space(T1, T2, LeftOptions, RightOptions) -->
    { need_space(T1, T2, LeftOptions, RightOptions) },
    html(' ').
space(_, _, _, _) -->
    [].

%!  need_space(@Term1, @Term2, +LeftOptions, +RightOptions)
%
%   True if a space is  needed  between   Term1  and  Term2  if they are
%   printed using the given option lists.

need_space(T1, T2, _, _) :-
    (   is_solo(T1)
    ;   is_solo(T2)
    ),
    !,
    fail.
need_space(T1, T2, LeftOptions, RightOptions) :-
    end_code_type(T1, TypeR, LeftOptions.put(side, right)),
    end_code_type(T2, TypeL, RightOptions.put(side, left)),
    \+ no_space(TypeR, TypeL).

no_space(punct, _).
no_space(_, punct).
no_space(quote(R), quote(L)) :-
    !,
    R \== L.
no_space(alnum, symbol).
no_space(symbol, alnum).

%!  end_code_type(+Term, -Code, Options)
%
%   True when code is the first/last character code that is emitted
%   by printing Term using Options.

end_code_type(_, Type, Options) :-
    MaxDepth = Options.max_depth,
    integer(MaxDepth),
    Options.depth >= MaxDepth,
    !,
    Type = symbol.
end_code_type(Term, Type, Options) :-
    primitive(Term, _),
    !,
    quote_atomic(Term, S, Options),
    end_type(S, Type, Options).
end_code_type(Dict, Type, Options) :-
    is_dict(Dict, Tag),
    !,
    (   Options.side == left
    ->  end_code_type(Tag, Type, Options)
    ;   Type = punct
    ).
end_code_type('$VAR'(Var), Type, Options) :-
    Options.get(numbervars) == true,
    !,
    format(string(S), '~W', ['$VAR'(Var), [numbervars(true)]]),
    end_type(S, Type, Options).
end_code_type(List, Type, _) :-
    (   List == []
    ;   List = [_|_]
    ),
    !,
    Type = punct.
end_code_type(OpTerm, Type, Options) :-
    compound_name_arity(OpTerm, Name, 1),
    is_op1(Name, OpType, Pri, ArgPri, Options),
    \+ Options.get(ignore_ops) == true,
    !,
    (   Pri > Options.priority
    ->  Type = punct
    ;   (   OpType == prefix
        ->  end_code_type(Name, Type, Options)
        ;   arg(1, OpTerm, Arg),
            arg_options(Options, ArgOptions),
            end_code_type(Arg, Type, ArgOptions.put(priority, ArgPri))
        )
    ).
end_code_type(OpTerm, Type, Options) :-
    compound_name_arity(OpTerm, Name, 2),
    is_op2(Name, LeftPri, Pri, _RightPri, Options),
    \+ Options.get(ignore_ops) == true,
    !,
    (   Pri > Options.priority
    ->  Type = punct
    ;   arg(1, OpTerm, Arg),
        arg_options(Options, ArgOptions),
        end_code_type(Arg, Type, ArgOptions.put(priority, LeftPri))
    ).
end_code_type(Compound, Type, Options) :-
    compound_name_arity(Compound, Name, _),
    end_code_type(Name, Type, Options).

end_type(S, Type, Options) :-
    number(S),
    !,
    (   (S < 0 ; S == -0.0),
        Options.side == left
    ->  Type = symbol
    ;   Type = alnum
    ).
end_type(S, Type, Options) :-
    Options.side == left,
    !,
    sub_string(S, 0, 1, _, Start),
    syntax_type(Start, Type).
end_type(S, Type, _) :-
    sub_string(S, _, 1, 0, End),
    syntax_type(End, Type).

syntax_type("\"", quote(double)) :- !.
syntax_type("\'", quote(single)) :- !.
syntax_type("\`", quote(back))   :- !.
syntax_type(S, Type) :-
    string_code(1, S, C),
    (   code_type(C, prolog_identifier_continue)
    ->  Type = alnum
    ;   code_type(C, prolog_symbol)
    ->  Type = symbol
    ;   code_type(C, space)
    ->  Type = layout
    ;   Type = punct
    ).


%!  html_dict(+Term, +Options)//

html_dict(Term, Options) -->
    { dict_pairs(Term, Tag, Pairs),
      quote_atomic(Tag, S, Options.put(embrace, never)),
      arg_options(Options, ArgOptions)
    },
    html(span(class('pl-dict'),
              [ span(class('pl-tag'), S),
                '{',
                \ dict_kvs_html(Pairs, ArgOptions),
                '}'
              ])).

dict_kvs_html([], _) --> [].
dict_kvs_html(_, Options) -->
    { Options.depth >= Options.max_depth },
    !,
    html(span(class('pl-ellipsis'), '...')).
dict_kvs_html(KVs, Options) -->
    dict_kvs2(KVs, Options).

dict_kvs2([K-V|T], Options) -->
    { quote_atomic(K, S, Options),
      end_code_type(V, VType, Options.put(side, left)),
      (   VType == symbol
      ->  VSpace = ' '
      ;   VSpace = ''
      ),
      arg_options(Options, ArgOptions)
    },
    html([ span(class('pl-key'), S),
           ':',                             % FIXME: spacing
           VSpace,
           \html_any(V, ArgOptions)
         ]),
    (   {T==[]}
    ->  []
    ;   html(', '),
        dict_kvs2(T, Options)
    ).

quote_atomic(Str, String, Options) :-
    \+ (Options.get(quoted) == false),
    (string(Str);atom(Str)),
    !,
    format(string(String), '~q', [Str]).
quote_atomic(Float, String, Options) :-
    float(Float),
    Format = Options.get(float_format),
    !,
    format(string(String), Format, [Float]).
quote_atomic(Plain, String, Options) :-
    atomic(Plain),
    Format = Options.get(format),
    !,
    format(string(String), Format, [Plain]).
quote_atomic(Plain, String, Options) :-
    rational(Plain),
    \+ integer(Plain),
    !,
    operator_module(Module, Options),
    format(string(String), '~W', [Plain, [module(Module)]]).
quote_atomic(Plain, Plain, _) :-
    number(Plain),
    !.
quote_atomic(Plain, String, Options) :-
    Options.get(quoted) == true,
    !,
    (   Options.get(embrace) == never
    ->  format(string(String), '~q', [Plain])
    ;   format(string(String), '~W', [Plain, Options])
    ).
quote_atomic(Var, String, Options) :-
    var(Var),
    !,
    format(string(String), '~W', [Var, Options]).
quote_atomic(Plain, Plain, _).

quote_op(Op, S, _Options) :-
    is_solo(Op),
    !,
    S = Op.
quote_op(Op, S, Options) :-
    quote_atomic(Op, S, Options.put(embrace,never)).

is_solo(Var) :-
    var(Var), !, fail.
is_solo(',').
is_solo(';').
is_solo('!').

%!  primitive(+Term, -Class) is semidet.
%
%   True if Term is a primitive term, rendered using the CSS
%   class Class.

primitive(Term, _Type) :- compound(Term),!,fail.
primitive(Term, Type) :- var(Term),      !, Type = 'pl-avar'.
primitive(Term, Type) :- atom(Term),     !, Type = 'pl-atom'.
primitive(Term, Type) :- string(Term),   !, Type = 'pl-string'.
primitive(Term, Type) :- integer(Term),  !, Type = 'pl-int'.
primitive(Term, Type) :- rational(Term), !, Type = 'pl-rational'.
primitive(Term, Type) :- float(Term),    !, Type = 'pl-float'.

%!  primitive_class(+Class0, +Value, -String, -Class) is det.
%
%   Fixup the CSS class for lexical variations.  Used to find
%   quoted atoms.

primitive_class('pl-atom', Atom, String, Class) :-
    \+ atom_string(Atom, String),
    !,
    Class = 'pl-atom'.
primitive_class(Class, _, _, Class).


%!  finalize_term(+Term, +Dict)// is det.
%
%   Handle the full_stop(Bool) and nl(Bool) options.

finalize_term(Term, Dict) -->
    (   { true == Dict.get(full_stop) }
    ->  space(Term, '.', Dict, Dict),
        (   { true == Dict.get(nl) }
        ->  html(['.', p([])])
        ;   html('. ')
        )
    ;   (   { true == Dict.get(nl) }
        ->  html(p([]))
        ;   []
        )
    ).


                 /*******************************
                 *             HOOKS            *
                 *******************************/

%!  blob_rendering(+BlobType, +Blob, +WriteOptions)// is semidet.
%
%   Hook to render blob atoms as HTML.  This hook is called whenever
%   a blob atom is encountered while   rendering  a compound term as
%   HTML. The blob type is  provided   to  allow  efficient indexing
%   without having to examine the blob. If this predicate fails, the
%   blob is rendered as an HTML SPAN with class 'pl-blob' containing
%   BlobType as text.


%:- fixup_exports.
:- multifile(user:portray/1).
:- dynamic(user:portray/1).
user:portray(S):- term_is_ansi(S), !, write_keeping_ansi(S).
user:portray(Term):- 
  %fail, 
  notrace(pc_portray(Term)),!.
