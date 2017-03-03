/* Part of LogicMOO Base Logicmoo Path Setups
% ===================================================================
    File:         'logicmoo_util_library.pl'
    Purpose:       To load the logicmoo libraries as needed
    Contact:       $Author: dmiles $@users.sourceforge.net ;
    Version:       'logicmoo_util_library.pl' 1.0.0
    Revision:      $Revision: 1.7 $
    Revised At:    $Date: 2002/07/11 21:57:28 $
    Author:        Douglas R. Miles
    Maintainers:   TeamSPoon
    E-mail:        logicmoo@gmail.com
    WWW:           http://www.prologmoo.com
    SCM:           https://github.com/TeamSPoon/PrologMUD/tree/master/pack/logicmoo_base
    Copyleft:      1999-2015, LogicMOO Prolog Extensions
    License:       Lesser GNU Public License
% ===================================================================
*/

% We save the name of the module loading this module
:- module(logicmoo_util_common,[add_history/1,qsave_lm/1,ignore_not_not/1,load_library_system/1,fixup_exports/0]).

% sets upo to restore the subsystems
:- meta_predicate(load_library_system(:)).
load_library_system(M:File):- load_library_system(M,File). 
load_library_system(M,File):- during_boot(gripe_time(40,(if_file_exists(ensure_loaded(M:File))))).
:- system:import(load_library_system/2).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DEFAULT PROLOG FLAGS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- set_prolog_flag(lm_expanders,default).
:- set_prolog_flag(dialect_pfc,default).
:- set_prolog_flag(qcompile,part).
:- set_prolog_flag(do_renames,never).
:- if( \+ current_module(prolog_stack)).
:- system:use_module(library(prolog_stack)).
 prolog_stack:stack_guard(none).
:- endif.


% invert_varname(NV):-  ignore(((NV=(N=V), V='$VAR'(N)))).

ignore_not_not(G):- ignore((catch((( \+ \+ (ignore(once(G))))),_,fail))),!.

make_historial(O,A):-ground(O),format(atom(A), '~W', [O, [fullstop(true),portrayed(true),quoted(true),numbervars(true)]]).
make_historial(O,A):-
    prolog_load_context(variable_names, Bindings),
    format(atom(A), '~W', [O, [fullstop(true),portrayed(true),quoted(true),variable_names(Bindings)]]).

add_history(O):- 
   ignore_not_not((make_historial(O,A),add_history0(A))),!.

add_history0(A):- 
   (current_predicate(system:rl_add_history/1) -> system:rl_add_history(A) ; true),
   (current_predicate(editline:el_add_history/2) -> editline:el_add_history(user_input,A) ; true).



system:nb_linkval_current(N,V):-duplicate_term(V,VV),V=VV,nb_linkval(N,VV),nb_current(N,V).

extend_varnames(ExpandedBindings):- 
    prolog_load_context(variable_names,Vs),
    append(Vs,ExpandedBindings,NewVs),    
    append(NewVs,[],NewVs),
    nb_linkval_current('$variable_names',NewVs).

:- user:dynamic(expand_query/4).
:- user:multifile(expand_query/4).

user:expand_query(Goal, _Expanded, Bindings, _ExpandedBindings):-
   ignore_not_not((once(( nb_linkval_current('$expand_query',Goal-Bindings),
    append(Bindings,[],Bindings),
    format(atom(A), '~W', [Goal, [fullstop(true),portrayed(true),quoted(true),variable_names(Bindings)]]),
    add_history0(A))))),
   fail.
       

:- user:dynamic(expand_answer/2).
:- user:multifile(expand_answer/2).

user:expand_answer(Bindings, ExpandedBindings):- 
    nb_linkval_current('$expand_answer',Bindings),
    toplevel_variables:expand_answer(Bindings, ExpandedBindings),
    nb_linkval_current('$expand_answer',ExpandedBindings).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DURING/AFTER BOOT HOOKS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- dynamic(system:after_boot_goal/1).
:- meta_predicate(system:during_boot(:)).
system:after_boot_call(How):- forall(system:after_boot_goal(Goal),call(How,Goal)),threads.
system:after_boot_call:- baseKB:after_boot_call(must_det).
system:during_boot(Goal):- call(Goal),after_boot(Goal). 
:- meta_predicate(system:after_boot(:)).
system:after_boot(Goal):- add_history(Goal),system:assertz(after_boot_goal(Goal)).
:- meta_predicate(system:after_boot_sanity_test(:)).
system:after_boot_sanity_test(M:Goal):- after_boot(M:sanity(Goal)).


qsave_lm(LM):- statistics(globallimit,G),statistics(locallimit,L),statistics(traillimit,T),
  qsave_program(LM,[toplevel(logicmoo_toplevel),
   goal(logicmoo_goal),op(save),
       % stand_alone(true),
       % class(development),
       % autoload(false),
       % foreign(no_save),
       global(G),trail(T),local(L)]),!.


/*
:- dynamic(baseKB:logicmoo_utils_separate/0).
:- retractall(baseKB:logicmoo_utils_separate).
:- set_prolog_flag(generate_debug_info, true).


% :- abox:defaultTBoxMt(_)->true;('$current_typein_module'(M),asserta(abox:defaultTBoxMt(M))).


:- dynamic(baseKB:mpred_is_impl_file/1).
:- multifile(baseKB:mpred_is_impl_file/1).
% :- volatile(baseKB:mpred_is_impl_file/1).


*/

:- module_transparent(fixup_exports/0).

fixup_exports:-
 ignore((source_location(S,_), prolog_load_context(module,LC),
 forall(source_file(M:H,S),
 ((on_x_debug(functor(H,F,A)),
  must((ignore(((atom(LC),atom(M), LC\==M,M:export(M:F/A),LC:multifile(M:F/A),fail,atom_concat('$',_,F),LC:import(M:F/A)))))),
  ignore(((\+ atom_concat('$',_,F),\+ atom_concat('__aux',_,F),LC:export(M:F/A), (current_predicate(system:F/A)->true; system:import(M:F/A))))),
  ignore(((\+ predicate_property(M:H,transparent), module_transparent(M:F/A), \+ atom_concat('__aux',_,F),debug(modules,'~N:- module_transparent((~q)/~q).~n',[F,A]))))))))).

:- fixup_exports.
