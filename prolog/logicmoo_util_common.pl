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
:- module(logicmoo_util_common,[add_history/1,add_history0/1,make_historial/2,
   normally/1,var_non_attvar/1,
   qsave_lm/0,qsave_lm/1,ignore_not_not/1,fixup_exports/0,
          all_source_file_predicates_are_transparent/0,
          all_source_file_predicates_are_transparent/2,
          all_source_file_predicates_are_exported/0,
          all_source_file_predicates_are_exported/2,
          add_file_search_path_safe/2,
          set_prolog_stack_gb/1,
          load_library_system/1,
          add_file_search_path_safe/2,
          pack_upgrade/0,run_prologmud/0,init_logicmoo/0,
          shared_vars/3
          ]).



:- meta_predicate(system:whenever_flag_permits(+,:)).
system:whenever_flag_permits(Flag,G):- (current_prolog_flag(Flag,false) -> true ; G).



%=======================================
% Load only if exists
%=======================================

% sets up and restore the subsystems
:- meta_predicate(load_library_system(:)).
load_library_system(M:File):- load_library_system(M,File). 

load_library_system(user,File):-!, during_boot(gripe_time(40,(if_file_exists(ensure_loaded(system:File))))).
load_library_system(M,File):- during_boot(gripe_time(40,(if_file_exists(ensure_loaded(M:File))))).
:- system:import(load_library_system/2).

during_boot(G):- during_init(G).
after_boot(G):- at_init(G).

% doesnt run if --nonet
:- meta_predicate(system:during_net_boot(:)).
system:during_net_boot(M:Goal):- during_boot(whenever_flag_permits(run_network,M:Goal)).

% --nonet
:- meta_predicate(system:after_net_boot(:)).
system:after_net_boot(M:Goal):- after_boot(whenever_flag_permits(run_network,M:Goal)).

:- meta_predicate(system:after_boot_sanity_test(:)).
system:after_boot_sanity_test(M:Goal):- after_boot(M:sanity(M:Goal)).



:- meta_predicate iff_defined(*).
:- meta_predicate iff_defined(:,0).
:- module_transparent((iff_defined/1,iff_defined/2)).

%% iff_defined( ?G) is semidet.
%
% If Defined.
%
iff_defined(Goal):- iff_defined(Goal,((dmsg(warn_undefined(Goal))),!,fail)).

%% iff_defined( ?Goal, :GoalElse) is semidet.
%
% If Defined Else.
%
iff_defined(Goal,Else):- current_predicate(_,Goal)*->Goal;Else.
% iff_defined(M:Goal,Else):- !, current_predicate(_,OM:Goal),!,OM:Goal;Else.
%iff_defined(Goal,  Else):- current_predicate(_,OM:Goal)->OM:Goal;Else.



:- module_transparent((add_history/1,qsave_lm/1,ignore_not_not/1,load_library_system/1,fixup_exports/0,
          all_source_file_predicates_are_transparent/0,
          all_source_file_predicates_are_transparent/2,
          all_source_file_predicates_are_exported/0,
          all_source_file_predicates_are_exported/2)).

:- meta_predicate(ignore_not_not(0)).
:- export(pack_upgrade/0).
pack_upgrade:- call((user:use_module(library(prolog_pack)),use_module(library(predicate_streams)), 
  with_input_from_predicate(({}/[X]>>(repeat,X='YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY')),
    forall(call(prolog_pack:current_pack(Pack)),maybe_pack_upgrade(Pack))))).

maybe_pack_upgrade(Pack):- pack_property(Pack, directory(PackDir)),\+ access_file(PackDir,write),!.
maybe_pack_upgrade(Pack):- pack_upgrade(Pack).



normally(G):- locally(set_prolog_flag(runtime_debug,0),locally(set_prolog_flag(bugger,false),G)).


shared_vars(Left,Right,SVG):-quietly(( term_variables(Left,Vs1),term_variables(Right,Vs2),intersect_eq0(Vs2,Vs1,SVG))).

 intersect_eq0([], _, []).
 intersect_eq0([X|Xs], Ys, L) :-
         (   member_eq0(X, Ys)
         ->  L = [X|T],
             intersect_eq0(Xs, Ys, T)
         ;   intersect_eq0(Xs, Ys, L)
         ).


% ======================================================
% Add Extra file_search_paths
% ======================================================
:- dynamic(user:file_search_path/2).
:- multifile(user:file_search_path/2).


add_file_search_path_safe(Name,Path):-  absolute_directory(Path,Dir),!,
   is_absolute_file_name(Dir), (( \+ user:file_search_path(Name,Dir)) ->asserta(user:file_search_path(Name,Dir));true).
add_file_search_path_safe(Name,Path):- writeln('user_error',skip(add_file_search_path_safe(Name,Path))),!.

absolute_directory(Dir,Dir):- atom(Dir),is_absolute_file_name(Dir),exists_directory(Dir),!.
absolute_directory(Dir,ABS):- absolute_file_name(Dir,ABS,[file_type(directory),solutions(all),expand(true),case_sensitive(false),access(read),file_errors(fail)]),exists_directory(ABS),!.
absolute_directory(Dir,ABS):- absolute_file_name(library(Dir),ABS,[file_type(directory),solutions(all),case_sensitive(false),expand(true),access(read),file_errors(fail)]),exists_directory(ABS),!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DEFAULT PROLOG FLAGS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 % :- set_prolog_flag(subclause_expansion,default).
 % :- set_prolog_flag(subclause_expansion,false).
:- set_prolog_flag(dialect_pfc,default).
:- set_prolog_flag(qcompile,part).
:- set_prolog_flag(do_renames,never).
:- if( \+ current_module(prolog_stack)).
:- user:use_module(library(prolog_stack)).
 prolog_stack:stack_guard(none).
:- endif.


/*
system:logicmoo_user_stacks:- Six = 6, set_prolog_stack(global, limit(Six*10**9)),
  set_prolog_stack(local, limit(Six*10**9)),set_prolog_stack(trail, limit(Six*10**9)).

:- rtrace,during_boot(system:logicmoo_user_stacks).
:- Six = 6, set_prolog_stack(global, limit(Six*10**9)),set_prolog_stack(local, limit(Six*10**9)),set_prolog_stack(trail, limit(Six*10**9)).

:- set_prolog_stack(global, limit(32*10**9)).
:- set_prolog_stack(local, limit(32*10**9)).
:- set_prolog_stack(trail, limit(32*10**9)).
:- module_transparent( (set_prolog_stack_gb)/1).
:- during_boot(set_prolog_stack_gb(16)).


*/

%% set_prolog_stack_gb( +Six) is semidet.
%
% Set Prolog Stack Gb.
%
set_prolog_stack_gb(Six):-set_prolog_stack(global, limit(Six*10**9)),set_prolog_stack(local, limit(Six*10**9)), set_prolog_stack(trail, limit(Six*10**9)).



%% all_source_file_predicates_are_exported() is det.
%
% All Module Predicates Are Exported.

:- module_transparent(all_source_file_predicates_are_exported/0).
all_source_file_predicates_are_exported:- current_prolog_flag(xref,true),!.
all_source_file_predicates_are_exported:-
 source_location(S,_), prolog_load_context(module,LC),
 all_source_file_predicates_are_exported(S,LC).

lmconfig:never_export_named(attr_unify_hook/2).
lmconfig:never_export_named(attribute_goals/3).
lmconfig:never_export_named(project_attributes/2).
lmconfig:never_export_named(attr_portray_hook/2).

% :- module_transparent(all_source_file_predicates_are_exported/2).
all_source_file_predicates_are_exported(S,LC):-
 forall(source_file(M:H,S),
 ignore((functor(H,F,A), \+ atom_concat('$',_,F), \+ lmconfig:never_export_named(F/A),
  ((ignore(((atom(LC),atom(M), LC\==M,M:export(M:F/A),LC:multifile(M:F/A),fail,atom_concat('$',_,F),LC:import(M:F/A)))))),
  ignore(((\+ atom_concat('$',_,F),\+ atom_concat('__aux',_,F),LC:export(M:F/A), 
  (current_predicate(system:F/A)->true; system:import(M:F/A)))))))).

%% all_source_file_predicates_are_transparent() is det.
%
% All Module Predicates Are Transparent.
:- module_transparent(all_source_file_predicates_are_transparent/0).
all_source_file_predicates_are_transparent:-
 source_location(S,_), prolog_load_context(module,LC),
 all_source_file_predicates_are_transparent(S,LC).

:- module_transparent(all_source_file_predicates_are_transparent/2).
all_source_file_predicates_are_transparent(S,_LC):- 
 forall(source_file(M:H,S),
 (functor(H,F,A),
  ignore(((\+ predicate_property(M:H,transparent), module_transparent(M:F/A), 
  \+ atom_concat('__aux',_,F),debug(modules,'~N:- module_transparent((~q)/~q).~n',[F,A])))))).


:- module_transparent(fixup_exports/0).
fixup_exports:- 
   all_source_file_predicates_are_exported,
   all_source_file_predicates_are_transparent.

var_non_attvar(V):- var(V),\+ attvar(V).


getenv_or(Name,ValueO,Default):-
   (getenv(Name,RV)->Value=RV;Value=Default),
   ( \+ number(Value) -> atom_number(Value,ValueO); Value=ValueO).


/*
:- dynamic(baseKB:logicmoo_utils_separate/0).
:- retractall(baseKB:logicmoo_utils_separate).
:- set_prolog_flag(generate_debug_info, true).


% :- abox:defaultTBoxMt(_)->true;('$current_typein_module'(M),asserta(abox:defaultTBoxMt(M))).


:- dynamic(baseKB:mpred_is_impl_file/1).
:- multifile(baseKB:mpred_is_impl_file/1).
% :- volatile(baseKB:mpred_is_impl_file/1).


*/



:- if( app_argv('--upgrade') ).
:- whenever(run_network,pack_upgrade).
:- endif.


qsave_lm:-  is_startup_file(X),!,atom_concat(X,'.o',F),!,qsave_lm(F).
qsave_lm:- qsave_lm(qsaved_lm),!.
qsave_lm(LM):- \+ access_file(LM,write),!,debug(logicmoo,'~N% NO FILE WRITE ~p~n',[qsave_lm(LM)]).
qsave_lm(_):- predicate_property(kb7166:assertion_content(_,_,_),number_of_clauses(N)),N>0,!.
qsave_lm(LM):-qsave_lm0(LM),!.
qsave_lm0(LM):- statistics(globallimit,G),statistics(locallimit,L),statistics(traillimit,T),
  X = qsave_program(LM,[toplevel(logicmoo_toplevel),
   goal(logicmoo_goal),op(save),
       stand_alone(false),
       class(development),
       autoload(false),
       % foreign(no_save),
       global(G),trail(T),local(L)]),
   dmsg(X),
   call(X).


run_prologmud :- ensure_loaded(library(prologmud_sample_games/run_mud_server)),init_why(run_prologmud).
init_logicmoo :- ensure_loaded(library(logicmoo_repl)),init_why(init_logicmoo).


% invert_varname(NV):-  ignore(((NV=(N=V), V='$VAR'(N)))).

add_history(O):- 
   ignore_not_not((make_historial(O,A),add_history0(A))),!.

ignore_not_not(G):- ignore((catch((( \+ \+ (ignore(once(G))))),_,fail))),!.

make_historial(_:O,A):-!,make_historial(O,A).
make_historial(whenever(_,O),A):-!,make_historial(O,A).
make_historial(add_history(O),A):-!,make_historial(O,A).
make_historial(O,A):-ground(O),format(atom(A), '~W', [O, [fullstop(true),portrayed(true),quoted(true),numbervars(true)]]).
make_historial(O,A):-
    prolog_load_context(variable_names, Bindings),
    format(atom(A), '~W', [O, [fullstop(true),portray(true),quoted(true),variable_names(Bindings)]]).

add_history0(_):- \+ app_argv('--history'),!.
add_history0(A):- prolog:history(user_input,add(A)).


system:nb_linkval_current(N,V):-duplicate_term(V,VV),V=VV,nb_linkval(N,VV),nb_current(N,V).

extend_varnames(ExpandedBindings):- 
    prolog_load_context(variable_names,Vs),
    append(Vs,ExpandedBindings,NewVs),    
    append(NewVs,[],NewVs),
    nb_linkval_current('$variable_names',NewVs).

:- user:dynamic(expand_query/4).
:- user:multifile(expand_query/4).


:- user:multifile(expand_answer/2).
:- user:dynamic(expand_answer/2).

user:expand_answer(Bindings, ExpandedBindings):- 
    nb_linkval_current('$expand_answer',Bindings),
    toplevel_variables:expand_answer(Bindings, ExpandedBindings),
    nb_linkval_current('$expand_answer',ExpandedBindings).


user:expand_query(Goal, _Expanded, Bindings, _ExpandedBindings):-        fail,
   ignore_not_not((once(( nb_linkval_current('$expand_query',Goal-Bindings),
    append(Bindings,[],Bindings),
    % ignore_not_not(nortrace),ignore_not_not(notrace),
    format(atom(A), '~W', [Goal, [fullstop(true),portray(true),quoted(true),variable_names(Bindings)]]),
    add_history0(A))))),
   fail.
       

:- fixup_exports.
:- system:ensure_loaded(logicmoo_util_startup).


