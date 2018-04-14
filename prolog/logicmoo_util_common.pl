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
   whenever_flag_permits/2,
   qsave_lm/0,qsave_lm/1,ignore_not_not/1, fixup_exports/0,
          all_source_file_predicates_are_transparent/0,
          all_source_file_predicates_are_transparent/2,
          all_source_file_predicates_are_exported/0,
          all_source_file_predicates_are_exported/2,
          add_file_search_path_safe/2,
          set_prolog_stack_gb/1,
          load_library_system/1,
          if_file_exists/1,
          add_file_search_path_safe/2,
          pack_upgrade/0,run_prologmud/0,init_logicmoo/0,
          shared_vars/3
          ]).



:- meta_predicate if_debugging(*,0).


fav_module:-
  '$current_typein_module'(Module),prolog_load_context(module,SourceModule),
  ((SourceModule==Module) -> true ;
  ((SourceModule==user -> '$set_source_module'(Module) ; true),
  (Module==user -> '$set_typein_module'(SourceModule) ; true))).



fav_debug9:- 
  fav_module,
  fav_debug,
   set_prolog_flag(access_level,system),
   set_prolog_flag(write_attributes,ignore),
   set_prolog_flag(fileerrors,true),
   set_prolog_flag(gc,false),
   %set_prolog_flag(occurs_check,true),
 % set_prolog_flag(retry_undefined, none),
   !.

fav_debug:- 
 set_prolog_flag(backtrace, true),
 set_prolog_flag(backtrace_goal_depth, 2000),
 set_prolog_flag(backtrace_show_lines, true),
 set_prolog_flag(debug,true),
 set_prolog_flag(debug_on_error,true),
 set_prolog_flag(debugger_show_context,true),
 set_prolog_flag(debugger_write_options,[quoted(true), portray(true), max_depth(10), attributes(write)]),
 set_prolog_flag(report_error,true),
 set_prolog_flag(runtime_debug, 3), % 2 = important but dont sacrifice other features for it
 set_prolog_flag(runtime_safety, 3),  % 3 = very important
 set_prolog_flag(runtime_speed, 0), % 1 = default
 set_prolog_flag(runtime_speed, 1), % 0 = dont care
 set_prolog_flag(unsafe_speedups, false),
 set_prolog_flag(verbose_autoload,true),
 set_prolog_flag(verbose_load,full),
 !.

setup_hist:-  '$toplevel':setup_history.
:- setup_hist.


bt:-
    use_module(library(prolog_stack)),
  dumpST9,
 prolog_stack:export(prolog_stack:get_prolog_backtrace_lc/3),
 use_module(library(prolog_stack),[print_prolog_backtrace/2,get_prolog_backtrace_lc/3]),
  stream_property(S,file_no(1)),
  prolog_stack:call(call,get_prolog_backtrace_lc,8000, Stack, [goal_depth(600)]),
  print_prolog_backtrace(S, Stack).



:- meta_predicate(whenever_flag_permits(+,:)).

whenever_flag_permits(Flag,G):- (current_prolog_flag(Flag,false) -> true ; G).
whenever(Flag,G):- whenever_flag_permits(Flag,G).


% startup_file0(File):- sub_argv(['-g',Opt]),atom_to_term(Opt,LoadsFile,_),is_loads_file(LoadsFile,File).
is_loads_file(ensure_loaded(SFile),File):- strip_module(File,_,SFile).
is_loads_file([File],SFile):- strip_module(File,_,SFile).
is_loads_file(consult(SFile),File):- strip_module(File,_,SFile).
is_loads_file(use_module(SFile),File):- strip_module(File,_,SFile).
is_loads_file(_:SFile,File):-!,is_loads_file(SFile,File).

%=======================================
% Load only if exists
%=======================================


%% if_file_exists( ?M) is semidet.
%
% If File Exists.
%
:- meta_predicate(if_file_exists(:)).
if_file_exists(M:Call):- arg(1,Call,MMFile),strip_module(MMFile,_,File),
 (exists_source(File)-> must(M:Call);wdmsg(not_installing(M,Call))),!.



% sets up and restore the subsystems

:- module_transparent(load_library_system/1).
load_library_system(M:File):- !, load_library_system(M,File). 
load_library_system(File):- context_module(M),load_library_system(M,File).
:- system:import(load_library_system/1).

:- module_transparent(load_library_system/2).
load_library_system(user,File):-!, during_boot(gripe_time(40,(if_file_exists(ensure_loaded(system:File))))).
load_library_system(M,File):- during_boot(gripe_time(40,(if_file_exists(ensure_loaded(M:File))))).

:- system:import(load_library_system/2).


:- meta_predicate iff_defined(*).
:- meta_predicate iff_defined(:,0).
:- module_transparent((iff_defined/1,iff_defined/2)).

%% iff_defined( ?G) is semidet.
%
% If Defined.
%
iff_defined(Goal):- iff_defined(Goal,((print_message(warning,warn_undefined(Goal))),!,fail)).

%% iff_defined( ?Goal, :GoalElse) is semidet.
%
% If Defined Else.
%
iff_defined(Goal,Else):- current_predicate(_,Goal)*->Goal;Else.
% iff_defined(M:Goal,Else):- !, current_predicate(_,OM:Goal),!,OM:Goal;Else.
%iff_defined(Goal,  Else):- current_predicate(_,OM:Goal)->OM:Goal;Else.



:- module_transparent((add_history/1,qsave_lm/1,ignore_not_not/1,load_library_system/1,
          all_source_file_predicates_are_transparent/0,
          all_source_file_predicates_are_transparent/2,
          all_source_file_predicates_are_exported/0,
          all_source_file_predicates_are_exported/2)).

:- meta_predicate(ignore_not_not(0)).
:- export(pack_upgrade/0).
pack_upgrade:- call((user:use_module(library(prolog_pack)),use_module(library(predicate_streams)), 
  with_input_from_predicate(({}/[X]>>(repeat,X='YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY')),
    forall(call(call,prolog_pack:current_pack,Pack),maybe_pack_upgrade(Pack))))).

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

:- set_prolog_stack_gb(16).


if_debugging(Topic,Goal):- debugging(Topic)->call(Goal);true.

%% all_source_file_predicates_are_exported() is det.
%
% All Module Predicates Are Exported.

:- module_transparent(all_source_file_predicates_are_exported/0).
all_source_file_predicates_are_exported:- current_prolog_flag(xref,true),!.
all_source_file_predicates_are_exported:-
 source_location(S,_), prolog_load_context(module,LC),
 all_source_file_predicates_are_exported(S,LC).

lmconfig:never_export_named(_,attr_unify_hook,2).
lmconfig:never_export_named(_,attribute_goals,3).
lmconfig:never_export_named(_,project_attributes,2).
lmconfig:never_export_named(_,attr_portray_hook,2).
% lmconfig:never_export_named(_M,F,A):- current_predicate(user:F/A).

% :- module_transparent(all_source_file_predicates_are_exported/2).

%:- set_prolog_flag(logicmoo_import_to_system, baseKB).
all_source_file_predicates_are_exported(S,LC)
 :- 
 ignore(source_location(S,_);prolog_load_context(source,S)),
 ignore(prolog_load_context(module,LC)),
 (current_prolog_flag(logicmoo_import_to_system, BaseKB)->true;BaseKB=[]),
 forall(source_file(M:H,S),
 ignore((functor(H,F,A), 
  %(module_property(M,exports(List))-> \+ member(F/A,List); true),
  \+ lmconfig:never_export_named(M,F,A),
  ignore((atom(LC),atom(M),LC\==M, M:multifile(M:F/A),fail,LC:export(M:F/A),ignore(atom_concat('$',_,F)),LC:import(M:F/A))),
  ignore(((\+ atom_concat('$',_,F),\+ atom_concat('__aux',_,F),LC:export(M:F/A), 
  ignore((M\==BaseKB, \+ current_predicate(system:F/A), system:import(M:F/A))))))))).

:- meta_predicate(sexport(:)).
sexport(M:F/A):- M:export(M:F/A),system:import(M:F/A).
                       
%% all_source_file_predicates_are_transparent() is det.
%
% All Module Predicates Are Transparent.
:- module_transparent(all_source_file_predicates_are_transparent/0).
all_source_file_predicates_are_transparent:- current_prolog_flag(xref,true),!.
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



% :- export(fixup_exports/0).

var_non_attvar(V):- var(V),\+ attvar(V).


getenv_or(Name,ValueO,Default):-
   (getenv(Name,RV)->Value=RV;Value=Default),
   (number(Default) -> ( \+ number(Value) -> atom_number(Value,ValueO); Value=ValueO);
      Value=ValueO).


/*
:- dynamic(baseKB:logicmoo_utils_separate/0).
:- retractall(baseKB:logicmoo_utils_separate).
:- set_prolog_flag(generate_debug_info, true).


% :- abox:defaultTBoxMt(_)->true;('$current_typein_module'(M),asserta(abox:defaultTBoxMt(M))).


:- dynamic(baseKB:mpred_is_impl_file/1).
:- multifile(baseKB:mpred_is_impl_file/1).
% :- volatile(baseKB:mpred_is_impl_file/1).


*/



qsave_lm:-!.
qsave_lm:-  is_startup_script(X),!,atom_concat(X,'.o',F),!,qsave_lm(F).
qsave_lm:- qsave_lm(qsaved_lm),!.
qsave_lm(_):- !.
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
make_historial(whenever_flag_permits(_,O),A):-!,make_historial(O,A).
make_historial(add_history(O),A):-!,make_historial(O,A).
make_historial(O,A):-ground(O),format(atom(A), '~W', [O, [fullstop(true),portrayed(true),quoted(true),numbervars(true)]]).
make_historial(O,A):-
    prolog_load_context(variable_names, Bindings),
    format(atom(A), '~W', [O, [fullstop(true),portray(true),quoted(true),variable_names(Bindings)]]).

:- multifile prolog:history/2.

add_history0(_):- \+ app_argv('--history'),!.
add_history0(A):- prolog:history(user_input,add(A)),
                  prolog:history(current_input,add(A)).


nb_linkval_current(N,V):-duplicate_term(V,VV),V=VV,nb_linkval(N,VV),nb_current(N,V).

extend_varnames(ExpandedBindings):- 
    prolog_load_context(variable_names,Vs),
    append(Vs,ExpandedBindings,NewVs),    
    append(NewVs,[],NewVs),
    nb_linkval_current('$variable_names',NewVs).

:- if(false).
:- user:multifile(expand_answer/2).
:- user:dynamic(expand_answer/2).
user:expand_answer(Bindings, ExpandedBindings):- 
    nb_linkval_current('$expand_answer',Bindings),
    toplevel_variables:expand_answer(Bindings, ExpandedBindings),
    nb_linkval_current('$expand_answer',ExpandedBindings).


:- user:dynamic(expand_query/4).
:- user:multifile(expand_query/4).
user:expand_query(Goal, _Expanded, Bindings, _ExpandedBindings):-        fail,
   ignore_not_not((once(( nb_linkval_current('$expand_query',Goal-Bindings),
    append(Bindings,[],Bindings),
    % ignore_not_not(nortrace),ignore_not_not(notrace),
    format(atom(A), '~W', [Goal, [fullstop(true),portray(true),quoted(true),variable_names(Bindings)]]),
    add_history0(A))))),
   fail.
:- endif.


:- fixup_exports.

% :- use_module(logicmoo_util_startup).

