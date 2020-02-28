/* Part of LogicMOO Base Logicmoo Utils
% ===================================================================
    File:         'logicmoo_util_startuo.pl'
    Purpose:       To load the logicmoo libraries inits as needed
    Contact:       $Author: dmiles $@users.sourceforge.net ;
    Version:       'logicmoo_util_startuo.pl' 1.0.0
    Revision:      $Revision: 1.2 $
    Revised At:    $Date: 2017/06/02 21:57:28 $
    Author:        Douglas R. Miles
    Maintainers:   TeamSPoon
    E-mail:        logicmoo@gmail.com
    WWW:           http://www.prologmoo.com
    SCM:           https://github.com/TeamSPoon/logicmoo_utils/blob/master/prolog/logicmoo_startup.pl
    Copyleft:      1999-2015, LogicMOO Prolog Extensions
    License:       Lesser GNU Public License
% ===================================================================
*/

% We save the name of the module loading this module
:- module(logicmoo_startup,[
          maybe_notrace/1,
          absolute_startup_script/1,
          at_init/1,
          during_init/1,
          has_ran_once/1,
          app_argv/1,
          app_argv1/1,
          app_argv_ok/1,
          app_argv_off/1,
          is_startup_script/1,
          init_why/1,
          run_pending_inits/0]).


%:- use_module(library(logicmoo_utils_all)).
:- create_prolog_flag(dmsg_level,filter,[type(term),keep(true)]).

%=======================================
% Utils
%=======================================


%= 	 	 

%% is_startup_script is semidet.
%
% If Startup Script.
%
is_startup_script:- prolog_load_context(source, File),is_startup_script(File).


:-export(is_startup_script/1).

is_startup_script(Name):- var(Name),!,absolute_startup_script(Path),directory_file_path(_,Name,Path).
is_startup_script(Name):- absolute_file_name(Name,File,[file_type(prolog),access(read),file_errors(fail)]),Name\==File,!,is_startup_script(File).
is_startup_script(Name):- exists_source(Name),absolute_startup_script(Path),same_file(Name,Path),!.
is_startup_script(Name):- absolute_startup_script(Path),directory_file_path(_,Named,Path),atom_concat(Name,_,Named),!.

absolute_startup_script(AFile):- short_startup_script(File),
   absolute_file_name(File,AFile,[file_type(prolog),access(read),file_errors(fail)]).

:-export(short_startup_script/1).

script_type('-f').
script_type('-l').
script_type('-s').
script_type(A):-atom(A), \+ atom_concat('-',_,A).

short_startup_script(File):- current_prolog_flag(associated_file,File).
short_startup_script(File):- sub_argv(Type,File),exists_source(File),script_type(Type).

sub_argv(X,Y):-app_argv(List),
  (append(ListL,[--|_],List) -> 
    append(_,[X,Y|_],ListL) ;
    append(_,[X,Y|_],List)).


:- dynamic(lmconf:saved_app_argv/1).
app_argv(Atom):- \+ atom(Atom),!,app_argv_l(Atom).
app_argv(Atom):- app_argv_off(Atom),!,fail.
app_argv(Atom):- app_argv1(Atom),!.
app_argv(Atom):- atom_concat(Pos,'=yes',Atom),!,app_argv1(Pos).
app_argv(Atom):- app_argv1('--all'), atom_concat('--',_Stem,Atom), \+ atom_concat('--no',_Stem2,Atom),!.

app_argv_ok(Atom):- app_argv1(Atom),!.
app_argv_ok(Atom):- \+ app_argv_off(Atom).

app_argv_off(Atom):- atom_concat('--',Pos,Atom), atom_concat('--no',Pos,Neg),app_argv1(Neg),!.
app_argv_off(Pos):- atom_concat('--no',Pos,Neg),app_argv1(Neg),!.

app_argv1(Atom):- app_argv_l(List),member(Atom,List).
app_argv1(Atom):- lmconf:saved_app_argv(Atom),\+ is_list(Atom).

app_argv_l(List):- lmconf:saved_app_argv(List).
app_argv_l(List):- current_prolog_flag(argv,List),List\==[].
app_argv_l(List):- current_prolog_flag(os_argv,List).

shell_format(Fmt,Args):-format(string(S),Fmt,Args),shell(S),!.
start_tty_redirect(PORT):-
  PORT100 is PORT + 100,  
  shell_format('lsof -t -i:~w | xargs --no-run-if-empty kill -9',[PORT100]),
  % shell_format('nohup node app.js -p ~w -c rlwrap -a -A -r -c -N -r --file=completion_~w --history-filename=history_~w -s 1000 telnet localhost ~w &',[PORT100,PORT,PORT,PORT]),
  shell_format('nohup ttyd -r 100 -p ~w rlwrap -a -A -r -c -N -r --file=completion_~w --history-filename=history_~w -s 1000 telnet localhost ~w &',[PORT100,PORT,PORT,PORT]),
  !.
  

erase_clause(H,B):- 
  BH=B+H,BHC=BC+HC,
  copy_term(BH,BHC),
  clause(HC,BC,Ref),
  BH=@=BHC,
  erase(Ref).   

:- meta_predicate(maybe_notrace(0)).

%% maybe_notrace(:Goal) is nondet.
%
% When not tracing, try to run Goal.
%   if Goal has a problem (like fails) 
%         run inside rtrace/1 (the non-interactive debugger).
% If tracing, try to run Goal inside of quietly(Goal)
%   if Goal has a problem (like fails) 
%         trace interactively.
%
% @NOTE quietly/1 is the nondet version of notrace/1.

% maybe_notrace(Goal):- !, call(Goal).
maybe_notrace(Goal):- tracing -> (debug,maybe_notrace(quietly(Goal), Goal)) ; maybe_notrace(Goal,rtrace(Goal)).

:- meta_predicate(maybe_notrace(0,0)).

maybe_notrace(Goal,Else):- !, (call(Goal)*-> true ; Else).
maybe_notrace(Goal,Else):-   
  (catch(Goal,E1,(wdmsg(error_maybe_zotrace(E1,Goal)),Else)) 
   -> ! 
   ; (( wdmsg(failed_maybe_zotrace(Goal)),
     ignore(catch(Else,E2,(wdmsg(else_error_maybe_zotrace(E2, Else, goal(Goal))),(nonvar(E1)->throw(E1);throw(E2)))))))).


%=======================================
% DURING/AFTER BOOT HOOKS
%=======================================

:- multifile(lmconf:at_restore_goal/1).
:- dynamic(lmconf:at_restore_goal/1).


%% at_init(:Goal) is semidet.
% 
% Run a +Goal just before entering/returning to prolog/0 or main goal.
%
%  much like to initialization(Goal,[restore]).  but *also* happens in non-compiled programs
%
%  
%  swipl -l some_startup_file   - run before the banner would be displayed 
%
%  ./some_qaved_program         - run at 'restore' time
%
%  ?-  use_module(has_several_hookers). -  run all just before returning to toplevel
%
%  swipl -s some_startup_file   - run immediately unless is module
% 
:- meta_predicate(at_init(:)).
at_init(Goal):- system:assertz(lmconf:at_restore_goal(Goal)),add_history(Goal).


%% during_init(:Goal) is semidet.
% 
% Run a +Goal as soon as possible
%
%  much like initialization(Goal,[now]).  but *also* happens in compiled systems
%
%  
%  swipl -l some_startup_file   - like initialization(Goal,[now])
%
%  ./some_qaved_program         - like initialization(Goal,[restore])
%
%  ?-  use_module(has_several_hookers). -  like initialization(Goal,[now])
%
%  swipl -s some_startup_file   - like initialization(Goal,[now])
%
:- meta_predicate(during_init(:)).
during_init(Goal):- ignore(try_pending_init(maybe_notrace,Goal)), at_init(Goal).


during_boot(G):- during_init(G).
after_boot(G):- at_init(G).

% doesnt run if --nonet
:- meta_predicate(during_net_boot(:)).
during_net_boot(M:Goal):- during_boot(whenever_flag_permits(run_network,M:Goal)).

% --nonet
:- meta_predicate(after_net_boot(:)).
after_net_boot(M:Goal):- after_boot(whenever_flag_permits(run_network,M:Goal)).

:- meta_predicate(after_boot_sanity_test(:)).
after_boot_sanity_test(M:Goal):- after_boot(M:sanity(M:Goal)).

%% call_last_is_var( :GoalMCall) is semidet.
%
% Call Last If Is A Variable.
%
:- meta_predicate(call_last_is_var(0)).
call_last_is_var(MCall):- strip_module(MCall,M,Call),
   must((compound(Call),functor(Call,_,A))),
   arg(A,Call,Last),nonvar(Last),Call=..FArgs,
   append(Left,[Last],FArgs),
   append(Left,[IsVar],NFArgs),NewCall=..NFArgs,!,
    ( M:NewCall*->IsVar=Last;fail).


:- meta_predicate(if_script(:)).   	 

%% if_script( :Goal) is semidet.
%
% If this is a Startup Script call Goal
%
if_script(Call):- is_startup_script->Call;dmsg(\+ is_startup_script(Call)).

is_startup_file(File):- is_startup_script(File).

%=======================================
%= CALL BOOT HOOKS
%=======================================
:- dynamic(lmcache:called_startup_goal/1).
:- volatile(lmcache:called_startup_goal/1).

:- meta_predicate run_pending_inits(1).
run_pending_inits(How):- 
   forall(lmconf:at_restore_goal(Goal),(try_pending_init(How,Goal))).

has_ran_once(Goal):- lmcache:called_startup_goal(GoalW), GoalW =@= Goal,!.


:- meta_predicate try_pending_init(1,*).
try_pending_init(_,Goal):- has_ran_once(Goal),!.
try_pending_init(How,Goal):- assert(lmcache:called_startup_goal(Goal)),
    ( \+ \+ call(How,Goal) 
      -> true ; 
       erase_clause(lmcache:called_startup_goal(Goal),true)).


:- module_transparent(run_pending_inits/0).
run_pending_inits:- run_pending_inits(maybe_notrace).

init_why(Phase):- 
  %dmsg("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"),
  %dmsg("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"),
  dmsg(init_why(Phase)),
  %dmsg("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"),
  %dmsg("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"),!,
  run_pending_inits.

:- if(app_argv('--nonet')).
:- set_prolog_flag(run_network,false).
:- endif.


%=======================================
%= REGISTER FOR INIT EVENTS
%=======================================

%= Register a hook after restore
:- initialization(init_why(restore),restore).

%= Register a hook after restore
:- initialization(init_why(program),program).


%= Register a hook after welcome
:- multifile prolog:message//1.
% prolog:message(welcome) -->  {init_why(welcome),fail}.

%= Register a hook after our so-called startup file (Should be last file in list)
:- multifile(system:'$init_goal'/3).
:- dynamic(system:'$init_goal'/3).
:- module_transparent(system:'$init_goal'/3).
:- forall(absolute_startup_script(F),
    (assertz(system:'$init_goal'(F,logicmoo_startup:init_why(after(F)),F:9999)))).

:- if(false).
:- user:multifile(expand_answer/2).
:- user:dynamic(expand_answer/2).
user:expand_answer(_Bindings, _ExpandedBindings):- run_pending_inits,fail.
:- user:dynamic(expand_query/4).
:- user:multifile(expand_query/4).
user:expand_query(_Goal, _Expanded, _Bindings, _ExpandedBindings):-  run_pending_inits,fail.
:- endif.

%:- use_module(logicmoo_utils_all).
%:- fixup_exports.

:- if( app_argv1('--upgrade') ).
:- whenever_flag_permits(run_network,pack_upgrade).
:- endif.


%:- use_module(library(logicmoo/each_call)).
%:- use_module(library(logicmoo_startup)).

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
   %set_prolog_flag(gc,false),
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
  call(call,
   with_input_from_predicate(({}/[X]>>(repeat,X='YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY')))),
    forall(call(call,prolog_pack:current_pack,Pack),maybe_pack_upgrade(Pack)))).

maybe_pack_upgrade(Pack):- pack_property(Pack, directory(PackDir)),\+ access_file(PackDir,write),!.
maybe_pack_upgrade(Pack):- pack_upgrade(Pack).



normally(G):- locally(set_prolog_flag(runtime_debug,0),locally(set_prolog_flag(bugger,false),G)).


shared_vars(Left,Right,SVG):-quietly(( term_variables(Left,Vs1),term_variables(Right,Vs2),intersect_eq0(Vs2,Vs1,SVG))).


  member_eq0(X, [Y|Ys]) :- X==Y;member_eq0(X,Ys).

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

%:- set_prolog_stack_gb(16).


if_debugging(Topic,Goal):- debugging(Topic)->call(Goal);true.

%% all_source_file_predicates_are_exported() is det.
%
% All Module Predicates Are Exported.

:- module_transparent(all_source_file_predicates_are_exported/0).
all_source_file_predicates_are_exported:- current_prolog_flag(xref,true),!.
all_source_file_predicates_are_exported:-
 source_location(S,_), prolog_load_context(module,LC),
 all_source_file_predicates_are_exported(S,LC),!.
all_source_file_predicates_are_exported:-
 prolog_load_context(module,LC),'$current_typein_module'(TIM),
 forall((LC\==user,module_property(LC,file(S))),all_source_file_predicates_are_exported(S,LC)),
 forall((TIM\==LC,TIM\==user,module_property(TIM,file(S))),all_source_file_predicates_are_exported(S,TIM)).



lmconfig:never_export_named(_,attr_unify_hook,2).
lmconfig:never_export_named(_,attribute_goals,3).
lmconfig:never_export_named(_,project_attributes,2).
lmconfig:never_export_named(_,attr_portray_hook,2).
lmconfig:never_export_named(_,F,_):- atom_concat('$',_,F) ; atom_concat('__aux',_,F).

lmconfig:never_reexport_named(_,goal_expansion,_).
lmconfig:never_reexport_named(_,term_expansion,_).

% lmconfig:never_export_named(_M,F,A):- current_predicate(user:F/A).

% :- module_transparent(all_source_file_predicates_are_exported/2).
maybe_export([],_,_,_):-!.
maybe_export([LC|LCT],M,F,A):- maybe_export(LC,M,F,A),!,maybe_export(LCT,M,F,A).
maybe_export(false,_,_,_):-!.
maybe_export(true, M,F,A):- !, maybe_export(system, M,F,A).
maybe_export(_, M,F,A):- lmconfig:never_export_named(M,F,A). 
maybe_export(LC,M,_,_):- \+ (atom(LC); \+ atom(M)), !.
maybe_export(LC,M,F,A):- LC==M, !, M:export(M:F/A).
maybe_export(LC,_,F,A):- current_predicate(LC:F/A),!.
maybe_export(LC,M,F,A):- 
   LC:import(M:F/A),
   (lmconfig:never_reexport_named(LC,F,A)-> true ; LC:export(M:F/A)).

:- set_prolog_flag(logicmoo_import_to_system, baseKB).

all_source_file_predicates_are_exported(S,LC)
 :- 
 (ignore(source_location(S,_);prolog_load_context(source,S))),
  ignore(prolog_load_context(module,LC)),
 
 forall(source_file(M:H,S),
 ignore((functor(H,F,A),  
   \+ atom_concat(_,'__aux_',F),
  %(module_property(M,exports(List))-> \+ member(F/A,List); true),
  % M:public(M:F/A),
  notrace(catch(maybe_export(M,M,F,A),_,fail)),
  maybe_export(LC,M,F,A),
  (current_prolog_flag(logicmoo_import_to_system, BaseKB)-> maybe_export(BaseKB,M,F,A) ; true),
  maybe_export(system,M,F,A)))).

:- export(con_x_fail/1).
:- meta_predicate(con_x_fail(:)).
con_x_fail((G1,G2)):-!, con_x_fail(G1),con_x_fail(G2).
con_x_fail(M:(G1,G2)):-!, con_x_fail(M:G1),con_x_fail(M:G2).
con_x_fail(G):-catch(G,_,fail).

:- meta_predicate(sexport(:)).
sexport(M:F/A):- M:export(M:F/A),system:import(M:F/A).
                       
%% all_source_file_predicates_are_transparent() is det.
%
% All Module Predicates Are Transparent.
:- module_transparent(all_source_file_predicates_are_transparent/0).
all_source_file_predicates_are_transparent:- current_prolog_flag(xref,true),!.
all_source_file_predicates_are_transparent:-
 source_location(S,_), prolog_load_context(module,LC),
 all_source_file_predicates_are_transparent(S,LC),!.
all_source_file_predicates_are_transparent:-
 prolog_load_context(module,LC),'$current_typein_module'(TIM),
 forall((LC\==user,module_property(LC,file(S))),all_source_file_predicates_are_transparent(S,LC)),
 forall((TIM\==LC,TIM\==user,module_property(TIM,file(S))),all_source_file_predicates_are_transparent(S,TIM)).

:- module_transparent(all_source_file_predicates_are_transparent/2).
all_source_file_predicates_are_transparent(S,_LC):- 
 forall(source_file(M:H,S),
 (functor(H,F,A),
  ignore(((\+ predicate_property(M:H,transparent), module_transparent(M:F/A), 
  \+ atom_concat('__aux',_,F),debug(modules,'~N:- module_transparent((~q)/~q).~n',[F,A])))))).



:- export(fixup_exports/0).

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

:- module_transparent(fixup_exports/0).

fixup_exports:-    
   all_source_file_predicates_are_exported,
   all_source_file_predicates_are_transparent.

fixup_exports_system:-   (prolog_load_context(source,SF)-> system:reexport(SF) ; true).


:- fixup_exports.


%:- logicmoo_startup:use_module(library(option),[options/3]).

logicmoo_base_port(Base):- app_argv1(One),\+ is_list(One),
   (atom(One)-> (atomic_list_concat([_,Atom],'port=',One),atom_number(Atom,Base))),!.
logicmoo_base_port(Base):- getenv_or('LOGICMOO_BASE_PORT',Base,3000),!.
:- export(logicmoo_base_port/1).
:- system:import(logicmoo_base_port/1).

% ==============================================
% Easier to trace while access_level system
% ==============================================
:- '$hide'('$toplevel':restore_debug).
:- '$hide'('$toplevel':save_debug).
:- '$hide'('$toplevel':residue_vars/2).
:- '$hide'('system':deterministic/1).
:- '$hide'(toplevel_call/2).
:- '$hide'('$toplevel':'$query_loop'/0).

% ==============================================
% System metapredicates
% ==============================================
:- meta_predicate '$syspreds':bit(2,?,?).
:- meta_predicate '$bags':findnsols_loop(*,*,0,*,*).
:- meta_predicate '$bags':findall_loop(*,0,*,*).
:- meta_predicate '$attvar':unfreeze(0).
:- meta_predicate '$attvar':run_crv(0,*,*,*).
:- meta_predicate '$expand':expand_term_list(4,*,*,*,*).
:- meta_predicate '$parms':cached_library_directory(*,0,*).
:- meta_predicate '$toplevel':residue_vars(0,-).
:- meta_predicate '$toplevel':toplevel_call(0).
:- meta_predicate '$toplevel':run_initialize(0,*).
% :- meta_predicate '$toplevel':run_init_goal(0,*).
% :- meta_predicate '$attvar':uhook(*,0,*,*).
% :- meta_predicate '$attvar':uhook(*,0,*).
%:- meta_predicate '$toplevel':'$execute_goal2'(0,*).








%:- use_module(library(logicmoo/each_call)).

%:- use_module(library(debuggery/dmsg)).
%:- use_module(library(must_sanity)).

% ( GFE = Girl-Friend Experience )


:- fixup_exports.


:- if(false). 
:- multifile(user:term_expansion/2).
:- dynamic(user:term_expansion/2).

% basically only run if is in 'user'
user:term_expansion(EOF,_):- EOF == end_of_file, prolog_load_context(source,File),prolog_load_context(file,File),
  prolog_load_context(module,SourceModule), '$current_typein_module'(TypeIn),
  wdmsg(File : '?='(SourceModule , TypeIn)),
  SourceModule == TypeIn,
  run_pending_inits, fail.
:- endif.


