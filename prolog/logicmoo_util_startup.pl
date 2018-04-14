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
    SCM:           https://github.com/TeamSPoon/logicmoo_utils/blob/master/prolog/logicmoo_util_startup.pl
    Copyleft:      1999-2015, LogicMOO Prolog Extensions
    License:       Lesser GNU Public License
% ===================================================================
*/

% We save the name of the module loading this module
:- module(logicmoo_util_startup,[
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
app_argv(Atom):- app_argv1(Atom),!.
app_argv(Atom):- atom_concat(Pos,'=yes',Atom),!,app_argv1(Pos).
app_argv(Atom):- app_argv_off(Atom),!,fail.
app_argv(Atom):- app_argv1('--all'), atom_concat('--',_,Atom), \+ atom_concat('--no',_,Atom),!.

app_argv_ok(Atom):- app_argv1(Atom),!.
app_argv_ok(Atom):- \+ app_argv_off(Atom).

app_argv_off(Atom):- atom_concat('--',Pos,Atom), atom_concat('--no',Pos,Neg),app_argv1(Neg),!,fail.

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
   forall(lmconf:at_restore_goal(Goal),try_pending_init(How,Goal)).

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
:- forall(absolute_startup_script(F),assertz(system:'$init_goal'(F,logicmoo_util_startup:init_why(after(F)),F:9999))).

:- if(false).
:- user:multifile(expand_answer/2).
:- user:dynamic(expand_answer/2).
user:expand_answer(_Bindings, _ExpandedBindings):- run_pending_inits,fail.
:- user:dynamic(expand_query/4).
:- user:multifile(expand_query/4).
user:expand_query(_Goal, _Expanded, _Bindings, _ExpandedBindings):-  run_pending_inits,fail.
:- endif.

%:- use_module(logicmoo_util_common).
:- fixup_exports.

:- if( app_argv1('--upgrade') ).
:- whenever_flag_permits(run_network,pack_upgrade).
:- endif.

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


