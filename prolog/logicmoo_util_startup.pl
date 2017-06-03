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
          startup_file/1,
          at_init/1,
          during_init/1,
          has_ran_once/1,
          app_argv/1,
          is_startup_file/1,
          init_why/1,
          if_file_exists/1,
          run_pending_inits/0]).

:- create_prolog_flag(dmsg_level,filter,[type(term),keep(true)]).

%=======================================
% Utils
%=======================================

:-export(is_startup_file/1).

is_startup_file(Name):- var(Name),!,startup_file(Path),directory_file_path(_,Name,Path).
is_startup_file(Name):- absolute_file_name(Name,File,[file_type(prolog),access(read),file_errors(fail)]),Name\==File,!,is_startup_file(File).
is_startup_file(Name):- exists_source(Name),startup_file(Path),same_file(Name,Path),!.
is_startup_file(Name):- startup_file(Path),directory_file_path(_,Named,Path),atom_concat(Name,_,Named),!.

startup_file(AFile):- startup_file0(File),absolute_file_name(File,AFile,[file_type(prolog),access(read),file_errors(fail)]).
:-export(startup_file0/1).
startup_file0(File):- sub_argv(['-f',File]).
startup_file0(File):- sub_argv(['-l',File]).
startup_file0(File):- sub_argv(['-g',Opt]),atom_to_term(Opt,LoadsFile,_),is_loads_file(LoadsFile,File).
startup_file0(File):- sub_argv(['-x',File]).
startup_file0(File):- sub_argv(['-o',File]).
startup_file0(File):- current_prolog_flag(os_argv,[_|ListR]),reverse(List,ListR),member(File,List).
% throw(is_startup_file(unknown)).

sub_argv([X,Y]):-current_prolog_flag(os_argv,[_|ListR]),reverse(List,ListR),append(_,[Y,X|_],List).

is_loads_file(ensure_loaded(SFile),File):- strip_module(File,_,SFile).
is_loads_file([File],SFile):- strip_module(File,_,SFile).
is_loads_file(consult(SFile),File):- strip_module(File,_,SFile).
is_loads_file(use_module(SFile),File):- strip_module(File,_,SFile).
is_loads_file(_:SFile,File):-!,is_loads_file(SFile,File).

%% if_file_exists( ?M) is semidet.
%
% If File Exists.
%
:- meta_predicate(if_file_exists(:)).
if_file_exists(M:Call):- arg(1,Call,MMFile),strip_module(MMFile,_,File),
 (exists_source(File)-> must(M:Call);wdmsg(not_installing(M,Call))),!.


:- dynamic(lmconf:saved_app_argv/1).
app_argv(List):- lmconf:saved_app_argv(List).
app_argv(List):- current_prolog_flag(os_argv,List).
app_argv(Atom):- atom(Atom),app_argv(List),memberchk(Atom,List).


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

maybe_notrace(Goal):- tracing -> (debug,maybe_notrace(quietly(Goal), Goal)) ; maybe_notrace(Goal,rtrace(Goal)).

:- meta_predicate(maybe_notrace(0,0)).

maybe_notrace(Goal,Else):-   
  (catch(Goal,E1,(wdmsg(error_maybe_notrace(E1,Goal)),Else)) 
   -> ! 
   ; (( wdmsg(failed_maybe_notrace(Goal)),
     ignore(catch(Else,E2,(wdmsg(else_error_maybe_notrace(E2, Else, goal(Goal))),(nonvar(E1)->throw(E1);throw(E2)))))))).


%=======================================
% DURING/AFTER BOOT HOOKS
%=======================================

:- multifile(lmconf:at_restore_goal/1).
:- dynamic(lmconf:at_restore_goal/1).


%% at_init(:Goal) is semidet.
% 
% Run a +Goal just before entering/returning to prolog/0 or main goal.
%
%  much like to initialization(Goal,[restore]).  but *also* happens in non-compiled system
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
at_init(M:Goal):- system:assertz(lmconf:at_restore_goal(M:Goal)),add_history(M:Goal).


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
during_init(M:Goal):- ignore(try_pending_init(maybe_notrace,M:Goal)), at_init(M:Goal).

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

%= Register a hook after welcome
:- multifile prolog:message//1.
prolog:message(welcome) -->  {init_why(welcome),fail}.

%= Register a hook after our so-called startup file (Should be last file in list)
:- multifile(system:'$init_goal'/3).
:- dynamic(system:'$init_goal'/3).
:- module_transparent(system:'$init_goal'/3).
:- (is_startup_file(X),absolute_file_name(X,F)) -> 
   assertz(system:'$init_goal'(F,logicmoo_util_startup:init_why(after(X)),F:9999)) 
   ; true.

:- if(true).
:- user:dynamic(expand_query/4).
:- user:multifile(expand_query/4).
user:expand_answer(_Bindings, _ExpandedBindings):- run_pending_inits,fail.
:- user:multifile(expand_answer/2).
:- user:dynamic(expand_answer/2).
user:expand_query(_Goal, _Expanded, _Bindings, _ExpandedBindings):-  run_pending_inits,fail.
:- endif.

:- use_module(logicmoo_util_common).
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



