/* Part of LogicMOO Base logicmoo_util_bb_env
% Provides a prolog database *env*
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

% File: /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/util/logicmoo_util_structs.pl
:- module(ru,
[	% uses_predicate/3,
         uses_undefined_hook/0,
         install_retry_undefined/2,
         % uses_predicate/5,
         %     retry_undefined/3,
         is_parent_goal/1,
         is_parent_goal/2,
         is_parent_goal/1,
         is_parent_goal/2
]).

:- thread_local(was_prolog_flag/1).
:- current_prolog_flag(retry_undefined,Was)->asserta(was_prolog_flag(retry_undefined,Was));asserta(was_prolog_flag(retry_undefined,none)).
%:- use_module(library(loop_check)).
:- use_module(library(bugger)).
:- use_module(library(hook_database)).

:- create_prolog_flag(retry_undefined, none,[type(term),keep(false)]).

:- module_transparent((	
   uses_predicate/2,
   uses_undefined_hook/0,				
   uses_predicate/5,
   retry_undefined/3,
   is_parent_goal/1,
   install_retry_undefined/2,
   is_parent_goal/2,
   is_parent_goal/1,
   get_retry_undefined_hook/2,
   is_parent_goal/2)).

:- dynamic(ru:retry_undefined_hook/2).

get_retry_undefined_hook(X,Y):- ru:retry_undefined_hook(X,Y).
get_retry_undefined_hook(_M,Was):- current_prolog_flag(retry_undefined, Was), Was\==module.

install_retry_undefined(Module,Setting):- asserta((ru:retry_undefined_hook(Module,Was):-!,Was=Setting)).

:- install_retry_undefined('$toplevel',error).
:- install_retry_undefined('user',error).

uses_undefined_hook.



 :- meta_predicate uses_predicate(*,*,*,*,*,*).
 :- meta_predicate uses_predicate(*,*,*,*,*,*).
 :- meta_predicate uses_predicate(1,*,*,*,*,*).


dumpST_dbreak:- dumpST,break.

% baseKBOnly mark_mark/3 must be findable from every module (dispite the fact that baseKB is not imported)
% :- dynamic baseKB:mpred_prop/4.

% hybrid_support (like spft/3) must be defined directly in every module and then aggregated thru genlMts (thus to baseKB)
/*
is_parent_goal(G):- prolog_current_frame(F),is_parent_goal(F,G).
% The user must ensure the checked parent goal is not removed from the stack due 
% to last-call optimisation 
is_parent_goal(F,G):- nonvar(G),prolog_frame_attribute(F,parent_goal, G).
%and be aware of the slow operation on deeply nested calls.
is_parent_goal(F,G):- prolog_frame_attribute(F,parent,P),parent_frame_goal(P,G).

parent_frame_goal(F,V):- parent_frame_goal_0(F,V0),contains_goalf(V0,V).
parent_frame_goal_0(F,V):- prolog_frame_attribute(F,goal,V);
   (prolog_frame_attribute(F,parent,P),parent_frame_goal_0(P,V)).

contains_goalf(V0,V):- nonvar(V),same_goalf(V0,V),!.
contains_goalf(V0,_):- \+ compound(V0),!,fail.
contains_goalf(V0,V):- var(V),same_goalf(V0,V).
contains_goalf(_:V0,V):- !, contains_goalf(V0,V).
contains_goalf('$execute_directive_3'(V0),V):-!, same_goalf(V0,V).
contains_goalf('<meta-call>'(V0),V):-!, same_goalf(V0,V).
contains_goalf(catch(V0,_,_),V):- same_goalf(V0,V).
contains_goalf(catch(_,_,V0),V):- same_goalf(V0,V).
same_goalf(V,V).
*/

% make sure we ignore calls to predicate_property/2  (or thus '$define_predicate'/1)
uses_predicate(_DEF,_,_,_,_,Error):- 
   prolog_current_frame(F), (is_parent_goal(F,'$define_predicate'(_));
   (is_parent_goal(F,_:'assert_u'(_)));is_parent_goal(F,'$syspreds':property_predicate(_,_))),!,
   error = Error.

uses_predicate(_Was,_CM,_M,_F,_A,Error):- is_parent_goal(check:check),!,Error=error.
uses_predicate(_Was,_CM,_M,_F,_A,Error):- is_parent_goal(check:check),!,Error=error.
uses_predicate(_Was,_CM,_M,_F,_A,Error):- show_success(is_parent_goal('$define_predicate')),!,Error=error.

uses_predicate(_Was,_CM,'$toplevel',_F,_A,Error):- !,Error=error.

uses_predicate(_DEF,_, _, ~, 1, error) :- !.
uses_predicate(_DEF,_,CallerMt,'$pldoc',4,retry):- make_as_dynamic(uses_predicate,CallerMt,'$pldoc',4),!.
uses_predicate(_DEF,User, User, module, 2, error):-!.
uses_predicate(_DEF,_,_, (:-), _, error) :- !, fail. 
uses_predicate(_DEF,_,_, (/), _, error) :- !. 
uses_predicate(_DEF,_,_, ( '//' ), _, error) :- !. 
uses_predicate(_DEF,_,_, F, _, error) :- atom_concat('__',_,F),!.
uses_predicate(_DEF,_,_, F, _, error) :- atom_concat('$',_,F),!.

uses_predicate(_DEF,_,_, (:), _, error) :- !. % ,dumpST_dbreak.
% uses_predicate(_DEF,_,_, '[|]', _, error) :- !,dumpST_dbreak.
% uses_predicate(_DEF,_,_, '>>',  4, error) :- !,dumpST_dbreak.
% uses_predicate(_DEF,_,M, inherit_above,_,retry):- M:use_module(library(virtualize_source)).

% makes sure we ignore calls to predicate_property/2  (or thus '$define_predicate'/1)
% uses_predicate(_DEF,_,M,F,A,R):- prolog_current_frame(FR), functor(P,F,A),(prolog_frame_attribute(FR,parent_goal,predicate_property(M:P,_))),!,R=error.
uses_predicate(_DEF,_,Module,Name,Arity,Action) :-
      current_prolog_flag(autoload, true),
	'$autoload'(Module, Name, Arity), !,
	Action = retry.


uses_predicate(E,_,_,_,_,Error):- E=error, !,Error=error.
uses_predicate(fail,_,_,_,_,_):- !,fail.
uses_predicate(break,_,_,_,_,Error):- !,dumpST_dbreak,Error=error.


uses_predicate(_DEF,_,System, _,_, error):- module_property(System,class(system)),!.
uses_predicate(_DEF,_,System, _,_, error):- module_property(System,class(library)),!.

uses_predicate(Setting,SM,M,F,A,Act):- Setting\==kb_shared, SM\==user, M\==baseKB,
     (dmsg(uses_predicate(Setting,SM,M,F,A,Act))),fail.

uses_predicate(kb_shared,System, M, F,A, retry):-   
   show_failure(uses_undefined_hook(M)),
   create_predicate_inheritance(kb_shared(M:F/A),M,F,A),
   nop(System:import(M:F/A)),!.

% uses_predicate(true,_, M,F,A, Retry):-  retry_undefined(M,F,A),!,Retry=retry.

uses_predicate(_,_, M,F,A, Retry):-  retry_undefined(M,F,A),!,Retry=retry.

uses_predicate(DEF,_, M, F,A, Retry):-  call(DEF, M:F/A),!,Retry=retry.

:- if(\+ current_predicate(autoload_library_index/4)).
in_autoload_library_index(F,A,_PredMt,File):- '$in_library'(F,A,File).
:- else.
in_autoload_library_index(F,A,PredMt,File):- autoload_library_index(F,A,PredMt,File).
:- endif.

:- meta_predicate with_no_retry_undefined(:).
with_no_retry_undefined(Goal):- locally(set_prolog_flag(retry_undefined, none),
                                     locally(set_prolog_flag(runtime_debug,0),Goal)).


% Every module has it''s own
retry_undefined(CallerMt,'$pldoc',4):- multifile(CallerMt:'$pldoc'/4),discontiguous(CallerMt:'$pldoc'/4),dynamic(CallerMt:'$pldoc'/4),!.
% System-like Autoloads (TODO: confirm these can be removed)
retry_undefined(CallerMt,debug,1):- use_module(CallerMt:library(debug)),!.
retry_undefined(CallerMt,debugging,1):- use_module(CallerMt:library(debug)),!.
retry_undefined(CallerMt,member,2):- use_module(CallerMt:library(lists)),!.
retry_undefined(CallerMt,directory_file_path,3):- use_module(CallerMt:library(filesex)),!.
% 3 very special Mts
% Module defines the type
% retry_undefined(baseKB,F,A):- make_as_dynamic(retry_undefined(baseKB),baseKB,F,A),!.
retry_undefined(lmcache,F,A):- volatile(lmcache:F/A),make_as_dynamic(retry_undefined(lmcache),lmcache,F,A),!.
retry_undefined(t_l,F,A):- thread_local(t_l:F/A),!,make_as_dynamic(retry_undefined(t_l),t_l,F,A),!.


:- if(false).

% adult-like Mt
retry_undefined_falsed_out(Mt, F, A):-  clause_b(mtCycLBroad(Mt)), clause_b(hybrid_support(F,A)),
   make_as_dynamic(mtCycLBroad(Mt),Mt,F,A).

% child-like Mt
retry_undefined_falsed_out(CallerMt,F,A):- clause_b(mtGlobal(CallerMt)), clause_b(hybrid_support(F,A)),
   % find_and_call(baseKB:mtGlobal(CallerMt)),
   create_predicate_inheritance(retry_undefined_falsed_out(CallerMt:F/A),CallerMt,F,A).

% import built-ins ?
retry_undefined_falsed_out(CallerMt,F,A):- current_predicate(system:F/A), current_module(M),M\=system,
  current_predicate(M:F/A),functor(P,F,A),predicate_property(M:P,defined),\+predicate_property(M:P,imported_from(_)),
  CallerMt:import(M:F/A).

% our autoloader hacks
retry_undefined_falsed_out(CallerMt,F,A):-
   in_autoload_library_index(F,A,_PredMt,File),
   use_module(CallerMt:File),!.

% Autoloads importing the entire other module
retry_undefined_falsed_out(CallerMt,F,A):- fail,fail,fail,fail,fail,fail,fail,fail,fail,fail,fail,fail,fail,fail,fail,fail,fail,fail,fail,fail,
       in_autoload_library_index(F,A,PredMt,File),
       asserta(lmcache:how_registered_pred(PredMt:use_module(CallerMt:File),CallerMt,F,A)),
       use_module(system:File),!.
       % system:add_import_module(CallerMt,system,start).


retry_undefined_falsed_out(CallerMt,F,A):- fail,
       in_autoload_library_index(F,A,_,File),
       load_files(CallerMt:File,[if(true),imports([F/A]),register(false),silent(false)]),!.

% Autoloads importing the entire other module
retry_undefined_falsed_out(CallerMt,F,A):- fail,fail,fail,fail,fail,fail,fail,fail,fail,fail,fail,fail,fail,fail,fail,fail,fail,fail,fail,fail,
       in_autoload_library_index(F,A,PredMt,File),
       asserta(lmcache:how_registered_pred(PredMt:use_module(CallerMt:File),CallerMt,F,A)),
       use_module(CallerMt:File),!.

/*
retry_undefined(CallerMt,F,A):-
      in_autoload_library_index(F,A,PredMt,File),
      ((current_module(PredMt),current_predicate(PredMt:F/A))
       -> add_import_module(CallerMt,PredMt,start) ;
       (PredMt:ensure_loaded(PredMt:File),add_import_module(CallerMt,PredMt,start))),!.
*/

retry_undefined_falsed_out(CallerMt,F,A):- fail,fail,fail,fail,fail,fail,fail,fail,fail,fail,fail,fail,fail,fail,fail,fail,fail,fail,fail,fail,
   functor(P,F,A),find_module(P,M),show_call(CallerMt:import(M:F/A)),!.



%retry_undefined(PredMt:must/1) % UNDO % :- add_import_module(PredMt,logicmoo_util_catch,start),!.
%retry_undefined(PredMt:debugm/2) % UNDO % :- add_import_module(PredMt,logicmoo_util_dmsg,start),!.

:- endif.



%uses_undefined_hook(CM):- (clause_b(genlMt(CM,_));clause_b(mtHybrid(CM))).
uses_undefined_hook(CM):- nonvar(CM),clause(mtNoInheritance(CM),true),!,fail.
uses_undefined_hook(CM):- clause_b(genlMt(CM,_)),!.
% uses_undefined_hook(CM):- is_pfc_module(CM),!.
uses_undefined_hook(baseKB).
%uses_undefined_hook(user).


user_exception_undefined_predicate(CM,M,F,A,ActionO):- 
  \+ prolog_load_context(reloading,true),
  current_prolog_flag(retry_undefined, Was), Was \== false, Was \== none,
  get_retry_undefined_hook(M,Setting),!, Setting\==error,    
   CM:setup_call_cleanup(set_prolog_flag(retry_undefined, false),
                      (uses_predicate(Setting,CM,M,F,A, ActionO), ActionO \== error),
                      set_prolog_flag(retry_undefined, Was)),!.


:- fixup_exports.

:- multifile(prolog:make_hook/2).
:- dynamic(prolog:make_hook/2).
:- multifile(lmcache:was_retry_undefined/2).
:- dynamic(lmcache:was_retry_undefined/2).
:- dynamic(prolog:make_hook/2).
prolog:make_hook(before, C):- current_prolog_flag(retry_undefined, WAS),asserta(lmcache:was_retry_undefined(WAS,C)),set_prolog_flag(retry_undefined, false),fail.

prolog:make_hook(after, C):- retract(lmcache:was_retry_undefined(WAS,C)),set_prolog_flag(retry_undefined, WAS),fail.

:- multifile(user:exception/3).
:- module_transparent(user:exception/3).
:- dynamic(user:exception/3).
/*
user:exception(undefined_predicate, F/A, ActionO):- !, strip_module(F/A,CM,_), 
  user_exception_undefined_predicate(CM,CM,F,A, ActionO).
user:exception(undefined_predicate, M:F/A, ActionO):- !, strip_module(F/A,CM,_), 
  user_exception_undefined_predicate(CM,M,F,A, ActionO).

*/
