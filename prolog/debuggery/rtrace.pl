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
:- module(rtrace,
   [
      rtrace/1,  % Non-interactive tracing
      rtrace_break/1,  % Interactive tracing
      quietly/1,  % Non-det notrace/1
      restore_trace/1, % After call restor tracer
      rtrace/0, % Start non-intractive tracing
      srtrace/0, % Start non-intractive tracing at System level
      nortrace/0, % Stop non-intractive tracing
      push_tracer/0,pop_tracer/0,reset_tracer/0, % Reset Tracer to "normal"
      on_x_debug/1, % Non-intractive tracing when exception occurs 
      on_f_rtrace/1, % Non-intractive tracing when failure occurs 
      maybe_leash/1, % Set leash only when it makes sense
      should_maybe_leash/0,
      non_user_console/0,
      ftrace/1, % rtrace showing only failures
      push_guitracer/0,pop_guitracer/0
   ]).


:- set_module(class(library)).
:- module_transparent(nortrace/0).
:- system:use_module(library(logicmoo_startup)).

:- prolog_load_context(directory,Dir),add_file_search_path_safe(library,Dir).



:-thread_local(t_l:rtracing/0).
:-thread_local(t_l:tracer_reset/1).
:-thread_local(t_l:was_gui_tracer/1).
:-thread_local(t_l:wastracer/1).

:- 'meta_predicate'(call_call(0)).
call_call(G):-call(G).


:- meta_predicate
   rtrace(0),
   restore_trace(0),
   on_x_debug(0),
   on_f_rtrace(0),  
   
   rtrace_break(0),
   quietly(*),
   ftrace(0).

%! on_f_rtrace( :Goal) is det.
%
% If :Goal fails trace it 
%


% on_f_rtrace(Goal):-  Goal *-> true; ((nortrace,notrace,debugCallWhy(failed(on_f_rtrace(Goal)),Goal)),fail).

on_f_rtrace(Goal):-  Goal *-> true; (ignore(rtrace(Goal)),debugCallWhy(on_f_rtrace(Goal),Goal)).


:- meta_predicate on_x_rtrace(*).
on_x_rtrace(G):-on_x_debug(G).

%! on_x_debug( :Goal) is det.
%
% If there If Is an exception in :Goal then rtrace.
%
on_x_debug(Goal):- 
 (( notrace( tracing; t_l:rtracing),!,maybe_leash(+exception))) 
  -> Goal
   ;
   (catchv(Goal,E,(ignore(debugCallWhy(on_x_debug(E,Goal),rtrace(Goal))),throw(E)))).



unhide(Pred):- old_set_predicate_attribute(Pred, trace, true),mpred_trace_childs(Pred).

%! maybe_leash( +Flags) is det.
%
% Only leashes interactive consoles
%
maybe_leash(Some):- is_list(Some),!,maplist(maybe_leash,Some).
maybe_leash(-Some):- !, leash(-Some).
maybe_leash(Some):- notrace((should_maybe_leash->leash(Some);true)).
:- totally_hide(maybe_leash/1).

should_maybe_leash:- notrace((\+ current_prolog_flag(runtime_must,keep_going), \+ non_user_console)).

%non_user_console:- !,fail.
%non_user_console:- thread_self(main),!,fail.
non_user_console:- notrace(((\+ stream_property(current_input, tty(true))));stream_property(current_input,close_on_abort(true))).

%! get_trace_reset( ?Reset) is det.
%
% Get Tracer.
%                    
get_trace_reset((notrace,set_prolog_flag(gui_tracer, GWas),
     '$visible'(_, OldV),'$leash'(_, OldL),set_prolog_flag(debug,WasDebug),RestoreTrace)):- 
     (tracing -> (notrace,RestoreTrace = trace) ; RestoreTrace = notrace),
     current_prolog_flag(debug,WasDebug),
     '$leash'(OldL, OldL),
     '$visible'(OldV, OldV),
     (current_prolog_flag(gui_tracer, GWas)->true;GWas=false),
     RestoreTrace.

:- totally_hide(get_trace_reset/1).
:- totally_hide(get_trace_reset/1).



%! push_guitracer is det.
%
% Save Guitracer.
%
push_guitracer:-  notrace(ignore(((current_prolog_flag(gui_tracer, GWas);GWas=false),asserta(t_l:was_gui_tracer(GWas))))).
:- totally_hide(push_guitracer/0).


%! pop_guitracer is det.
%
% Restore Guitracer.
%
pop_guitracer:- notrace(ignore(((retract(t_l:was_gui_tracer(GWas)),set_prolog_flag(gui_tracer, GWas))))).
:- totally_hide(pop_guitracer/0).



%! push_tracer is det.
%
% Push Tracer.
%
push_tracer:- get_trace_reset(Reset),notrace(asserta(t_l:tracer_reset(Reset))).
:- totally_hide(push_tracer/0).

%! pop_tracer is det.
%
% Pop Tracer.
%
pop_tracer:- notrace((retract(t_l:tracer_reset(Reset))))->Reset;notrace(true).
:- totally_hide(pop_tracer/0).

%! reset_tracer is det.
%
% Reset Tracer.
%
reset_tracer:- notrace,ignore((t_l:tracer_reset(Reset)->Reset;true)).
:- totally_hide(reset_tracer/0).


:- multifile(user:prolog_exception_hook/4).
:- dynamic(user:prolog_exception_hook/4).
:- module_transparent(user:prolog_exception_hook/4).

% Make sure interactive debugging is turned back on

user:prolog_exception_hook(error(_, _),_, _, _) :- leash(+all),fail.

user:prolog_exception_hook(error(_, _),_, _, _) :- fail, 
   notrace((  reset_tracer ->
     should_maybe_leash ->
     t_l:rtracing ->
     leash(+all),
     fail)).

%! quietly( :Goal) is det.
%
% Unlike notrace/1, it allows nondet tracing 
%
% But also may be break when excpetions are raised during Goal.
%
% version 21
quietly1(Goal):- \+ tracing -> Goal ; scce_orig(notrace,Goal,trace).

% version 2 
quietly(Goal):- \+ tracing -> Goal ; (setup_call_cleanup(notrace,trusted_redo_call_cleanup(notrace,Goal,trace),trace)).

:- 'set_pred_attrs'(quietly(_),[trace=1,hide_childs=1]).

% version 3 
% quietly(Goal):- !, Goal.  % for overiding
quietly3(Goal):- \+ tracing, !, Goal.
quietly3(Goal):- notrace,(Goal*-> (deterministic(true) -> trace ; (trace;(notrace,fail))) ; (trace,!,notrace(fail))).

% :- 'totally_hide'(rtrace:quietly/1).
:- old_set_predicate_attribute(rtrace:quietly/1, hide_childs, true).

% Alt version?
quietlySE(Goal):- \+ tracing -> Goal ; 
 notrace((S = notrace, E = trace)),
 (S,
  (((Goal,deterministic(YN))) *->
     (YN == true -> E ; (E;(S,fail)));
  (E,!,notrace(fail)))).

% Alt version?
rtraceSE(Goal):-  
 notrace((S = rtrace, E = nortrace)),
 (S,
  (((Goal,deterministic(YN))) *->
     (YN == true -> E ; (E;(S,fail)));
  (E,!,notrace(fail)))).



deterministically_must(G):- (call(call,G),deterministic(YN),true),
  (YN==true -> true; 
     ((wdmsg(failed_deterministically_must(G)),(!)))),!.


%:- totally_hide(quietly/1).


%! rtrace is det.
%
% Start RTracer.
%

rtrace:- start_rtrace,trace.

:- 'totally_hide'(rtrace/0).

start_rtrace:- notrace(t_l:rtracing), !,  leash(-all), assert(t_l:rtracing), push_guitracer.
start_rtrace:-
      leash(-all),
      visible(-all),
      assert(t_l:rtracing),
     % set_prolog_flag(access_level,system),
      push_guitracer,
      set_prolog_flag(gui_tracer,false),      
      maybe_leash(+exception),
      visible(+exception).

:- 'totally_hide'(start_rtrace/0).

%! srtrace is det.
%
% Start RTracer.
%
srtrace:- notrace, set_prolog_flag(access_level,system), rtrace.

:- totally_hide(srtrace/0).



%! nortrace is det.
%
% Stop Tracer.
%
stop_rtrace:- 
  notrace,
  maybe_leash(+all),
  visible(+all),
  maybe_leash(+exception),
  retractall(t_l:rtracing),
  !.

:- 'totally_hide'(stop_rtrace/0).
:- system:import(stop_rtrace/0).

nortrace:- stop_rtrace,ignore(pop_tracer).

:- totally_hide(nortrace/0).



:- thread_local('$leash_visible'/2).

%! restore_trace( :Goal) is det.
%
% restore  Trace.
%
%! restore_trace( :Goal) is det.
%
% restore  Trace.
%
restore_trace(Goal):- 
  setup_call_cleanup(
   push_leash_visible,
   scce_orig(push_tracer,Goal,pop_tracer),
   restore_leash_visible).

restore_trace0(Goal):- 
  '$leash'(OldL, OldL),'$visible'(OldV, OldV),
   scce_orig(restore_leash_visible,
   ((Goal*-> (push_leash_visible, '$leash'(_, OldL),'$visible'(_, OldV)) ; fail)),
   ('$leash'(_, OldL),'$visible'(_, OldV))).

:- totally_hide(system:'$leash'/2).
:- totally_hide(system:'$visible'/2).

push_leash_visible:- notrace((('$leash'(OldL0, OldL0),'$visible'(OldV0, OldV0), asserta('$leash_visible'(OldL0,OldV0))))).
restore_leash_visible:- notrace((('$leash_visible'(OldL1,OldV1)->('$leash'(_, OldL1),'$visible'(_, OldV1));true))).

% restore_trace(Goal):- setup_call_cleanup(get_trace_reset(Reset),Goal,notrace(Reset)).
:- totally_hide(restore_trace/1).



%! rtrace( :Goal) is det.
%
% Trace a goal non-interactively until the first exception on
%  total failure
%
% ?- rtrace(member(X,[1,2,3])).
%    Call: (9) [lists] lists:member(_7172, [1, 2, 3])    
%    Unify: (9) [lists] lists:member(_7172, [1, 2, 3])   
%    Call: (10) [lists] lists:member_([2, 3], _7172, 1)  
%    Unify: (10) [lists] lists:member_([2, 3], 1, 1)     
%    Exit: (10) [lists] lists:member_([2, 3], 1, 1)      
%    Exit: (9) [lists] lists:member(1, [1, 2, 3])        
% X = 1 ;                                                
%    Redo: (10) [lists] lists:member_([2, 3], _7172, 1)  
%    Unify: (10) [lists] lists:member_([2, 3], _7172, 1) 
%    Call: (11) [lists] lists:member_([3], _7172, 2)     
%    Unify: (11) [lists] lists:member_([3], 2, 2)        
%    Exit: (11) [lists] lists:member_([3], 2, 2)         
%    Exit: (10) [lists] lists:member_([2, 3], 2, 1)      
%    Exit: (9) [lists] lists:member(2, [1, 2, 3])        
% X = 2 ;                                                
%    Redo: (11) [lists] lists:member_([3], _7172, 2)     
%    Unify: (11) [lists] lists:member_([3], _7172, 2)    
%    Call: (12) [lists] lists:member_([], _7172, 3)      
%    Unify: (12) [lists] lists:member_([], 3, 3)         
%    Exit: (12) [lists] lists:member_([], 3, 3)          
%    Exit: (11) [lists] lists:member_([3], 3, 2)         
%    Exit: (10) [lists] lists:member_([2, 3], 3, 1)      
%    Exit: (9) [lists] lists:member(3, [1, 2, 3])        
% X = 3.                                                 
%                                                        
%  ?- rtrace(fail).                                      
%    Call: (9) [system] fail                             
%    Fail: (9) [system] fail                             
% ^  Redo: (8) [rtrace] rtrace:rtrace(user:fail)
% false.

/*
  ?- rtrace((member(X,[writeln(1),throw(good),writen(failed)]),X)).
   Call: (10) [lists] lists:member(_13424, [writeln(1), throw(good), writen(failed)])
   Unify: (10) [lists] lists:member(_13424, [writeln(1), throw(good), writen(failed)])
   Call: (11) [lists] lists:member_([throw(good), writen(failed)], _13424, writeln(1))
   Unify: (11) [lists] lists:member_([throw(good), writen(failed)], writeln(1), writeln(1))
   Exit: (11) [lists] lists:member_([throw(good), writen(failed)], writeln(1), writeln(1))
   Exit: (10) [lists] lists:member(writeln(1), [writeln(1), throw(good), writen(failed)])
   Call: (10) [system] writeln(1)
1
   Exit: (10) [system] writeln(1)
X = writeln(1) ;
   Redo: (11) [lists] lists:member_([throw(good), writen(failed)], _13424, writeln(1))
   Unify: (11) [lists] lists:member_([throw(good), writen(failed)], _13424, writeln(1))
   Call: (12) [lists] lists:member_([writen(failed)], _13424, throw(good))
   Unify: (12) [lists] lists:member_([writen(failed)], throw(good), throw(good))
   Exit: (12) [lists] lists:member_([writen(failed)], throw(good), throw(good))
   Exit: (11) [lists] lists:member_([throw(good), writen(failed)], throw(good), writeln(1))
   Exit: (10) [lists] lists:member(throw(good), [writeln(1), throw(good), writen(failed)])
   Call: (10) [system] throw(good)
ERROR: Unhandled exception: good
*/

set_leash_vis(OldL,OldV):- '$leash'(_, OldL),'$visible'(_, OldV),!.
:- totally_hide(set_leash_vis/2).

next_rtrace:- (nortrace;(rtrace,trace,notrace(fail))).
:- 'totally_hide'(next_rtrace/0).

rtrace(Goal):- notrace(non_user_console),!,notrace,set_prolog_flag(debug,false),setup_call_cleanup((leash(-all),set_prolog_flag(gui_tracer,false),trace),Goal,notrace).
rtrace(Goal):- push_tracer,notrace,!,setup_call_cleanup(rtrace,
  (((Goal,deterministic(YN))*->(notrace(YN==true)->true;(reset_tracer;(rtrace,notrace(fail)))))),pop_tracer).
/*
rtrace(Goal):- 
  % notrace(non_user_console -> leash(-all) ; true),     
 tracing -> (notrace,(rtrace0(Goal)*->trace;(trace,fail))) ; 

 setup_call_cleanup(
    current_prolog_flag(debug,WasDebug),
    rtrace0(Goal),
    (set_prolog_flag(debug,WasDebug),stop_rtrace)).

rtrace0(Goal):-
 setup_call_cleanup(
    (notrace,current_prolog_flag(debug,WasDebug),rtrace),
    (trace,Goal,notrace,deterministic(YN), (YN == true -> !;next_rtrace)),
    set_prolog_flag(debug,WasDebug)).
*/
%:- '$hide'(system:tracing/0).
%:- '$hide'(system:notrace/1).
%:- old_set_predicate_attribute(system:notrace/1, hide_childs, true).
%:- '$hide'(system:notrace/0).
%:- '$hide'(system:trace/0).

:- 'totally_hide'(rtrace:rtrace/1).
:- old_set_predicate_attribute(rtrace:rtrace/1, hide_childs, false).
:- '$hide'(rtrace:reset_rtrace0/1).
:- old_set_predicate_attribute(rtrace:reset_rtrace0/1, hide_childs, true).
%:- old_set_predicate_attribute(rtrace:reset_rtrace0/1, hide_childs, false).


%! rtrace_break( :Goal) is det.
%
% Trace a goal non-interactively and break on first exception 
% or on total failure
%
rtrace_break(Goal):- \+ should_maybe_leash, !, rtrace(Goal).
rtrace_break(Goal):- stop_rtrace,trace,debugCallWhy(rtrace_break(Goal),Goal).
%:- totally_hide(rtrace_break/1).
:- old_set_predicate_attribute(rtrace_break/1, hide_childs, false).




%:- '$hide'(quietly/1).
%:- if_may_hide('totally_hide'(notrace/1,  hide_childs, 1)).
%:- if_may_hide('totally_hide'(notrace/1)).
:- totally_hide(system:tracing/0).
:- totally_hide(system:notrace/0).
:- totally_hide(system:notrace/1).
:- totally_hide(system:trace/0).

%! ftrace( :Goal) is det.
%
% Functor Trace.
%
ftrace(Goal):- restore_trace((
   visible(-all),visible(+unify),
   visible(+fail),visible(+exception),
   maybe_leash(-all),maybe_leash(+exception),trace,Goal)).



how_must(How, Goal):- locally(set_prolog_flag(runtime_must,How),Goal).

keep_going(Goal):- how_must(keep_going, Goal).

ignore_must(Goal):- how_must(fail, Goal).



:- ignore((source_location(S,_),prolog_load_context(module,M),module_property(M,class(library)),
 forall(source_file(M:H,S),
 ignore((functor(H,F,A),
  ignore(((\+ atom_concat('$',_,F),(export(F/A) , current_predicate(system:F/A)->true; system:import(M:F/A))))),
  ignore(((\+ predicate_property(M:H,transparent), module_transparent(M:F/A), \+ atom_concat('__aux',_,F),
   prolog_debug:debug(modules,'~N:- module_transparent((~q)/~q).~n',[F,A]))))))))).

% % % OFF :- system:use_module(library(logicmoo_utils_all)).
:- fixup_exports.
:- totally_hide('$toplevel':save_debug).
:- totally_hide('$toplevel':toplevel_call/1).
%:- totally_hide('$toplevel':residue_vars(_,_)).
:- totally_hide('$toplevel':save_debug).
:- totally_hide('$toplevel':no_lco).
:- ignore(rtrace(non_user_console)).
