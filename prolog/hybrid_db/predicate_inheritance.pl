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
:- module(predicate_inheritance,
          [
check_mfa/4,
%skip_mfa/4,
create_predicate_inheritance/4,
now_inherit_above/4,
decl_as/2,
decl_kb_global/3,

decl_kb_shared/3,
decl_kb_local/3,
decl_kb_type/4,
predicate_m_f_a_decl/4,
do_import/4,
kb_local/1,
(kb_global)/1,
(kb_local)/1,
(kb_shared)/1,
make_as_dynamic/4
]).

:- set_module(class(library)).
:- reexport(library(must_sanity)).
:- reexport(library(loop_check/no_loops)).

:- meta_predicate decl_as(*,+).
:- meta_predicate decl_as_rev(+,*).


:- if( \+ current_op(_,_,(kb_global))).

:- current_prolog_flag(access_level,Was),
   set_prolog_flag(access_level,system),
   op(1150,fx,(kb_global)),
   op(1150,fx,(kb_global)),
   op(1150,fx,(kb_local)),
   set_prolog_flag(access_level,Was).

:- endif.


:- module_transparent((
check_mfa/4, 
%skip_mfa/4,
create_predicate_inheritance/4,
now_inherit_above/4,
decl_as/2,
do_import/4,
(kb_local)/1,
(kb_global)/1,
(kb_shared)/1,
make_as_dynamic/4
          )).



%% create_predicate_inheritance(+Reason,+ChildDefMt,+F,+A) is semidet.
%
% Ensure inherit_above/2 stub is present in ChildDefMt.
%

%create_predicate_inheritance(Reason,CallerMt,F,A):- clause_b((baseKB:mpred_prop(CallerMt,F,A,inherits_above))),!.
%create_predicate_inheritance(Reason,CallerMt,F,A):- assert_if_new((baseKB:mpred_prop(CallerMt,F,A,inherits_above))),fail.

create_predicate_inheritance(_Reason,CallerMt,F,A):- lmcache:already_decl(kb_global,M,F,A),!,CallerMt:import(M:F/A).
%create_predicate_inheritance(Reason,_,F,A):- lmcache:already_decl(kb_shared,_,F,A),!.
%create_predicate_inheritance(Reason,M,F,A):- show_success(lmcache:already_decl(kb_local,M,F,A)),!.
%create_predicate_inheritance(Reason,M,F,A):- show_success(lmcache:already_decl(kb_shared,M,F,A)),!.

create_predicate_inheritance(Reason,CallerMt,F,A):- now_inherit_above(Reason,CallerMt,F,A),!.



% TODO unsuspect the next line (nothing needs to see above baseKB)

%% now_inherit_above(+Reason,+ChildDefMt,+F,+A) is semidet.
%
% Ensure now_inherit_above/2 stub is present in ChildDefMt.
%
now_inherit_above(_Reason,Nonvar,F,A):- var(Nonvar)-> break ; (sanity(ground(now_inheritance(Nonvar,F,A))),fail).
now_inherit_above(Reason,baseKB,F,A):- !, make_as_dynamic(Reason,baseKB,F,A).
/*
now_inherit_above(_Reason,baseKB,F,A):- !,
  make_as_dynamic(now_inherit_above(_Reason,baseKB,F,A),baseKB,F,A), 
     ignore((( \+ (defaultAssertMt(CallerMt),CallerMt\==baseKB,now_inherit_above(_Reason,CallerMt,F,A) )))).
*/
now_inherit_above(Reason,abox,F,A):-  
       !, must(defaultAssertMt(CallerMt)),
       sanity(CallerMt\=abox),!,
       now_inherit_above(Reason,CallerMt,F,A).

now_inherit_above(Reason,CallerMt,F,A):- fail, clause_b(mtProlog(CallerMt)),
   sanity(\+ clause_b(mtHybrid(CallerMt))),!,
   wdmsg(warn(create_predicate_istAbove_mtProlog(Reason,CallerMt,F,A))),dtrace.

now_inherit_above(_Reason,CallerMt,F,A):- 
  lmcache:already_decl(kb_global,M,F,A),do_import(CallerMt,M,F,A),!.

now_inherit_above(Reason,CallerMt,F,A):-
   make_as_dynamic(Reason,CallerMt,F,A),
   functor(Goal,F,A),
   CallerMt:import(inherit_above/2),
   CallerMt:import(do_ihherit_above/2),
   get_inherit_above_clause(CallerMt,Goal,Head,Body),
   CallerMt:assertz_new(Head:-Body).

% get_inherit_above_clause(CallerMt,Goal,Head,Body)
get_inherit_above_clause(From,Goal,IAHead,IABody):-
   (nonvar(Goal)->(strip_module(Goal,_,Call), functor(Call,F,A),functor(Head,F,A)) ; Goal=Head),
   IAHead = From:Head,
   IABody = (zwc,inherit_above(From,Head)).

awc:-true.
zwc:-true.

:- module_transparent(system:inherit_above/2).
:- export(system:inherit_above/2).
system:inherit_above(Mt,Query):- 
   \+ context_module(baseKB), 
   Mt\=baseKB, 
   Query\=do_inherit_above(_,_),
   do_inherit_above(Mt,Query).

never_move(spft,_).
never_move(mpred_prop,_).
never_move(meta_argtypes,_).
never_move(pt,_).
never_move(bt,_).
never_move(tc,_).
never_move(proven_tru,_).
never_move(is_pfc_file,_):- dumpST,break.

%never_move(_,_).

:- module_transparent(system:do_inherit_above/2).
:- export(system:do_inherit_above/2).
:- thread_local(t_l:exact_kb/1).
system:do_inherit_above(Mt,_):- t_l:exact_kb(Mt),!,fail.
system:do_inherit_above(user,_):- context_module(user),!,fail.

system:do_inherit_above(Mt,QueryIn):- 
   functor(QueryIn,F,A),\+ never_move(F,A),
   predicate_property(QueryIn,number_of_clauses(N)),
   Mt:nth_clause(QueryIn,N,Ref),clause(_,Body,Ref),
   get_inherit_above_clause(Mt,QueryIn,IAHead,IABody),
   Body\=IABody,
   once((Mt:clause(IAHead,IABody,Kill),
   erase(Kill),% functor(QueryIn,F,A),
   dmsg(moving(IAHead)), % inherit_above(Mt,Query)
   Mt:assertz(IAHead:-IABody))),fail.
   

  % TODO   no_repeats(MtAbove,(clause(Mt:genlMt(Mt,MtAbove),true);clause(baseKB:genlMt(Mt,MtAbove),true))),

system:do_inherit_above(Mt,Query):- 
   clause(genlMt(Mt,MtAbove),true),
   do_call_inherited(MtAbove,Query).

:- module_transparent(system:do_call_inherited/2).
:- export(system:do_call_inherited/2).
system:do_call_inherited(MtAbove,Query):- 
   \+ current_prolog_flag(retry_undefined,none),
   % use_inheritance(MtAbove),
   \+ current_predicate(_,MtAbove:Query),
   functor(Query,F,A) -> create_predicate_inheritance(do_call_inherited(MtAbove,Query),MtAbove,F,A) -> fail.

system:do_call_inherited(MtAbove,Query):- !, on_x_debug(MtAbove:Query).
system:do_call_inherited(MtAbove,Query):- ireq(MtAbove:Query).
  


export_everywhere(system,F,A):- !, system:export(system:F/A).
export_everywhere(user,F,A):- !,user:export(user:F/A),system:import(user:F/A),baseKB:import(user:F/A).
export_everywhere(baseKB,F,A):- !, baseKB:export(baseKB:F/A),system:import(baseKB:F/A),user:import(baseKB:F/A).
export_everywhere(M,F,A):- M:export(M:F/A),system:import(M:F/A),user:import(M:F/A),baseKB:import(M:F/A).

%make_as_dynamic(M,F,A):- make_as_dynamic(make_as_dynamic,M,F,A).

make_as_dynamic(Reason,M,F,A):- Reason= kb_global(_),!,make_as_dynamic_realy(Reason,M,F,A),export_everywhere(M,F,A).
make_as_dynamic(Reason,M,F,A):- Reason= kb_local(_),!,make_as_dynamic_realy(Reason,M,F,A),!. 
make_as_dynamic(Reason,M,F,A):- Reason= decl_kb_type(_,_),!,make_as_dynamic_realy(Reason,M,F,A),!. 
make_as_dynamic(Reason,M,F,A):- F== is_pfc_file, break, make_as_dynamic_realy(Reason,M,F,A).
make_as_dynamic(Reason,M,F,A):- dmsg(make_as_dynamic(Reason,M,F,A)),!,make_as_dynamic_realy(Reason,M,F,A),!. 

:- multifile(user:message_hook/3).
:- dynamic(user:message_hook/3).
user:message_hook(import_private(Module, Private),_,_):- Module==system,!, nop(dmsg(import_private(Module, Private))).
user:message_hook(import_private(Module, Private),_,_):- current_prolog_flag(runtime_message_hook, true), dmsg(import_private(Module, Private)).

make_as_dynamic_realy(Reason,M,F,A):- 
 must_det_l((
   functor(PI,F,A),
   M:multifile(M:F/A),
   M:discontiguous(M:F/A),
   M:module_transparent(M:F/A),
   (is_static_predicate(M:PI) -> true ; (predicate_property(M:PI,dynamic) -> true ; must(M:dynamic(M:PI)))),   
   public(M:F/A),
   nop(on_f_throw( (M:F/A)\== (baseKB:loaded_external_kbs/1))),
   nop(assertz_if_new(( M:PI :- (fail,infoF(createdFor(Reason)))))))).


do_inherit(_SM,_M,_F,_A).

% TODO uncomment these out!
%do_import(system,M,F,A):-throw(unexpected(do_import(system,M,F,A))).
%do_import(user,M,F,A):-throw(unexpected(do_import(user,M,F,A))).
do_import(TM,M,F,A):- 
   must((TM:import(M:F/A),TM:export(TM:F/A))),!.
   % must((TM:module_transparent(M:F/A))). % in case this has clauses th

%% decl_as(Types, TermM) is semidet.
%
% Declare as Types.
%
decl_as(Types,Var):-var(Var),!,trace_or_throw(var_decl_shared(Types,Var)).
decl_as(Types,M:FA):- if_defined(defaultAssertMt(M),fail),!,decl_as(Types,FA),!.
decl_as(Types,abox:FA):-!,decl_as(Types,FA),!.
decl_as(Types,_:M:G1):-!,decl_as(Types,M:G1),!.

decl_as(Types,(G1,G2)):-!,decl_as(Types,G1),!,decl_as(Types,G2),!.
decl_as(Types,[G1]):-!,decl_as(Types,G1),!.
decl_as(Types,[G1|G2]):-!,decl_as(Types,G1),!,decl_as(Types,G2),!.
decl_as(Types,M:(G1,G2)):-!,decl_as(Types,M:G1),!,decl_as(Types,M:G2),!.
decl_as(Types,M:[G1]):-!,decl_as(Types,M:G1),!.
decl_as(Types,M:[G1|G2]):-!,decl_as(Types,M:G1),!,decl_as(Types,M:G2),!.
decl_as(Types,M:F):-atom(F),!,decl_as(Types,M,F,_).
decl_as(Types,F):-atom(F),!,decl_as(Types,_,F,_).
decl_as(Types,M:F//Am2):-!,A is Am2+2, decl_as(Types,M,F,A).
decl_as(Types,M:F/A):-!,decl_as(Types,M,F,A).
decl_as(Types,F//Am2):-!,A is Am2+2, decl_as(Types,_,F,A).
decl_as(Types,F/A):-!,decl_as(Types,_,F,A).
decl_as(Types,M:Goal):-compound(Goal),!,functor(Goal,F,A),decl_as(Types,M,F,A).
decl_as(Types,Goal):-compound(Goal),!,functor(Goal,F,A),decl_as(Types,_,F,A).
decl_as(Types,Goal):-trace_or_throw(bad_decl_as(Types,Goal)).


decl_as(Types,M,F,A):- var(M),if_defined(defaultAssertMt(M),M=baseKB),!,decl_as(Types,M,F,A).
decl_as(Types,M,F,A):- var(A),!,forall(between(1,12,A),decl_as(Types,M,F,A)).
decl_as(M:Types,M,F,A):-!, decl_as(Types,M,F,A).
decl_as(Types,M,F,A):-!, decl_as_rev(M:F/A,Types).

decl_as_rev(MFA,(G1,G2)):-!,decl_as_rev(MFA,G1),!,decl_as_rev(MFA,G2),!.
decl_as_rev(MFA,[G1]):-!,decl_as_rev(MFA,G1),!.
decl_as_rev(MFA,[G1|G2]):-!,decl_as_rev(MFA,G1),!,decl_as_rev(MFA,G2),!.
decl_as_rev(MFA,M:(G1,G2)):-!,decl_as_rev(MFA,M:G1),!,decl_as_rev(MFA,M:G2),!.
decl_as_rev(MFA,M:[G1]):-!,decl_as_rev(MFA,M:G1),!.
decl_as_rev(MFA,M:[G1|G2]):-!,decl_as_rev(MFA,M:G1),!,decl_as_rev(MFA,M:G2),!.

decl_as_rev(M:F/A,_OM:Pred):- check_mfa(Pred,M,F,A),
  must(call(Pred,M,F,A)),!.


decl_as_rev(M:F/A,Pred):- check_mfa(Pred,M,F,A),
  must(call(Pred,M,F,A)).











% skip_mfa(Why,M, genlMt, 2):- baseKB\=M,dumpST,dmsg(skip_mfa(Why,M, genlMt, 2)),!,break.
check_mfa(_Why,M,F,A):-sanity(atom(F)),sanity(integer(A)),sanity(current_module(M)).



% kb_global(SPEC):- SPEC=(_:_), !, decl_as(decl_kb_global,SPEC), context_module(M),!,( \+ mtHybrid(M) -> M:import(SPEC); true).
kb_global(SPEC):- must(decl_as(decl_kb_global,SPEC)),!.



:- multifile(lmcache:already_decl/4).
:- dynamic(lmcache:already_decl/4).                                     

predicate_m_f_a_decl(M,F,A,Other):- lmcache:already_decl(Other,M,F,A).

pred_decl_kb_mfa_type(M,F,A,Other):- lmcache:already_decl(Other,M,F,A).


%:- dynamic(rdf_rewrite:decl_kb_global/3).
%:- multifile(rdf_rewrite:decl_kb_global/3).
%:- import(rdf_rewrite:decl_kb_global/3).


% TODO comment this out!
decl_kb_global(M,'==>',A):- !, dmsg(warn(skip(decl_kb_global(M,'==>',A)))).

decl_kb_global(M,F,A):- check_mfa(kb_global,M,F,A),!,
  (lmcache:already_decl(kb_global,M,F,A)->true;
  (asserta(lmcache:already_decl(kb_global,M,F,A)),do_decl_kb_global(M,F,A))),!.
decl_kb_global(M,F,A):- trace_or_throw(bad_kb_global(M,F,A)).

do_decl_kb_global(M,prologSingleValued,0):- trace_or_throw(do_decl_kb_global(M,prologSingleValued,0)).

do_decl_kb_global(M,F,A):-functor(PI,F,A),do_decl_kb_global_1(M,F,A,PI).

%:- rdf_rewrite:import(decl_kb_global/3).

%do_decl_kb_global_1(M,F,A,PI):- M\=baseKB,M\=elmt,M\=rdf_rewrite,\+ clause(baseKB:using_pfc(user,M,pfc_mod),true),dumpST,break,(trace_or_throw(do_decl_kb_global_m(M,F,A,PI))).
%do_decl_kb_global_1(M,F,A,PI):- if_defined(mpred_database_term(F,A,_),F = ~),dmsg(trace_or_throw(do_decl_kb_global_1(M,F,A,PI))).
do_decl_kb_global_1(M,F,A,PI):- lmcache:already_decl(Other,M,F,A), Other \== (kb_global), dmsg(warn(trace_or_throw(already_decl(Other,M,F,A,PI)))),!.

do_decl_kb_global_1(M,F,A,PI):- \+ predicate_property(M:PI,imported_from(_)), predicate_property(M:PI,defined),!,do_decl_kb_global_2(M,F,A,PI).
% not possible do_decl_kb_global_1(M,F,A,PI):- predicate_property(M:PI,imported_from(M)),!,do_decl_kb_global_2(M,F,A,PI).

do_decl_kb_global_1(M,F,A,PI):- predicate_property(M:PI,imported_from(R)),R\==M,!,
   show_failure(pfc(inherited_shared(R)),do_import(M,R,F,A)),
   do_decl_kb_global_2(R,F,A,PI),
   nop(do_import(system,R,F,A)),!.

do_decl_kb_global_1(M,F,A,PI):- current_predicate(F,R:PI), 
   \+ predicate_property(R:PI,inherited_from(_)),
   R\==M,
   dmsg(pfc(shared_found_peer(R,M:F/A))),
   do_import(M,R,F,A),
   do_decl_kb_global_2(R,F,A,PI),
   nop(do_import(system,R,F,A)),!.

do_decl_kb_global_1(M,F,A,PI):- do_decl_kb_global_2(M,F,A,PI),!.
  

do_decl_kb_global_2(M,F,A,_PI):- 
   nop(dmsg((do_decl_kb_global(M,F,A)))),
 must_det_l((
   make_as_dynamic(kb_global(M:F/A),M,F,A),
    M:export(M:F/A),
    do_import(baseKB,M,F,A),
    do_import(pfc_toplevel,M,F,A),   
    do_import(pfc_mod,M,F,A),   
    do_import(pfc_lib,M,F,A),   
    do_import(mpred_type_isa,M,F,A),

   do_import(system,M,F,A),   
% TODO BEGIN comment these out!
   do_import(user,M,F,A),
   %do_import(header_sane,M,F,A),      
   %'$current_source_module'(SM),do_import(SM,M,F,A),   
   %'$current_typein_module'(TM),do_import(TM,M,F,A),
% TODO END comment these out!
   decl_wrapped(M,F,A,ereq))).

   % on_f_throw( (M:F/A)\== (lmcache:loaded_external_kbs/1)),
   % (find_and_call(mtHybrid(M))->ain(baseKB:prologHybrid(F));true),


% kb_local(SPEC):- !,kb_global(SPEC),!.



kb_local(SPEC):- decl_as(decl_kb_local,SPEC),!.
kb_shared(SPEC):- decl_as(decl_kb_shared,SPEC),!.

decl_kb_shared(M,F,A):- lmcache:already_decl(kb_global,R,F,A), nop(dmsg(warn(kb_local(already_decl(kb_global,R->M,F,A))))),!.
decl_kb_shared(R,F,A):- lmcache:already_decl(kb_global,M,F,A),!,do_import(M,R,F,A).
decl_kb_shared(M,F,A):- decl_kb_type(kb_shared,M,F,A).

decl_kb_local(M,F,A):- lmcache:already_decl(kb_global,R,F,A), nop(dmsg(warn(kb_local(already_decl(kb_global,R->M,F,A))))),!.
decl_kb_local(R,F,A):- lmcache:already_decl(kb_global,M,F,A),!,do_import(M,R,F,A).
decl_kb_local(M,F,A):- decl_kb_type(kb_local,M,F,A).

decl_kb_type(Type,M,'==>',A):- A==1, !, nop(dmsg(skip(decl_kb_type(Type,M,'==>',A)))).

decl_kb_type(Type,M,F,A):- lmcache:already_decl(kb_global,R,F,A),M==R, 
   nop(dmsg(warn(trace_or_throw(already_decl(kb_global(Type),R->M,F,A))))),!.
decl_kb_type(Type,M,F,A):- lmcache:already_decl(kb_global,R,F,A), 
   dmsg(warn(trace_or_throw(already_decl(kb_global(Type),R->M,F,A)))),!.


decl_kb_type(Type,M,F,A):- check_mfa(Type,M,F,A),!,
  (lmcache:already_decl(Type,M,F,A)->true;
    (asserta(lmcache:already_decl(Type,M,F,A)),do_decl_kb_type(Type,Type,M,F,A))),!.
decl_kb_type(Type,M,F,A):- trace_or_throw(bad_decl_kb_type(Type,M,F,A)).

do_decl_kb_type(Type,Type,M,prologSingleValued,0):- 
  trace_or_throw(do_decl_kb_type(Type,Type,M,prologSingleValued,0)).

do_decl_kb_type(Type,Type,M,F,A):-functor(PI,F,A),do_decl_kb_type_1(Type,M,F,A,PI),!.

do_decl_kb_type_1(Type,M,F,A,_):- lmcache:already_decl(Other,M,F,A),Other\=(Type),!. % ,dmsg(lmcache:already_decl(Other,M,F,A)).

do_decl_kb_type_1(Type,M,F,A,PI):-
  predicate_property(M:PI,inherited_from(R)),R\==M,!,
  do_decl_kb_type_2(Type,R,F,A,PI),
  show_call(pfc(inherited_local(R)),do_import(M,R,F,A)).

do_decl_kb_type_1(Type,M,F,A,PI):- 
  % \+ predicate_property(M:PI,inherited_from(_)), 
  predicate_property(M:PI,defined),
  do_decl_kb_type_2(Type,M,F,A,PI).
% not possible do_decl_kb_type_1(Type,M,F,A,PI):- predicate_property(M:PI,inherited_from(M)),!,do_decl_kb_type_2(Type,M,F,A,PI).

do_decl_kb_type_1(_Type,M,F,A,PI):- fail,
   findall(R,(current_predicate(F,R:PI), 
   \+ predicate_property(R:PI,inherited_from(_)),
   R\==M),Rs),Rs\==[],Rs\==[baseKB],
   dmsg(pfc(local_found_peer(Rs,M:F/A))),fail,
   !,
   show_call(pfc(found_peer(R)),do_import(M,R,F,A)).

do_decl_kb_type_1(Type,M,F,A,PI):- do_decl_kb_type_2(Type,M,F,A,PI),!.
  

do_decl_kb_type_2(Type,M,F,A,_PI):- 
 nop(dmsg((do_decl_kb_type(Type,M,F,A)))),
 must_det_l((
  Why = decl_kb_type(Type,M:F/A),
  make_as_dynamic(Why,M,F,A),
  ain(baseKB:mpred_prop(M,F,A,Type)),
  create_predicate_inheritance(Why,M,F,A),
  decl_wrapped(M,F,A,ereq))).

expand_globals(In,_):- notrace(( \+ callable(In);In\=(':'(_,_)))),!,fail.
expand_globals((P:-B),(PO:-B)):-!,expand_globals(P,PO).
expand_globals(P,Out):-
   (compound(P)->functor(P,F,A);F=P,A=0),
   expand_already_decl(P,F,A,Out).

expand_already_decl(P,F,A,Out):- lmcache:already_decl(kb_global,M,F,A),Out=':'(M,P),!.
expand_already_decl(P,F,A,Out):- lmcache:already_decl(kb_shared,M,F,A),Out=':'(M,P),!.


:- set_prolog_flag(predicate_inheritance,false).

:- multifile(system:goal_expansion/4).
:- dynamic(system:goal_expansion/4).
:- module_transparent(system:goal_expansion/4).
system:goal_expansion(In,P,Out,PO):- notrace( current_prolog_flag(predicate_inheritance,true)), expand_globals(In,Out),P=PO.

:- multifile(system:term_expansion/4).
:- dynamic(system:term_expansion/4).
:- module_transparent(system:term_expansion/4).
system:term_expansion(In,P,Out,PO):- notrace( current_prolog_flag(predicate_inheritance,true)), 
   % checks if asserting
   notrace((nb_current('$term', Term),In == Term ; (Term=(Head:-_),In == Head))),
   expand_globals(In,Out),P=PO.


:- fixup_exports.

