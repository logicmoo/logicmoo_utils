% ===================================================================
% File 'logicmoo_util_ctx_frame.pl'
% Purpose: An Implementation in SWI-Prolog of Unwindable context frames
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'logicmoo_util_ctx_frame.pl' 1.0.0
% Revision:  $Revision: 1.1 $
% Revised At:   $Date: 2002/07/11 21:57:28 $
% ===================================================================
% ===================================================================
:- module(logicmoo_util_bb_frame, []).


push_frame(Info, Frame):- var(Frame), !, gensym(frame, F), Frame = [lbl(F)], push_frame(Info, Frame).
push_frame(Info, Frame):- do_eval_or_same(Info, BetterInfo), Info\=@=BetterInfo, push_frame(BetterInfo, Frame).
push_frame(Info, Frame):- member(Sub, Frame), Sub==Info, !.
push_frame([I1|I2], Frame):- !, push_frame(I1, Frame), push_frame(I2, Frame).
push_frame('&'(I1,I2), Frame):- !, push_frame(I1, Frame), push_frame(I2, Frame).
push_frame(Info, Frame):- Frame = [H|T], setarg(2, Frame, [H|T]), setarg(1, Frame, Info).


%  LocalContexts
%   They hold name-values in
%     -- assoc/1 lists
%     -- open tailed lists
%     -- frame/1 contains one or more of the above

% v/3s 
%  = v(Value,Setter,KeyDestructor)

% frame/3s
%  = frame(Named,Destructor,Ctx)

% well i played with a couple few differnt environment impls.. they have their pros cons.. one impl.. 
% that was unique is that an array of "binding pairs" live in an arraylist.. to be "in" an environment 
% it meant that you held an "index" into the arry list that as you went backwards you'd find your bindings.. each symbol had a java ftInt field "lastBindingIndex" 
% .. that was a "hint" to where you could fastforward the backwards search .. end named binding context also had a "index" to when you leave a named block.. 
% you could quickly reset the top of an index.
% File: /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/util/logicmoo_util_ctx_frame.pl

do_eval_or_same(G, GG):- \+ compound(G), !, GG=G.
do_eval_or_same([G1|G2], [GG1|GG2]):- !, do_eval_or_same(G1, GG1), do_eval_or_same(G2, GG2).
do_eval_or_same({O}, {O}):- !.
do_eval_or_same(G, GG):- compound_name_arguments(G, HT, [F|GL]), atom(F), member(HT, [t, h]), !,
 compound_name_arguments(GM, F, GL), !, do_eval_or_same(GM, GG).

do_eval_or_same(textString(P, G), textString(P, GG)):- ground(G), must(to_string_lc(G, GG)), !.
/*
do_eval_or_same(PEG, PEGG):- xnotrace((compound_name_arguments(PEG, F, Args), downcase_atom(F, D), (atom_concat(_, 'text', D);atom_concat(_, 'string', D)),
  append(Left, [G], Args))), ground(G), \+ string(G), !, must(to_string_lc(G, GG)), !,
  append(Left, [GG], NewArgs), compound_name_arguments(PEGG, F, NewArgs).
*/
do_eval_or_same(iza(P, G), Out):- !, do_eval_or_same(isa(P, G), Out). 
do_eval_or_same(isa(P, G), isa(P, GG)):- ground(G), !, must(asCol(G, GG)), !.

do_eval_or_same(xfn(P, G), GG):- !, must( call(P, G, GG)), !.
do_eval_or_same(G, GG):- compound_name_arguments(G, F, GL), F\==percept_props, !,
 maplist(do_eval_or_same, GL, GGL), !, compound_name_arguments(GG, F, GGL).
do_eval_or_same(G, G).

frame_var(_, Frame, _):- \+ compound(Frame), !, fail.
frame_var(Name, Frame, Var):- nonvar(Var), !, frame_var(Name, Frame, NewVar), !, NewVar=Var.
frame_var(Name, Frame, Var):- compound(Name), !, arg(_, Name, E), frame_var(E, Frame, Var), !.
frame_var(Name, [Frame1|Frame2], Var):- !, frame_var(Name, Frame1, Var);frame_var(Name, Frame2, Var).
frame_var(Name, Prop = Var, Var):- !, same_name(Name, Prop).
frame_var(Name, f(Pred, 1, [Var]), Var):- !, same_name(Name, Pred).
frame_var(Name, f(_, _, [Prop|List]), Var):- !, same_name(Name, Prop), last(List, Var).
frame_var(Name, Frame, Var):- compound_name_arity(Frame, Pred, Arity), Arity > 0, compound_name_arguments(Frame, Pred, List),
  frame_var(Name, f(Pred, Arity, List), Var).
frame_var(Name, Frame, Var):- arg(_, Frame, E), frame_var(Name, E, Var), !.

asCol(A, A):- var(A), !.
asCol(A, 'TypeFn'(A)):- \+ callable(A), !.
asCol(A, S):- format(atom(S), '~w', [A]).

to_upcase_name(V, V):- var(V), !.
to_upcase_name(T, N):- compound(T), !, compound_name_arity(T, A, _), !, to_upcase_name(A, N).
to_upcase_name(T, N):- format(atom(A), '~w', [T]), upcase_atom(A, N).



same_name(T1, T2):- ground(T1), ground(T2), to_upcase_name(T1, N1), to_upcase_name(T2, N2), !, N1==N2.



%frame_to_asserts(List, cmdFrame(Frame)):- is_list(List), sort(List, ListR), list_to_conjuncts('&', ListR, Frame), !.
%frame_to_asserts(Frame, cmdFrame(Frame)).
frame_to_asserts(Frame, Frame).

frame_defaults([], _Frame):-!.
frame_defaults([FrameArg| FrameArgS], Frame):-
   ignore((
     member(var(NewArg), FrameArg), var(NewArg),
     member(default(D), FrameArg),
     debug_var(D, NewArg),
    % D=NewArg,
   !)),
   frame_defaults(FrameArgS, Frame).

subst_into_list([], []).
subst_into_list(+(AB), [optional(true)|AABB]):- !, subst_into_list(AB, AABB), !.
subst_into_list(A+B, AABB):-!, subst_into_list(A, AA), subst_into_list(B, BB), append(AA, BB, AABB).
subst_into_list([A|B], AABB):-!, subst_into_list(A, AA), subst_into_list(B, BB), append(AA, BB, AABB).
subst_into_list(A, [A]):-!.

fix_frame_args([], []).
fix_frame_args([LastArg, []], BetterFrameArgS):- !, fix_frame_args([LastArg], BetterFrameArgS).
fix_frame_args([FrameArg| FrameArgS], [[slot(Slot)|FrameArgL]|BetterFrameArgS]):-
  subst_into_list(FrameArg, FrameArgL),
  ignore(member(var(NewArg), FrameArgL)),
  ignore((member(default(Name), FrameArgL), functor(Name, F, _), debug_var(F, NewArg), debug_var(F, Slot))),
  fix_frame_args(FrameArgS, BetterFrameArgS).

compute_frame_slots([], []).
compute_frame_slots([FrameArg| FrameArgS], [FrameSlot|FrameSlotS]):-
  frame_arg_to_slot(FrameArg, FrameSlot),
  compute_frame_slots(FrameArgS, FrameSlotS).
compute_frame_slots([_FrameArg| FrameArgS], FrameSlotS):-
  compute_frame_slots(FrameArgS, FrameSlotS).

frame_arg_to_slot(FrameArg, Name=NewArg):-
   % \+ member(optional(true), FrameArg),
   (member(var(NewArg), FrameArg);member(slot(NewArg), FrameArg)), !,
   (member(pred(Name), FrameArg);member(prep(Name), FrameArg);member(default(Name), FrameArg)), !.


frmprint(Frame) :-
    %catch(make_pretty(I, O), _, I=O),
    guess_pretty(Frame),
    predsort(frcmp, Frame, FrameA),
    reverse(FrameA, FrameO),
    maplist(frmprint_e, FrameO).
frmprint_e(Frame) :- format('~N  ', []), fmt90(Frame).

sortDeref(P, PP):- \+ compound(P), !, P=PP.
%sortDeref(isa(X, Y), visa(X, Y)):-!.
sortDeref(~(P), PP):-!, sortDeref(P, PP).
sortDeref(P, PP):- arg(1, P, PP), compound(PP).
sortDeref(P, PP):- safe_functor(P, F, N), wrapper_funct_sortin(F), arg(N, P, E), !, sortDeref(E, PP).
sortDeref(P, P).


all_different_bindings([]):- !.
all_different_bindings([_]):- !.
all_different_bindings([X, Y]):- !, dif(X, Y).
all_different_bindings([X, Y, Z]):- !, dif(X, Y), dif(X, Z), dif(Z, Y).
all_different_bindings([X|Text]):- maplist(dif(X), Text), all_different_bindings(Text).

wrapper_funct_sortin(F):- arg(_, v(~, post, pre), F).
wrapper_funct_correction(F):- arg(_, v(~, post, normally, pre), F).

correct_normals(Nil, Nil):- Nil==[], !.
correct_normals(EOL, []):- EOL==end_of_list, !.
correct_normals(UNormals, Normals):- \+ compound(UNormals), !, [UNormals]=Normals.
correct_normals(~(PreU), Normals):- compound(PreU), PreU=pre(U), !, correct_normals(pre(~(U)), Normals).
correct_normals((U, UU), Normals):- !, correct_normals(U, UC), correct_normals(UU, UUC), !, append(UC, UUC, Normals).
correct_normals([U|UU], Normals):- !, correct_normals(U, UC), correct_normals(UU, UUC), !, append(UC, UUC, Normals).
correct_normals(P, Normals):- P=..[F, A1, A2|List], wrapper_funct_correction(F),
  P1=..[F, A1], P2=..[F, A2|List], !,
  correct_normals([P1|P2], Normals).
correct_normals(Normal, [Normal]).

frcmp(P1, P2, Cmp):- (\+ compound(P1) ; \+ compound(P2)), !, compare(P1, P2, Cmp).
frcmp(P1, P2, Cmp):- N=1, (arg(N, P1, A);arg(N, P2, A)), is_list(A), !, compare(P1, P2, Cmp).
frcmp(P2, P1, Cmp):- sortDeref(P1, PP1)->P1\=@=PP1, !, frcmp(P2, PP1, Cmp).
frcmp(P1, P2, Cmp):- sortDeref(P1, PP1)->P1\=@=PP1, !, frcmp(PP1, P2, Cmp).
frcmp(P1, P2, Cmp):- N=1, arg(N, P1, F1), arg(N, P2, F2), F1==F2, !, compare(P1, P2, Cmp).
frcmp(P1, P2, Cmp):- safe_functor(P1, F1, _), safe_functor(P2, F2, _), F1\==F2, compare(F1, F2, Cmp), Cmp \= (=), !.
frcmp(P1, P2, Cmp):- arg(N, P1, F1), arg(N, P2, F2), frcmp(F1, F2, Cmp), Cmp \= (=), !.
frcmp(P1, P2, Cmp):- compare(P1, P2, Cmp).
%reframed_call( Pred, Doer, [give, Object, to, Recipient], give(Doer, Object, Recipient), _Mem):- !.

:- fixup_exports.



