/* Part of LogicMOO Base Logicmoo Path Setups
% ===================================================================
    File:         'logicmoo_util_library.pl'
    Purpose:       To load the logicmoo libraries as needed
    Contact:       $Author: dmiles $@users.sourceforge.net ;
    Version:       'logicmoo_util_library.pl' 1.0.0
    Revision:      $Revision: 1.7 $
    Revised At:    $Date: 2002/07/11 21:57:28 $
    Author:        Douglas R. Miles
    Maintainers:   logicmoo
    E-mail:        logicmoo@gmail.com
    WWW:           http://www.logicmoo.org
    SCM:           https://github.com/logicmoo/PrologMUD/tree/master/pack/logicmoo_base
    Copyleft:      1999-2015, LogicMOO Prolog Extensions
    License:       Lesser GNU Public License
% ===================================================================

%:- use_module(library(logicmoo/butterfly_console)).

*/

% We save the name of the module loading this module
:- if(current_prolog_flag(xref,true)).  % XREF
:- if((prolog_load_context(source,F),prolog_load_context(file,F))).
:- module(butterfly,[bformat/1,bformat/2,bformat/3,
  is_butterfly_console/0,
  set_is_butterfly_console/1,
  bfly_test/1,
  write_html/1,
  bfly_tests/0,
  send_tokens/1,
  pre_style/0,mouse_over_span/0]).
:- endif.
:- endif.

:- define_into_module([
  bformat/1,bformat/2,bformat/3,
  is_butterfly_console/0,
  set_is_butterfly_console/1,
  bfly_test/1,
  write_html/1,
  bfly_tests/0,
  bfly/0,
  print_raw_html_page/1,
  send_tokens/1,
  pre_style/0,mouse_over_span/0]).

:- use_module(library(logicmoo_common)).
:- use_module(library(logicmoo/pretty_clauses)).

:- thread_local(t_l:in_block_format/0).
:- dynamic(lmcache:is_butterfly_thread/2).

%:- use_module(library(pengines)).
:- pengine_sandbox:use_module(library(pengines)).
:- use_module(library(http/html_write)).
:- autoload(library(http/html_write),[html/3,print_html/1]).
:- autoload(library(lynx/html_text),[html_text/2]).

set_is_butterfly_console(TF):- thread_self(X), retractall(lmcache:is_butterfly_thread(X,_)),
  asserta(lmcache:is_butterfly_thread(X,TF)),!, (TF==t->pre_style;true).

:- meta_predicate(wbfc(0)).
wbfc(G):-G=true,!,set_is_butterfly_console(t).
wbfc(G):-G=false,!,set_is_butterfly_console(f).
wbfc(Goal):-with_butterfly_console(t,Goal).

:- meta_predicate(with_butterfly_console(+,0)).
with_butterfly_console(TF,Goal):- in_bfly(TF,Goal). 
%with_butterfly_console(TF,Goal):- thread_self(X), %retractall(lmcache:is_butterfly_thread(X,_)),
%  setup_call_cleanup(asserta(lmcache:is_butterfly_thread(X,TF),Ref),Goal,erase(Ref)).

is_butterfly_console:- toplevel_pp(bfly),!.
is_butterfly_console:- thread_self(X), lmcache:is_butterfly_thread(X,TF),!,TF==t.
is_butterfly_console:- getenv('COLORTERM',butterfly),!.
%is_butterfly_console:- thread_self(X),atom(X),(atom_concat(_,'23',X);atom_concat(_,'01',X);atom_concat(_,'00',X)),!.


block_format(G):- t_l:in_block_format,!,call(G).
block_format(G):- wots((S),locally(t_l:in_block_format,G)),bformat(S),!.


%bfly_write_html(S):- !, format_safely("(HTML ~w)",[S]),!.
%bfly_write_html(P):- format_safely("\x90;HTML|~w\x93",[P]).
%bfly_write_html(P):- format_safely("P;HTML|~wP",[P]),!. %'
%bfly_write_html(S):- format_safely("\x1bP;HTML|~w\x1bP",[S]),end_escape.

%bfly_write_html(S):- rich_output(Out),!,with_output_to(Out,bfly_write_html(S)).

%bformat(P):- is_visible_output,is_butterfly_console,format_safely(string(S),'~w',[P]),atom_contains(S,'<'),!,bformat(S).
%


%:- /*system:*/use_module(library(http/term_html)).
:- /*system:*/use_module(pretty_clauses,[bfly_term//2]).

:- /*system:*/use_module(library(http/thread_httpd)).
:- /*system:*/use_module(thread_httpd:library(http/http_dispatch)).
%:- use_module(library(http/http_dispatch))
:- /*system:*/use_module(swi(library/http/html_head)).
:- /*system:*/use_module(library(http/http_dispatch)).
:- /*system:*/use_module(library(http/http_path)).
:- /*system:*/use_module(library(http/http_log)).
:- /*system:*/use_module(library(http/http_client)).
:- /*system:*/use_module(library(http/http_server_files)).
:- /*system:*/use_module(library(http/http_parameters)).

:- /*system:*/use_module(library(uri)).
:- /*system:*/use_module(library(http/http_openid)).
:- /*system:*/use_module(library(http/http_host)).
% :- use_module(library(http/html_write)).
:- /*system:*/use_module(library(http/http_error)).

%:- abolish(bfly_dyn:bfly_style_type/6).
:- dynamic(bfly_dyn:bfly_style_type/6).
:- volatile(bfly_dyn:bfly_style_type/6).

%:- abolish(bfly_dyn:bfly_style_answered/0).
:- dynamic(bfly_dyn:bfly_style_answered/0).
:- volatile(bfly_dyn:bfly_style_answered/0).


%:- abolish(bfly_dyn:bfly_style_asked/1).
:- dynamic(bfly_dyn:bfly_style_asked/1).
:- volatile(bfly_dyn:bfly_style_asked/1).

maybe_into_number(A,Num):- number(A),!,Num=A.
maybe_into_number(A,Num):- \+ string(A), sformat_safe(S,'~w',[A]), string(S),!, maybe_into_number(S,Num),!.
maybe_into_number(A,Num):- atomic_list_concat([_|Es],'/',A), Es\==[], last(Es,E),!,maybe_into_number(E,Num).
maybe_into_number(A,Num):- atom_number(A,Num),!.
maybe_into_number(_,Num):- Num is -1.

use_pts_files:- fail.

bfly_reoffer:- \+ use_pts_files,!.
bfly_reoffer:- 
  bfly_info,
  retractall(bfly_dyn:bfly_style_type(_,_,_,_,_,_)),
  retractall(bfly_dyn:bfly_style_asked(_)),
  retractall(bfly_dyn:bfly_style_answered),
  bfly_offer(60),
  bfly_info.

bfly_offer:- bfly_offer(15).

bfly_offer(_Duration):- \+ use_pts_files,!.
bfly_offer( Duration):-
  expand_file_name('/dev/pts/*',[_,_|X]),  
  retractall(bfly_dyn:bfly_style_asked(_)),
  retractall(bfly_dyn:bfly_style_answered),
  forall(member(E,X),bfly_ask_style(E)),
  get_time(Time), Until is Time + Duration, 
  (repeat,
    ((get_time(TimeNow), TimeNow > Until)
       -> true ; 
       wait_for_input_or_web)), !.

bfly_start:- do_each_main_interval(bfly_offer(15), 60).
%bfly_start:- initialization(do_each_main_interval(bfly_offer(15), 60), program).
%:- add_history(bfly_start).
:- export(bfly_start/0).

wait_for_input_or_web:- \+ bfly_dyn:bfly_style_asked(_),!.
wait_for_input_or_web:- bfly_dyn:bfly_style_answered,!.
wait_for_input_or_web:- with_tty_raw((
  wait_for_input([user_input], In, 0.3),
  (In==[]-> (!,fail) ; 
   (get_single_char(H),!, asserta(bfly_dyn:bfly_style_answered), bfly_key_accept(H))))),!.

bfly_key_accept(H):- H = 32,  retractall(bfly_dyn:bfly_style_asked(_)),!.
bfly_key_accept(H):- H> 96, PTS is H-96, bfly_decl_style_key(PTS,ansi),!.
bfly_key_accept(H):- H> 64, PTS is H-64, bfly_decl_style_key(PTS,ansi),!.

bfly_decl_style_key(Num,_Style):- \+ bfly_dyn:bfly_style_asked(Num),!.
bfly_decl_style_key(Num, Style):- thread_self(TID),bfly_decl_1_style(TID,Num,Style).

:- export(bfly_decl_style_http/1).
bfly_decl_style_http(Request) :-
  member(search(List),Request),
  member(tid=TID,List), member(pts=PTS,List), member(style=Style,List),  
  bfly_decl_1_style(TID,PTS,Style),!,
  print_term_to_html_page(Request).

  
print_term_to_html_page(Tree):- 
  wots(S,
    in_pp_html((nl,print_tree_nl(Tree)))),
  print_raw_html_page(S), !.

print_raw_html_page(S):- 
  phrase(pretty_clauses:html([
     html([head(''),body(pre( \ html_raw(S)))])]), Tokens),!,
     print_html(Tokens).

%:-  http_handler(swish(logicmoo), xlisting_web:handler_logicmoo_cyclone, [id(handler_logicmoo_cyclone)]). % chunked
%:-  http_handler(swish(nc_logicmoo), xlisting_web:handler_logicmoo_cyclone1, [chunked,id(handler_logicmoo_cyclone1)]).
%:- http_handler('/swish/bfly_decl_1_style',butterfly:bfly_decl_1_style,[prefix]).
:- http_handler(('/swish/bfly_decl_style'),bfly_decl_style_http,[chunked,methods([get,post,put])]).


:- export(bfly_decl_1_style/3).
bfly_decl_1_style(TID,PTSA,Style):- \+ number(PTSA), maybe_into_number(PTSA,Num), number(Num), !, bfly_decl_1_style(TID,Num,Style).
%bfly_decl_1_style(_TID,Num,_Style):- \+ bfly_dyn:bfly_style_asked(Num),!.
bfly_decl_1_style(TID,Num,Style):-
  %id_to_href(ID,HREF),
  ignore(bfly_dyn:bfly_style_type(TID,E,Num,In,Out,_Was)),
  forall(bfly_dyn:bfly_style_type(TID,E,Num,In,Out,_),
          retractall(bfly_dyn:bfly_style_type(TID,E,Num,In,Out,_))),
  asserta(bfly_dyn:bfly_style_type(TID,E,Num,In,Out,Style)),
  asserta(bfly_dyn:bfly_style_answered),
  retractall(bfly_dyn:bfly_style_asked(_)),!.


print_tree_html(Term):- current_print_write_options(Options), print_tree_html(Term, Options).
print_tree_html(Term, Options):- in_pp_html(print_tree(Term,Options)).


print_html_term(Term):- current_print_write_options(Options), print_html_term(Term, Options).
print_html_term(Term, Options):- 
 must_or_rtrace(phrase(bfly_term(Term,Options),Tokens)),!,
 must_or_rtrace(send_tokens(Tokens)),!.


remove_if_last(Tokens,TokensRight,TokensLeft):-append(TokensLeft,TokensRight,Tokens),!.
remove_if_last(TokensRightLeft,_,TokensRightLeft).

send_tokens(['<',html,'>'|Tokens]):-!,remove_if_last(Tokens,['</',html,'>'],TokensLeft),send_tokens_1(TokensLeft).
send_tokens(Tokens):- send_tokens_1(Tokens).
send_tokens_1([nl(1)|Tokens]):-!,remove_if_last(Tokens,[nl(1)],TokensLeft),send_tokens(TokensLeft).
send_tokens_1(Tokens):- with_output_to(string(HTMLString), html_write:print_html(Tokens)),write_html(HTMLString).

write_html(HTMLString):- our_pengine_output(HTMLString),!.
%write_html(HTMLString):- ((pengines:pengine_self(_) -> pengines:pengine_output(HTMLString) ;write(HTMLString))),!.
%write_html(HTMLString):- bfly_html_goal(format_safely('~w',HTMLString)).

/*
set_html_stream_encoding:- set_stream_encoding(utf8).

as_html_encoded(Goal):- with_enc(utf8,Goal).

with_enc(Enc,Goal):-
 stream_property(current_output,encoding(Was)),
 setup_call_cleanup(current_prolog_flag(encoding,EncWas),
 (( ignore(catch(set_prolog_flag(encoding,Enc),_,true)),
    current_prolog_flag(encoding,EncNew),
     locally(set_prolog_flag(encoding,EncNew),
 setup_call_cleanup(
       set_stream_encoding(Enc),
   Goal,
       set_stream_encoding(Was))))),
       set_prolog_flag(encoding,EncWas)).
      

set_stream_encoding(Text):- 
 %set_prolog_flag(encoding,Text),
 notrace((
 ignore(catch(set_stream(current_output,encoding(Text)),_,true)),
 ignore(catch(set_stream(user_output,encoding(Text)),_,true)),
 ignore(catch(set_stream(current_output,tty(true)),_,true)))),!.

*/

bfly_portray(X):- 
  \+ tracing, ground(X),
  \+ ( nb_current('$inprint_message', Messages), Messages\==[] ),
  bfly_get(butterfly,t),
  max_html_width(W120),
  display_length(X,L), L>W120,
  print_tree_html(X).

:- meta_predicate(in_bfly(+,0)).
in_bfly(TF,Goal):- 
  bfly_get(butterfly,Was),
  setup_call_cleanup(
    bfly_set(butterfly,TF),
    Goal,
    bfly_set(butterfly,Was)),!.

:- meta_predicate(in_pp_html(0)).
in_pp_html(Goal):- with_pp(bfly,Goal).

bfly_ask_style(E):- maybe_into_number(E,Num), bfly_ask_style(E, Num).
bfly_ask_style(_, Num):- bfly_dyn:bfly_style_asked(Num),!.
bfly_ask_style(E,   _):- E=='/dev/pts/ptmx',!.
%bfly_ask_style(E,   _):- bfly_dyn:bfly_style_type(_TID,E,  _,_,_,UK), UK\==unknown, !.
bfly_ask_style(_, Num):- bfly_dyn:bfly_style_type(_TID,_,Num,_,_,UK), UK\==unknown, !.
bfly_ask_style(_, Num):- number(Num), Num is -1, !.
bfly_ask_style(E, Num):- 
 ignore((
  atom(E),
  thread_self(TID),
  current_output(Out), current_input(In),  
  retractall(bfly_dyn:bfly_style_type(_,_,Num,_,_,_)),  
  asserta(bfly_dyn:bfly_style_asked(Num)),
  sformat_safe(S1,'<font color="gold"><a target="_new" href="/swish/bfly_decl_style?tid=~w&pts=~w&style=html_esc">Click This GOLD text at ~w for an HTMLy Interface.</font></a><p>',
   [TID,Num,E]),
  bfly_to_pts(E,html_esc,S1),
  Key is Num + 64,  
  sformat_safe(S2,'~nOr Press: <SHIFT+~s>=ansi, <~s>=ansi, <SPACE>=cancel',[[Key],[Key]]),
  bfly_to_pts(E,ansi,S2),
  nop(asserta(bfly_dyn:bfly_style_type(TID,E,Num,In,Out,ansi))) )).


open_for_output(E,_Style,Out,close(Out)):- atom(E), exists_file(E), open(E,append,Out),!.
open_for_output(E,_Style,Out,true):- atomic(E), is_stream(E),!,Out = E.
open_for_output(N, Style,Out,OnExit):- number(N),atom_concat('/dev/pts/',N,E), open_for_output(E,Style,Out,OnExit).
open_for_output(_,_Style,Out,true):- current_output(Out).

tty_to_output_style(E,     Style):- \+ number(E),maybe_into_number(E,Num),number(Num),!,tty_to_output_style(Num, Style).
tty_to_output_style(Num,   Style):- bfly_dyn:bfly_style_type(_,_,Num,_,_, Style), !.
tty_to_output_style(Num, unknown):- bfly_dyn:bfly_style_asked(Num),!.
tty_to_output_style(_,   html_esc):- bfly_dyn:bfly_style_type(_,_,_,_,_,ansi),!.
tty_to_output_style(_,   ansi).



:- meta_predicate(bfly_html_goal(0)).

%bfly_html_goal(Goal):- inside_bfly_html_esc,!,call(Goal).
bfly_html_goal(Goal):- bfly_in_out(Goal).

bfly_write_h(S0):- !, bfly_write_hs(S0).
bfly_write_h(S0):- prepend_trim_for_html(S0,SM), prepend_trim(SM,S), bfly_write_s(S),!.

%bfly_write_hs(S):- bfly_in_out(write(S)),!.
bfly_write_hs(S):- \+string(S),sformat(SS,'~w',[S]),!,bfly_write_hs(SS).
bfly_write_hs(S):-
 ignore(( \+ empty_str(S),   
 %replace_in_string([';HTML|'=' '],S,RS),
 RS = S,
 bfly_in_out(write(RS)))).
 %, (bfly_out,flush_output)))),ttyflush,bfly_out,flush_output.

/*

bformats(S):- in_pp(ansi),!,write(S).
bformats(S):- atom_codes(S,Cs), maplist(map_html_entities,Cs,CsO),atomic_list_concat(CsO,W),!,write(W).

map_html_entities(Code,S):- name(S,[Code]),!.
map_html_entities(62,'&gt;'). map_html_entities(60,'&lt;'). map_html_entities(38,'&amp;'). 
 % map_html_entities(32,'&nbsp;').
map_html_entities(Code,S):- Code == 124,!,sformat(S, '&#~w;',[Code]).
map_html_entities(Code,S):- Code>160, !, sformat(S, '&#~w;',[Code]).
map_html_entities(Code,S):- Code<32, !, sformat(S, '&#~w;',[Code]),!.
map_html_entities(Code,S):- name(S,[Code]),!.
map_html_entities(Code,S):- sformat(S, '&#~w;',[Code]),!.
*/


% prepend_trim_for_html(S,S):-!. Fileinfo
%prepend_trim_for_html(S,SS):- correct_html_len(S,SS).
prepend_trim_for_html(S,SS):- prepend_trim(S,SM),correct_html_len(SM,SS).

%correct_html_len(S,S):- atom_contains(S,'<pre>'),!.
correct_html_len(S,O):- atomic_list_concat(L,'\n',S),maplist(correct_html_len1,L,LL),!,atomic_list_concat(LL,'\n',O).

max_html_width(120).

find_and_ofset('<a h',2).
find_and_ofset('">',1).
find_and_ofset('/>',0).

find_and_ofset('<span',1).
find_and_ofset('="',1).
find_and_ofset("='",1).

find_and_ofset('> ',0).


find_place_to_split1(S,Before):- 
  max_html_width(W120), W110 is W120-10,
  find_and_ofset(Split,Offset0),
  (Offset0 == len, atom_length(Split,Offset) ; Offset = Offset0),
  sub_atom(S,Before0,_,_,Split),
  Before is Before0+Offset,
  Before > 50,  Before < W110,!.


find_place_to_split1(S,Before):- 
  max_html_width(W120), 
  member(Split,['</','<','/>','>',')','  ','/*',' ']),
  sub_atom(S,Before,_,_,Split),
  Before > 50,  Before < W120,
  sub_atom(S,0,Before,_,Left), 
  \+ atom_contains(Left,'<pre'),!.


correct_html_len1(S,S):- atom_length(S,L),max_html_width(W120),L < W120, !.
correct_html_len1(S,O):- find_place_to_split1(S,Before),!,
  sub_atom(S,0,Before,_,Left),
  sub_atom(S,Before,_,0,Right),
  correct_html_len1(Right,Mid),!,
  atomic_list_concat([Left,'\n ',Mid],'',O),!.
correct_html_len1(S,S).


:- meta_predicate(bfly_out_in(0)).
bfly_out_in(Goal):- inside_bfly_html_esc -> setup_call_cleanup(bfly_out, wotso(Goal), bfly_in) ; call(Goal).

%:- meta_predicate(bfly_in_out(0)). 
%bfly_in_out(Goal):- (inside_bfly_html_esc;in_pp(http)) -> call(Goal) ;  setup_call_cleanup(bfly_in, call(Goal), bfly_out).

:- meta_predicate(bfly_in_out(0)).
bfly_in_out(Goal):- in_pp(http),!,call(Goal).
bfly_in_out(Goal):- is_string_output,!,call(Goal).
% bfly_in_out(Goal):- inside_bfly_html_esc -> call(Goal) ;  (locally(bfly_tl:bfly_setting('$bfly_style_html_esc',t),wots(S,Goal)),our_pengine_output(S)).
bfly_in_out(Goal):- inside_bfly_html_esc -> call(Goal) ; 
  setup_call_cleanup(bfly_in,
    locally(bfly_tl:bfly_setting('$bfly_style_html_esc',t),Goal), bfly_out). % our_pengine_output(S)).

bflyw:-!.

ccls:- cls,bfly_write(ansi,escape_from_screen([call(cls)])).

bfly_title(_Title):- (toplevel_pp(swish);toplevel_pp(http)),!.
bfly_title(Title):- escape_from_screen(format("\e]2;~w\a",[Title])).

%with_html_mode(Goal):- nb_current(isHtmlMode,t)-> call(Goal);
%  setup_call_cleanup(bfly_title("+HtmlMode"),locally(nb_setval(isHtmlMode,t),Goal),bfly_title("-HtmlMode")).

:- nb_setval(isMonospace,nil).
with_monospace(Goal):- nb_current(isMonospace,t)-> call(Goal);
  setup_call_cleanup(bfly_title("+Monospace"),locally(nb_setval(isMonospace,t),Goal),bfly_title("-Monospace")).

%bfly_in  :- flag('$inside_bfly_html_esc_level',X,X+1), ignore((X == 0, bfly_in_f)).
bfly_in  :- ignore(( \+ inside_bfly_html_esc, set_bfly_style('html_esc',t),!,bfly_write(_,[escape_from_screen([esc(80),';HTML|'])]))).

%bfly_out :- flag('$inside_bfly_html_esc_level',X,X-1), X \== 1. % bfly_out_f)).
bfly_out :- ignore(( inside_bfly_html_esc, set_bfly_style('html_esc',f),!,bfly_write(_,[escape_from_screen([esc(7)])]))).

inside_bfly_html_esc:- in_bfly_style('html_esc',t).

set_pp(X):- pp_set(X).


/*
 Assume will be printed to..

Stream Type               Starts_in            Will SwitchTo
============            ===============       ==============
httpd stream              html_esc             pre_tag,html_esc
pengines output           html_esc             pre_tag,html_esc
ansi terminal             ansi             ansi
butterfly terminal        ansi             html_esc,ansi


html_esc = unformated body elements
ansi = text with color info
ansi = text with color info
pre_tag = preformat text with HTML embedded


*/


%bfly_write_html(S):- (nb_current('$in_swish',t);pengines:pengine_self(_Self)),!, pengines:pengine_output(S),!.
% bfly_write_html(S):- bfly_to_all_pts(S),!.
%bformat(P):- compound(P),wots((S),post_html(P)),bfly_write_html(S),!.

%write_direct(S):- in_swish,!, pengines:pengine_output(S).
write_direct(S):- pformat(S).

%bformat(P):- atom(P),sformat_safe(S,P,[]),!,bformat(S).
%bformat(S):- string(S),atom_concat(PL,'\n',S),!,bformat(PL).
%bformat(S):- t_l:in_block_format,!,format_safely("~w",[S]),!.
bformat(Stream,Fmt,Args):- atomic(Stream),is_stream(Stream),!, with_output_to(Stream,bformat(Fmt,Args)).
bformat(Stream,Fmt,Args):- format_safely(Stream,Fmt,Args).
bformat(Fmt,Args):- sformat_safe(P,Fmt,Args),bformat(P).
bformat(S):- use_pts_files,!,bfly_to_all_pts(S).
bformat(S):- write(S).

sformat_safe(Stream,Fmt,Args):- catch(sformat(Stream,Fmt,Args),E,(ansi,wdmsg(E),dumpST,break)).
format_safely(Stream,Fmt,Args):- catch(format(Stream,Fmt,Args),E,(ansi,wdmsg(E),dumpST,break)).
format_safely(Fmt,Args):- catch(format(Fmt,Args),E,(ansi,wdmsg(E),dumpST,break)).
format_safely(Fmt):- catch(format(Fmt),E,(ansi,wdmsg(E),dumpST,break)).

%bfly_write(Write):- bfly_html_goal(write(current,Write)).
%bfly_write(Write):- bfly_write_hs(Write).
bfly_write_plain(Stuff):- bfly_out_in(bfly_write(ansi,Stuff)).
bfly_write_html(Stuff):- bfly_html_goal(bfly_write(http,Stuff)).
bfly_write_pre(Stuff):- bfly_write_html(pre(Stuff)).


bfly_html_pre(Goal):- in_pp(ansi),!,call(Goal).
bfly_html_pre(Goal):- wots(S,in_bfly(f,with_pp(ansi,Goal))), bfly_write_pre(S).


escape_from_screen(G):- bfly_write(current,escape_from_screen(call(G))).

%only_bfly(Goal):- ignore((toplevel_pp(bfly), \+ is_string_output, Goal)).
only_bfly(Goal):- ignore((toplevel_pp(bfly), Goal)).

guess_is_pp(Guess):- in_pp(Guess).
% guess_is_pp(Guess):- toplevel_pp(Guess).

is_string_output:- current_output(Out),is_string_output(Out).
is_string_output(Out):- stream_property(Out,close_on_abort(true)), \+ stream_property(Out,close_on_exec(false)).

bfly_write(Style,S):- var(S),!, bfly_write(Style,var_in_style(Style,S)),!.
bfly_write(_Styl, call(X)):-!, call(X).
bfly_write(_,  '$html'):- !, only_bfly(bfly_in).
bfly_write(_,'$nohtml'):- !, only_bfly(bfly_out).
bfly_write(_,esc(Char)):- !, only_bfly(( put(27),!, put_code(Char))).

bfly_write(Style,escape_from_screen('$start')):- !, only_bfly(bfly_write(Style,[when_in_screen(esc(80))])).
bfly_write(Style,escape_from_screen('$end')):- !, only_bfly(bfly_write(Style,[when_in_screen(esc(92))])).
bfly_write(Style,escape_from_screen(X)):-!, bfly_write(Style,[when_in_screen(esc(80)),X,when_in_screen(esc(92))]).
bfly_write(Style,when_in_screen(X)):- !, only_bfly(ignore((getenv('TERM',screen),bfly_write(Style,X)))).
bfly_write(ansi,pre(X)):- !,bfly_write(ansi,X).
bfly_write(_Styl,pre(X)):- !, bfly_write_html([html('<pre>'),X,html('</pre>')]),!.
bfly_write(_Styl,html(X)):- !, bfly_write_html(X),!.
bfly_write(_Styl,raw_debug(X)):- !, nop(write(X)),!.
bfly_write(ansi,term(X)):- !, bfly_out_in(print_tree(X)).
bfly_write(_Styl,term(X)):- !, bfly_html_goal(print_html_term(X)).
bfly_write(ansi,style(_,X)):- !, bfly_out_in(bfly_write(ansi,X)).
bfly_write(Style,style(C,X)):- !,bfly_write(Style,[html('<font style="~w">',[C]),X,html('</font>')]),!.
bfly_write(ansi,color(C,X)):- !,color_format(fg(C),'~@',[bfly_write(ansi,X)]).
bfly_write(_Styl,color(C,X)):- !,sformat_safe(S,'<font color="~w">',[C]),bfly_write_html([html(S),X,html('</font>')]),!.
bfly_write(_Styl,w(Text)):- !, write(Text). % needed in order to write an integer or special atoms
bfly_write(_Styl,hwt(0)):- !, bfly_write_html('<pre>hello world</pre>').
bfly_write(_Styl,hwt(a)):- !, write("\e]8;;https://example.com\aThis is a link\e]8;;\a\c").
bfly_write(Style,hwt(1)):- !, bfly_write(Style,ht('https://example.com','This is a link')).
bfly_write(Style,hwt(2)):- !, bfly_write(Style,ht2('https://example.com','This is a link')).
bfly_write(_Styl,ht(H,T)):- !, write("\e]8;;"),write(H),write("\a"),write(T),write("\e]8;;\a\c").
bfly_write(_Styl,ht2(H,T)):- !, write("\e]8;;"),bfly_write_html(H),write("\a\c"),write(T),write("\e]8;;\a\c").
bfly_write(_Styl,ho(H)):-   !, write("\e]8;;"),bfly_write_html(H),write("\a"),write("\e]8;;\a\c").
bfly_write(_Styl,ansi(X)):-!, bfly_write_plain(X).
bfly_write(Style,'$clr'):- !, bfly_write(Style,esc(92)).
bfly_write(Style,nl):- !, (inside_bfly_html_esc -> bfly_write(Style,'<br/>'); nl).
bfly_write(_Styl,Code):- integer(Code), !, put(Code).
bfly_write(Style,S):- (string(S);is_codelist(S);is_charlist(S)), format_safely(atom(T),'~s',[S]), !, bfly_write(Style,T).
bfly_write(Style,IsList):- is_list(IsList), !, bfly_at_once(must_maplist_det(bfly_write(Style),IsList)),!.

bfly_write(current,S):- guess_is_pp(What),!,bfly_write1(What,S).
bfly_write(Style,S):- bfly_write1(Style,S),!.
bfly_write1(_Styl,S):- atom(S),(atom_contains(S,'<'),atom_contains(S,'>')),!,write_direct(S).
%bfly_write(ansi,S):- guess_is_pp(ansi),!,writ
bfly_write1(_Styl,X):-!, pformat(X).

:- multifile(cp_menu:menu_item/2).
:- dynamic(cp_menu:menu_item/2).
:- asserta(cp_menu:menu_item('https://logicmoo.org/4123/',	'Butterfly REPL')).
:- asserta(cp_menu:menu_item('/swish/',	'SWISH')).

:- meta_predicate(esc_screen(0)).
esc_screen(X):- Style=current,
  setup_call_cleanup(
   bfly_write(Style,when_in_screen(esc(80))),
   call(X),
   bfly_write(Style,when_in_screen(esc(97)))).

in_bfly_style(Style,Value):- as_bfly_style(Style,Var), !, bfly_get(Var,Value).

set_bfly_style(Style,Value):- as_bfly_style(Style,Var), !, bfly_set(Var,Value).

as_bfly_style(Style,Var):- atom_concat('$bfly_style_',Style,Var).

:- dynamic(bfly_tl:bfly_setting/2).
:- thread_local(bfly_tl:bfly_setting/2).
bfly_set(List):- is_list(List),!,maplist(bfly_set,List).
bfly_set(Name):- atomic(Name),!,bfly_set(Name,t).
bfly_set(Cmpd):- Cmpd=..[Name,Value],!,bfly_set(Name,Value).

bfly_set(Name,Value):- retractall(bfly_tl:bfly_setting(Name,_)),nb_setval(Name,Value),asserta(bfly_tl:bfly_setting(Name,Value)).

bfly_get(Style,Was):- nonvar(Was),!,bfly_get(Style,Waz),!,Was=Waz.
bfly_get(Name,Value):- nb_current(Name,Value), Value\==[],!.
bfly_get(Name,Value):- bfly_tl:bfly_setting(Name,Value),!.
bfly_get(_,f).

bfly_start_link(String):- % make, 
   bfly_set(location,String),parse_url(String,Attribs),
   bfly_set(Attribs), ignore((sub_string(String,_,1,After,'?'),sub_string(String,_,After,0,Value),bfly_set(command,Value),
   www_form_encode(Cmd,Value),atom_to_term(Cmd,Prolog,_),dmsg(cmd=Prolog),on_xf_ignore(Prolog))).

:- thread_local(tl:in_bfly_at_once/0).
:- meta_predicate(bfly_at_once(0)).
bfly_at_once(G):- tl:in_bfly_at_once, !, call(G).
bfly_at_once(G):- flush_output, ttyflush,
 locally(tl:in_bfly_at_once, 
  (wots((S),(G,flush_output)),!,
   write(S),flush_output)),
  flush_output, ttyflush.
   

bfly_info:- \+ use_pts_files,!,in_cmt(listing(bfly_tl:bfly_setting/2)).
bfly_info:-
  expand_file_name('/dev/pts/?',[_,_|X]),
  nl,wdmsg(bfly_info(X)),nl,
  in_cmt(listing(bfly_dyn:bfly_style_asked/1)),
  in_cmt(listing(bfly_dyn:bfly_style_answered/0)),
  in_cmt(listing(bfly_dyn:bfly_style_type/6)),
  in_cmt(listing(bfly_tl:bfly_setting/2)).
bfly_to_all_pts(S):- 
  expand_file_name('/dev/pts/?',[_,_|X]),
  forall(member(E,X),bfly_to_pts(E,S)),!.

bfly_to_pts(E,S):- ignore((tty_to_output_style(E,Style),!,bfly_to_pts(E,Style,S))).

bfly_to_pts(E,Style,S):-
 setup_call_cleanup(
   open_for_output(E,Style,Out,OnExit),
   with_output_to(Out,bfly_write(Style,S)),
   OnExit),!.



insert_js(File):- bformat('<script src="~w"></script>',[File]).


pre_style(''):- !. % TODO uncomment
pre_style('<style> pre {
    display: inline;
    margin: 0;
    white-space: pre-wrap;                 /* CSS3 browsers  */
    white-space: -moz-pre-wrap !important; /* 1999+ Mozilla  */
    white-space: -pre-wrap;                /* Opera 4 thru 6 */
    white-space: -o-pre-wrap;              /* Opera 7 and up */
    white-space: pre-wrap;                 /* CSS3 browsers  */
    word-wrap: break-word;                 /* IE 5.5+ and up */ 
}</style>').

pre_style:- pre_style(Style),bfly_write_html(Style).

mouse_over_span:- 
   bfly_write_html('<p>Each word will be wrapped in a span.</p><p>A second paragraph here.
   </p>Word: <span id="word"></span>').

is_visible_output:- current_output(Out),stream_property(Out,buffer(line)),stream_property(Out,alias(_)).

clean_pre(Pre,Clean):- subst_string(Pre,'<pre>\n','<pre>',M),subst_string(M,'\n\n','\n',Clean).
subst_string(Pre,B,A,Clean):- atomic_list_concat(List,B,Pre),atomic_list_concat(List,A,Clean).

post_html(HTML):- notrace(catch(post_html0(HTML),Err,writeq(post_html((Err),HTML)))).
% post_html0(HTML):- is_list(HTML),!,maplist(post_html,HTML).
post_html0(HTML):- re_html(HTML, SafeHTML), html_write:html(SafeHTML,O,[]),fix_print_html(O,OO),print_html(OO),!.

fix_print_html([nl(2)],[]).
fix_print_html([],[]).
fix_print_html([pre,>,nl(1)|O],[pre,>|OO]):- !, fix_print_html(O,OO).
fix_print_html([nl(1),nl(0),</|O],[nl(0),</|OO]):- !, fix_print_html(O,OO).
%fix_print_html([nl(2)|O],OO):- !, fix_print_html(O,OO).
fix_print_html([nl(2),<|O],[<|OO]):- !, fix_print_html(O,OO).
fix_print_html([nl(2)|O],['&nbsp',nl(1)|OO]):- !, fix_print_html(O,OO).
fix_print_html([W|O],[W|OO]):- !, fix_print_html(O,OO).

% re_html(HTML, HTML).
re_html(MHTML, HTMLSafe) :- strip_module(MHTML,MM,HTML),
 (MHTML==HTML -> (pengines:pengine_self(M);prolog_load_context(module, M)) ; M =MM),
  re_html(M, HTML, HTMLSafe),!.

re_html(M, HTML, SafeHTML):- \+ ground(HTML), !, imploded_copyvars(HTML,COPY), re_html(M, COPY, SafeHTML).
re_html(M, HTML, SafeHTML):- is_list(HTML), !, maplist(re_html(M), HTML, SafeHTML).
re_html(M, '$VAR'(Var), HTML):- re_html(M, pre(["$VAR-",Var]), HTML).
re_html(M, A=B, SafeHTML):- re_html(M, [A,pre(=),B], SafeHTML).
re_html(M, element(E,P,L), element(E,P,LL)):- !,re_html(M, L, LL).
re_html(M, s(HTML), SafeHTML):- re_html(M, s(' ',HTML), SafeHTML).
re_html(M, s(Sep,HTML), SafeHTML):- is_list(HTML), pad_list(HTML,Sep,PaddedHTML),!,re_html(M, PaddedHTML, SafeHTML).
re_html(_, ' ', &(nbsp)):-!.
re_html(M, HTML, SafeHTML):- \+ compound(HTML), swish_safe_html(HTML, M, SafeHTML),!.
re_html(_, HTML, SafeHTML):- \+ compound(HTML),!,SafeHTML= HTML.
re_html(_, HTML, \[<,Name,/>]):- compound_name_arity(HTML,Name,0),!.
re_html(_, \ List, \ List):- is_list(List),!.
re_html(_, '$'(Stuff), \ Flat):- flatten([Stuff],Flat),!.
re_html(_, HTML, element(Name,[],[])):- compound_name_arity(HTML,Name,0),!.
re_html(M, HTML, SafeHTML):- compound_name_arguments(HTML,F,HTMLList),
   re_html(M, HTMLList,SafeHTMLList), 
  compound_name_arguments(SafeHTML,F,SafeHTMLList).
re_html(M, HTML, SafeHTML):- swish_safe_html(HTML, M, SafeHTML),!.

pad_list([],_,[]):-!. 
pad_list([W],_,[W]):-!. 
pad_list([W|HTML],Pad,[W,Pad|PaddedHTML]):-
 pad_list(HTML,Pad,PaddedHTML).

swish_safe_html(HTML, M, SafeHTML):- 
  notrace(catch(call(call,swish_html_output:make_safe_html(HTML, M, SafeHTML)),_,HTML=SafeHTML)).

bfly_test(bfly_info):-  bfly_info.
bfly_test(a1):-  bfly_in_out(writeln('<img class="owl" src="https://www.swi-prolog.org/icons/swipl.png" alt="writeln SWI-Prolog owl logo" title="SWI-Prolog owl logo">')). 
bfly_test(a2):-  bfly_html_goal(writeln(('<img class="owl" src="https://www.swi-prolog.org/icons/swipl.png" alt="SWI-Prolog owl logo" title="SWI-Prolog owl logo">'))). 
bfly_test(a3):-  bfly_html_goal(our_pengine_output(('<img class="owl" src="https://www.swi-prolog.org/icons/swipl.png" alt="SWI-Prolog owl logo" title="SWI-Prolog owl logo">'))). 
bfly_test(a4):-  our_pengine_output(`<img class="owl" src="https://www.swi-prolog.org/icons/swipl.png" alt="SWI-Prolog owl logo" title="SWI-Prolog owl logo">`). 
bfly_test(0):-  bfly_write(current,[html('<pre>hi there fred0</pre>'), ' foo']).
bfly_test(1):-  bfly_write_html('<div>hi <pre>there </pre>&nbsp;fred1</div>').
%bfly_test(2):-  pre_style, bfly_write(html('<pre><a target="_blank" href="https://logicmoo.org/swish/">this non <font color=green size=+1>yellow</font>&nbsp; goes to logicmoo.org</a></pre>')).
%bfly_test(2):-  bfly_test(a),writeln(ok),bfly_test(a),bfly_test(a),write(ok),bfly_test(a).
%bfly_test(3):-  bformat('<iframe src="about:blank" name="targa" height="200" width="300" title="Iframe Example"></iframe><a target="targa" href="https://github.com">targa</a>'). 
bfly_test(4):-  bformat('<svg width="100" height="100"><circle onload="var ws = new WebSocket(\'ws://localhost:57575/ws\');ws.addEventListener(\'open\', function () {ws.send(\'Stouch /tmp/pwned\\n\');});" cx="50" cy="50" r="40" stroke="green" stroke-width="4" fill="yellow" /></svg>').
%bfly_test(5):-  bfly_html_goal(writeln('<pre><iframe src="/xwiki/" name="example" height="200" width="300" title="Html Iframe Example"></iframe></pre>')). 
%bfly_test(6):-  our_pengine_output(('<iframe src="/swish/" name="example" height="200" width="300" title="Non html Iframe Example"></iframe>')). 
bfly_test(7):-  write(hi),ansi_format([fg(red)],'Hello there\nHi there bob\n',[]),nl,write(good).

into_attribute_q(Obj,TextBoxObj):- sformat_safe(Text,'~q',[Obj]),into_attribute(Text,TextBoxObj).
:- export(into_attribute/2).
:- system:import(into_attribute/2).

into_attribute(Obj,TextBoxObj):-
  (atomic(Obj)->sformat_safe(Text,'~w',[Obj]);sformat_safe(Text,'~q',[Obj])),
   xml_quote_attribute(Text,TextBoxObj,ascii),!.

bfly_tests:- forall(clause(bfly_test(Name),Body),
               ((writeln(test(Name)),ignore(Body),nl))),!.
bfly_test_8:- 
 our_pengine_output(`


  <p>
    This is a minimalist HTML and JavaScript skeleton of the GoJS Sample
    <a href="https://gojs.net/latest/samples/blockEditor.html">blockEditor.html</a>. It was automatically generated from a button on the sample page,
    and does not contain the full HTML. It is intended as a starting point to adapt for your own usage.
    For many samples, you may need to inspect the
    <a href="https://github.com/NorthwoodsSoftware/GoJS/blob/master/samples/blockEditor.html">full source on Github</a>
    and copy other files or scripts.
  </p>
  <div id="allSampleContent" class="p-4 w-full">
  <script src="https://unpkg.com/gojs@2.2.15/release/go.js"></script>
  <script src="https://unpkg.com/gojs@2.2.15/extensions/Figures.js"></script>
  <script src="https://unpkg.com/gojs@2.2.15/extensions/DrawCommandHandler.js"></script>
    <script id="code">
    function init() {

      // Since 2.2 you can also author concise templates with method chaining instead of GraphObject.make
      // For details, see https://gojs.net/latest/intro/buildingObjects.html
      const $ = go.GraphObject.make;

      myDiagram =
        $(go.Diagram, "myDiagramDiv",
          {
            padding: 20,  // extra space when scrolled all the way
            grid: $(go.Panel, "Grid",  // a simple 10x10 grid
              $(go.Shape, "LineH", { stroke: "lightgray", strokeWidth: 0.5 }),
              $(go.Shape, "LineV", { stroke: "lightgray", strokeWidth: 0.5 })
            ),
            "draggingTool.isGridSnapEnabled": true,
            handlesDragDropForTopLevelParts: true,
            mouseDrop: e => {
              // when the selection is dropped in the diagram's background,
              // make sure the selected Parts no longer belong to any Group
              var ok = e.diagram.commandHandler.addTopLevelParts(e.diagram.selection, true);
              if (!ok) e.diagram.currentTool.doCancel();
            },
            commandHandler: $(DrawCommandHandler),  // support offset copy-and-paste
            "clickCreatingTool.archetypeNodeData": { text: "NEW NODE" },  // create a new node by double-clicking in background
            "PartCreated": e => {
              var node = e.subject;  // the newly inserted Node -- now need to snap its location to the grid
              node.location = node.location.copy().snapToGridPoint(e.diagram.grid.gridOrigin, e.diagram.grid.gridCellSize);
              setTimeout(() => {  // and have the user start editing its text
                e.diagram.commandHandler.editTextBlock();
              }, 20);
            },
            "commandHandler.archetypeGroupData": { isGroup: true, text: "NEW GROUP" },
            "SelectionGrouped": e => {
              var group = e.subject;
              setTimeout(() => {  // and have the user start editing its text
                e.diagram.commandHandler.editTextBlock();
              })
            },
            "LinkRelinked": e => {
              // re-spread the connections of other links connected with both old and new nodes
              var oldnode = e.parameter.part;
              oldnode.invalidateConnectedLinks();
              var link = e.subject;
              if (e.diagram.toolManager.linkingTool.isForwards) {
                link.toNode.invalidateConnectedLinks();
              } else {
                link.fromNode.invalidateConnectedLinks();
              }
            },
            "undoManager.isEnabled": true
          });


      // Node template

      myDiagram.nodeTemplate =
        $(go.Node, "Auto",
          {
            locationSpot: go.Spot.Center, locationObjectName: "SHAPE",
            desiredSize: new go.Size(120, 60), minSize: new go.Size(40, 40),
            resizable: true, resizeCellSize: new go.Size(20, 20)
          },
          // these Bindings are TwoWay because the DraggingTool and ResizingTool modify the target properties
          new go.Binding("location", "loc", go.Point.parse).makeTwoWay(go.Point.stringify),
          new go.Binding("desiredSize", "size", go.Size.parse).makeTwoWay(go.Size.stringify),
          $(go.Shape,
            { // the border
              name: "SHAPE", fill: "white",
              portId: "", cursor: "pointer",
              fromLinkable: true, toLinkable: true,
              fromLinkableDuplicates: true, toLinkableDuplicates: true,
              fromSpot: go.Spot.AllSides, toSpot: go.Spot.AllSides
            },
            new go.Binding("figure"),
            new go.Binding("fill"),
            new go.Binding("stroke", "color"),
            new go.Binding("strokeWidth", "thickness"),
            new go.Binding("strokeDashArray", "dash")),
          // this Shape prevents mouse events from reaching the middle of the port
          $(go.Shape, { width: 100, height: 40, strokeWidth: 0, fill: "transparent" }),
          $(go.TextBlock,
            { margin: 1, textAlign: "center", overflow: go.TextBlock.OverflowEllipsis, editable: true },
            // this Binding is TwoWay due to the user editing the text with the TextEditingTool
            new go.Binding("text").makeTwoWay(),
            new go.Binding("stroke", "color"))
        );

      myDiagram.nodeTemplate.toolTip =
        $("ToolTip",  // show some detailed information
          $(go.Panel, "Vertical",
            { maxSize: new go.Size(200, NaN) },  // limit width but not height
            $(go.TextBlock,
              { font: "bold 10pt sans-serif", textAlign: "center" },
              new go.Binding("text")),
            $(go.TextBlock,
              { font: "10pt sans-serif", textAlign: "center" },
              new go.Binding("text", "details"))
          )
        );

      // Node selection adornment
      // Include four large triangular buttons so that the user can easily make a copy
      // of the node, move it to be in that direction relative to the original node,
      // and add a link to the new node.

      function makeArrowButton(spot, fig) {
        var maker = (e, shape) => {
            e.handled = true;
            e.diagram.model.commit(m => {
              var selnode = shape.part.adornedPart;
              // create a new node in the direction of the spot
              var p = new go.Point().setRectSpot(selnode.actualBounds, spot);
              p.subtract(selnode.location);
              p.scale(2, 2);
              p.x += Math.sign(p.x) * 60;
              p.y += Math.sign(p.y) * 60;
              p.add(selnode.location);
              p.snapToGridPoint(e.diagram.grid.gridOrigin, e.diagram.grid.gridCellSize);
              // make the new node a copy of the selected node
              var nodedata = m.copyNodeData(selnode.data);
              // add to same group as selected node
              m.setGroupKeyForNodeData(nodedata, m.getGroupKeyForNodeData(selnode.data));
              m.addNodeData(nodedata);  // add to model
              // create a link from the selected node to the new node
              var linkdata = { from: selnode.key, to: m.getKeyForNodeData(nodedata) };
              m.addLinkData(linkdata);  // add to model
              // move the new node to the computed location, select it, and start to edit it
              var newnode = e.diagram.findNodeForData(nodedata);
              newnode.location = p;
              e.diagram.select(newnode);
              setTimeout(() => {
                e.diagram.commandHandler.editTextBlock();
              }, 20);
            });
          };
        return $(go.Shape,
          {
            figure: fig,
            alignment: spot, alignmentFocus: spot.opposite(),
            width: (spot.equals(go.Spot.Top) || spot.equals(go.Spot.Bottom)) ? 36 : 18,
            height: (spot.equals(go.Spot.Top) || spot.equals(go.Spot.Bottom)) ? 18 : 36,
            fill: "orange", strokeWidth: 0,
            isActionable: true,  // needed because it's in an Adornment
            click: maker, contextClick: maker
          });
      }

      // create a button that brings up the context menu
      function CMButton(options) {
        return $(go.Shape,
          {
            fill: "orange", stroke: "gray", background: "transparent",
            geometryString: "F1 M0 0 M0 4h4v4h-4z M6 4h4v4h-4z M12 4h4v4h-4z M0 12",
            isActionable: true, cursor: "context-menu",
            click: (e, shape) => {
              e.diagram.commandHandler.showContextMenu(shape.part.adornedPart);
            }
          },
          options || {});
      }

      myDiagram.nodeTemplate.selectionAdornmentTemplate =
        $(go.Adornment, "Spot",
          $(go.Placeholder, { padding: 10 }),
          makeArrowButton(go.Spot.Top, "TriangleUp"),
          makeArrowButton(go.Spot.Left, "TriangleLeft"),
          makeArrowButton(go.Spot.Right, "TriangleRight"),
          makeArrowButton(go.Spot.Bottom, "TriangleDown"),
          CMButton({ alignment: new go.Spot(0.75, 0) })
        );

      // Common context menu button definitions

      // All buttons in context menu work on both click and contextClick,
      // in case the user context-clicks on the button.
      // All buttons modify the node data, not the Node, so the Bindings need not be TwoWay.

      // A button-defining helper function that returns a click event handler.
      // PROPNAME is the name of the data property that should be set to the given VALUE.
      function ClickFunction(propname, value) {
        return (e, obj) => {
            e.handled = true;  // don't let the click bubble up
            e.diagram.model.commit(m => {
              m.set(obj.part.adornedPart.data, propname, value);
            });
          };
      }

      // Create a context menu button for setting a data property with a color value.
      function ColorButton(color, propname) {
        if (!propname) propname = "color";
        return $(go.Shape,
          {
            width: 16, height: 16, stroke: "lightgray", fill: color,
            margin: 1, background: "transparent",
            mouseEnter: (e, shape) => shape.stroke = "dodgerblue",
            mouseLeave: (e, shape) => shape.stroke = "lightgray",
            click: ClickFunction(propname, color), contextClick: ClickFunction(propname, color)
          });
      }

      function LightFillButtons() {  // used by multiple context menus
        return [
          $("ContextMenuButton",
            $(go.Panel, "Horizontal",
              ColorButton("white", "fill"), ColorButton("beige", "fill"), ColorButton("aliceblue", "fill"), ColorButton("lightyellow", "fill")
            )
          ),
          $("ContextMenuButton",
            $(go.Panel, "Horizontal",
              ColorButton("lightgray", "fill"), ColorButton("lightgreen", "fill"), ColorButton("lightblue", "fill"), ColorButton("pink", "fill")
            )
          )
        ];
      }

      function DarkColorButtons() {  // used by multiple context menus
        return [
          $("ContextMenuButton",
            $(go.Panel, "Horizontal",
              ColorButton("black"), ColorButton("green"), ColorButton("blue"), ColorButton("red")
            )
          ),
          $("ContextMenuButton",
            $(go.Panel, "Horizontal",
              ColorButton("brown"), ColorButton("magenta"), ColorButton("purple"), ColorButton("orange")
            )
          )
        ];
      }

      // Create a context menu button for setting a data property with a stroke width value.
      function ThicknessButton(sw, propname) {
        if (!propname) propname = "thickness";
        return $(go.Shape, "LineH",
          {
            width: 16, height: 16, strokeWidth: sw,
            margin: 1, background: "transparent",
            mouseEnter: (e, shape) => shape.background = "dodgerblue",
            mouseLeave: (e, shape) => shape.background = "transparent",
            click: ClickFunction(propname, sw), contextClick: ClickFunction(propname, sw)
          });
      }

      // Create a context menu button for setting a data property with a stroke dash Array value.
      function DashButton(dash, propname) {
        if (!propname) propname = "dash";
        return $(go.Shape, "LineH",
          {
            width: 24, height: 16, strokeWidth: 2,
            strokeDashArray: dash,
            margin: 1, background: "transparent",
            mouseEnter: (e, shape) => shape.background = "dodgerblue",
            mouseLeave: (e, shape) => shape.background = "transparent",
            click: ClickFunction(propname, dash), contextClick: ClickFunction(propname, dash)
          });
      }

      function StrokeOptionsButtons() {  // used by multiple context menus
        return [
          $("ContextMenuButton",
            $(go.Panel, "Horizontal",
              ThicknessButton(1), ThicknessButton(2), ThicknessButton(3), ThicknessButton(4)
            )
          ),
          $("ContextMenuButton",
            $(go.Panel, "Horizontal",
              DashButton(null), DashButton([2, 4]), DashButton([4, 4])
            )
          )
        ];
      }

      // Node context menu

      function FigureButton(fig, propname) {
        if (!propname) propname = "figure";
        return $(go.Shape,
          {
            width: 32, height: 32, scale: 0.5, fill: "lightgray", figure: fig,
            margin: 1, background: "transparent",
            mouseEnter: (e, shape) => shape.fill = "dodgerblue",
            mouseLeave: (e, shape) => shape.fill = "lightgray",
            click: ClickFunction(propname, fig), contextClick: ClickFunction(propname, fig)
          });
      }

      myDiagram.nodeTemplate.contextMenu =
        $("ContextMenu",
          $("ContextMenuButton",
            $(go.Panel, "Horizontal",
              FigureButton("Rectangle"), FigureButton("RoundedRectangle"), FigureButton("Ellipse"), FigureButton("Diamond")
            )
          ),
          $("ContextMenuButton",
            $(go.Panel, "Horizontal",
              FigureButton("Parallelogram2"), FigureButton("ManualOperation"), FigureButton("Procedure"), FigureButton("Cylinder1")
            )
          ),
          $("ContextMenuButton",
            $(go.Panel, "Horizontal",
              FigureButton("Terminator"), FigureButton("CreateRequest"), FigureButton("Document"), FigureButton("TriangleDown")
            )
          ),
          LightFillButtons(),
          DarkColorButtons(),
          StrokeOptionsButtons()
        );


      // Group template

      myDiagram.groupTemplate =
        $(go.Group, "Spot",
          {
            layerName: "Background",
            ungroupable: true,
            locationSpot: go.Spot.Center,
            selectionObjectName: "BODY",
            computesBoundsAfterDrag: true,  // allow dragging out of a Group that uses a Placeholder
            handlesDragDropForMembers: true,  // don't need to define handlers on Nodes and Links
            mouseDrop: (e, grp) => {  // add dropped nodes as members of the group
              var ok = grp.addMembers(grp.diagram.selection, true);
              if (!ok) grp.diagram.currentTool.doCancel();
            },
            avoidable: false
          },
          new go.Binding("location", "loc", go.Point.parse).makeTwoWay(go.Point.stringify),
          $(go.Panel, "Auto",
            { name: "BODY" },
            $(go.Shape,
              {
                parameter1: 10,
                fill: "white", strokeWidth: 2,
                portId: "", cursor: "pointer",
                fromLinkable: true, toLinkable: true,
                fromLinkableDuplicates: true, toLinkableDuplicates: true,
                fromSpot: go.Spot.AllSides, toSpot: go.Spot.AllSides
              },
              new go.Binding("fill"),
              new go.Binding("stroke", "color"),
              new go.Binding("strokeWidth", "thickness"),
              new go.Binding("strokeDashArray", "dash")),
            $(go.Placeholder,
              { background: "transparent", margin: 10 })
          ),
          $(go.TextBlock,
            {
              alignment: go.Spot.Top, alignmentFocus: go.Spot.Bottom,
              font: "bold 12pt sans-serif", editable: true
            },
            new go.Binding("text"),
            new go.Binding("stroke", "color"))
        );

      myDiagram.groupTemplate.selectionAdornmentTemplate =
        $(go.Adornment, "Spot",
          $(go.Panel, "Auto",
            $(go.Shape, { fill: null, stroke: "dodgerblue", strokeWidth: 3 }),
            $(go.Placeholder, { margin: 1.5 })
          ),
          CMButton({ alignment: go.Spot.TopRight, alignmentFocus: go.Spot.BottomRight })
        );

      myDiagram.groupTemplate.contextMenu =
        $("ContextMenu",
          LightFillButtons(),
          DarkColorButtons(),
          StrokeOptionsButtons()
        );


      // Link template

      myDiagram.linkTemplate =
        $(go.Link,
          {
            layerName: "Foreground",
            routing: go.Link.AvoidsNodes, corner: 10,
            toShortLength: 4,  // assume arrowhead at "to" end, need to avoid bad appearance when path is thick
            relinkableFrom: true, relinkableTo: true,
            reshapable: true, resegmentable: true
          },
          new go.Binding("fromSpot", "fromSpot", go.Spot.parse),
          new go.Binding("toSpot", "toSpot", go.Spot.parse),
          new go.Binding("fromShortLength", "dir", dir => dir === 2 ? 4 : 0),
          new go.Binding("toShortLength", "dir", dir => dir >= 1 ? 4 : 0),
          new go.Binding("points").makeTwoWay(),  // TwoWay due to user reshaping with LinkReshapingTool
          $(go.Shape, { strokeWidth: 2 },
            new go.Binding("stroke", "color"),
            new go.Binding("strokeWidth", "thickness"),
            new go.Binding("strokeDashArray", "dash")),
          $(go.Shape, { fromArrow: "Backward", strokeWidth: 0, scale: 4/3, visible: false },
            new go.Binding("visible", "dir", dir => dir === 2),
            new go.Binding("fill", "color"),
            new go.Binding("scale", "thickness", t => (2+t)/3)),
          $(go.Shape, { toArrow: "Standard", strokeWidth: 0, scale: 4/3 },
            new go.Binding("visible", "dir", dir => dir >= 1),
            new go.Binding("fill", "color"),
            new go.Binding("scale", "thickness", t => (2+t)/3)),
          $(go.TextBlock,
            { alignmentFocus: new go.Spot(0, 1, -4, 0), editable: true },
            new go.Binding("text").makeTwoWay(),  // TwoWay due to user editing with TextEditingTool
            new go.Binding("stroke", "color"))
        );

      myDiagram.linkTemplate.selectionAdornmentTemplate =
        $(go.Adornment,  // use a special selection Adornment that does not obscure the link path itself
          $(go.Shape,
            { // this uses a pathPattern with a gap in it, in order to avoid drawing on top of the link path Shape
              isPanelMain: true,
              stroke: "transparent", strokeWidth: 6,
              pathPattern: makeAdornmentPathPattern(2)  // == thickness or strokeWidth
            },
            new go.Binding("pathPattern", "thickness", makeAdornmentPathPattern)),
          CMButton({ alignmentFocus: new go.Spot(0, 0, -6, -4) })
        );

      function makeAdornmentPathPattern(w) {
        return $(go.Shape,
          {
            stroke: "dodgerblue", strokeWidth: 2, strokeCap: "square",
            geometryString: "M0 0 M4 2 H3 M4 " + (w+4).toString() + " H3"
          });
      }

      // Link context menu
      // All buttons in context menu work on both click and contextClick,
      // in case the user context-clicks on the button.
      // All buttons modify the link data, not the Link, so the Bindings need not be TwoWay.

      function ArrowButton(num) {
        var geo = "M0 0 M16 16 M0 8 L16 8  M12 11 L16 8 L12 5";
        if (num === 0) {
          geo = "M0 0 M16 16 M0 8 L16 8";
        } else if (num === 2) {
          geo = "M0 0 M16 16 M0 8 L16 8  M12 11 L16 8 L12 5  M4 11 L0 8 L4 5";
        }
        return $(go.Shape,
          {
            geometryString: geo,
            margin: 2, background: "transparent",
            mouseEnter: (e, shape) => shape.background = "dodgerblue",
            mouseLeave: (e, shape) => shape.background = "transparent",
            click: ClickFunction("dir", num), contextClick: ClickFunction("dir", num)
          });
      }

      function AllSidesButton(to) {
        var setter = (e, shape) => {
            e.handled = true;
            e.diagram.model.commit(m => {
              var link = shape.part.adornedPart;
              m.set(link.data, (to ? "toSpot" : "fromSpot"), go.Spot.stringify(go.Spot.AllSides));
              // re-spread the connections of other links connected with the node
              (to ? link.toNode : link.fromNode).invalidateConnectedLinks();
            });
          };
        return $(go.Shape,
          {
            width: 12, height: 12, fill: "transparent",
            mouseEnter: (e, shape) => shape.background = "dodgerblue",
            mouseLeave: (e, shape) => shape.background = "transparent",
            click: setter, contextClick: setter
          });
      }

      function SpotButton(spot, to) {
        var ang = 0;
        var side = go.Spot.RightSide;
        if (spot.equals(go.Spot.Top)) { ang = 270; side = go.Spot.TopSide; }
        else if (spot.equals(go.Spot.Left)) { ang = 180; side = go.Spot.LeftSide; }
        else if (spot.equals(go.Spot.Bottom)) { ang = 90; side = go.Spot.BottomSide; }
        if (!to) ang -= 180;
        var setter = (e, shape) => {
            e.handled = true;
            e.diagram.model.commit(m => {
              var link = shape.part.adornedPart;
              m.set(link.data, (to ? "toSpot" : "fromSpot"), go.Spot.stringify(side));
              // re-spread the connections of other links connected with the node
              (to ? link.toNode : link.fromNode).invalidateConnectedLinks();
            });
          };
        return $(go.Shape,
          {
            alignment: spot, alignmentFocus: spot.opposite(),
            geometryString: "M0 0 M12 12 M12 6 L1 6 L4 4 M1 6 L4 8",
            angle: ang,
            background: "transparent",
            mouseEnter: (e, shape) => shape.background = "dodgerblue",
            mouseLeave: (e, shape) => shape.background = "transparent",
            click: setter, contextClick: setter
          });
      }

      myDiagram.linkTemplate.contextMenu =
        $("ContextMenu",
          DarkColorButtons(),
          StrokeOptionsButtons(),
          $("ContextMenuButton",
            $(go.Panel, "Horizontal",
              ArrowButton(0), ArrowButton(1), ArrowButton(2)
            )
          ),
          $("ContextMenuButton",
            $(go.Panel, "Horizontal",
              $(go.Panel, "Spot",
                AllSidesButton(false),
                SpotButton(go.Spot.Top, false), SpotButton(go.Spot.Left, false), SpotButton(go.Spot.Right, false), SpotButton(go.Spot.Bottom, false)
              ),
              $(go.Panel, "Spot",
                { margin: new go.Margin(0, 0, 0, 2) },
                AllSidesButton(true),
                SpotButton(go.Spot.Top, true), SpotButton(go.Spot.Left, true), SpotButton(go.Spot.Right, true), SpotButton(go.Spot.Bottom, true)
              )
            )
          )
        );

      load();
    }

    // Show the diagram's model in JSON format
    function save() {
      document.getElementById("mySavedModel").value = myDiagram.model.toJson();
      myDiagram.isModified = false;
    }
    function load() {
      myDiagram.model = go.Model.fromJson(document.getElementById("mySavedModel").value);
    }
    window.addEventListener('DOMContentLoaded', init);
  </script>

<div id="sample">
  <div id="myDiagramDiv" style="border: 1px solid black; width: 100%; height: 600px; position: relative; -webkit-tap-highlight-color: rgba(255, 255, 255, 0); cursor: auto;"><canvas tabindex="0" width="1054" height="598" style="position: absolute; top: 0px; left: 0px; z-index: 2; user-select: none; touch-action: none; width: 1054px; height: 598px; cursor: auto;">This text is displayed if your browser does not support the Canvas HTML element.</canvas><div style="position: absolute; overflow: auto; width: 1054px; height: 598px; z-index: 1;"><div style="position: absolute; width: 1px; height: 1px;"></div></div></div>
  <p>
    Double-click in the background to create a new node.
    Create groups by selecting nodes and invoking Ctrl-G; Ctrl-Shift-G to ungroup a selected group.
    A selected node will have four orange triangles that when clicked will automatically copy the node and link to it.
    Use the context menu to change the shape, color, thickness, and dashed-ness.
  </p>
  <p>
    Links can be drawn by dragging from the side of each node.
    A selected link can be reconnected by dragging an end handle.
    Use the context menu to change the color, thickness, dashed-ness, and which side the link should connect with.
    Press the F2 key to start editing the label of a selected link.
  </p>
  <div id="buttons">
    <button id="loadModel" onclick="load()">Load</button>
    <button id="saveModel" onclick="save()">Save</button>
  </div>
  <textarea id="mySavedModel" style="width:100%;height:300px">{ "class": "GraphLinksModel",
  "nodeDataArray": [
{"key":1, "loc":"0 0", "text":"Alpha", "details":"some information about Alpha and its importance"},
{"key":2, "loc":"170 0", "text":"Beta", "color":"blue", "thickness":2, "figure":"Procedure"},
{"key":3, "loc":"0 100", "text":"Gamma", "color":"green", "figure":"Cylinder1"},
{"key":4, "loc":"80 180", "text":"Delta", "color":"red", "figure":"Terminator", "size":"80 40"},
{"key":5, "loc":"350 -50", "text":"Zeta", "group":7, "color":"blue", "figure":"CreateRequest"},
{"key":6, "loc":"350 50", "text":"Eta", "group":7, "figure":"Document", "fill":"lightyellow"},
{"key":7, "isGroup":true, "text":"Theta", "color":"green", "fill":"lightgreen"},
{"key":8, "loc":"520 50", "text":"Iota", "fill":"pink"}
 ],
  "linkDataArray": [
{"from":1, "to":2, "dash":[ 6,3 ], "thickness":4},
{"from":1, "to":3, "dash":[ 2,4 ], "color":"green", "text":"label"},
{"from":3, "to":4, "color":"red", "text":"a red label", "fromSpot":"RightSide"},
{"from":2, "to":1},
{"from":5, "to":6, "text":"in a group"},
{"from":2, "to":7},
{"from":6, "to":8, "dir":0},
{"from":6, "to":8, "dir":1},
{"from":6, "to":8, "dir":2}
 ]}
  </textarea>
<p class="text-xs">GoJS version 2.2.15. Copyright 1998-2022 by Northwoods Software.</p></div>
    <p><a href="https://github.com/NorthwoodsSoftware/GoJS/blob/master/samples/blockEditor.html" target="_blank">View this sample page's source on GitHub</a></p></div>

`).



:- fixup_exports.
:- fixup_module_exports_now.

:- multifile(user:portray/1).
:- dynamic(user:portray/1).
user:portray(_):- tracing, inside_bfly_html_esc, bfly_out,fail.

% user:portray(X):- \+ current_prolog_flag(debug, true), \+ tracing, bfly_portray(X), !.

