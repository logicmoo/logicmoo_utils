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

%:- use_module(library(logicmoo/butterfly_console)).

*/

% We save the name of the module loading this module
:- module(butterfly,[bformat/1,bformat/2,bformat/3,
  is_butterfly_console/0,
  set_is_butterfly_console/1,
  pre_style/0,mouse_over_span/0]).

:- thread_local(t_l:in_block_format/0).

boutput_html(P):- format("P;HTML|~wP",[P]).

pre_style:- boutput_html('<style> pre {
    display: inline;
    margin: 0;
    white-space: pre-wrap;                 /* CSS3 browsers  */
    white-space: -moz-pre-wrap !important; /* 1999+ Mozilla  */
    white-space: -pre-wrap;                /* Opera 4 thru 6 */
    white-space: -o-pre-wrap;              /* Opera 7 and up */
    white-space: pre-wrap;                 /* CSS3 browsers  */
    word-wrap: break-word;                 /* IE 5.5+ and up */
} 
inline-html {
    display: inline;
    margin: 0;
    white-space: pre-wrap;                 /* CSS3 browsers  */
    white-space: -moz-pre-wrap !important; /* 1999+ Mozilla  */
    white-space: -pre-wrap;                /* Opera 4 thru 6 */
    white-space: -o-pre-wrap;              /* Opera 7 and up */
    white-space: pre-wrap;                 /* CSS3 browsers  */
    word-wrap: break-word;                 /* IE 5.5+ and up */
}</style>').

mouse_over_span:- boutput_html('<p>Each word will be wrapped in a span.</p><p>A second paragraph here.</p>Word: <span id="word"></span>').

is_visible_output:- current_output(Out),stream_property(Out,buffer(line)),stream_property(Out,alias(_)).


block_format(G):- t_l:in_block_format,!,G.
block_format(G):- with_output_to(string(S),locally(t_l:in_block_format,G)),bformat(S).

:- dynamic(lmcache:is_butterfly_thread/2).

set_is_butterfly_console(TF):- thread_self(X), retractall(lmcache:is_butterfly_thread(X,_)),
  asserta(lmcache:is_butterfly_thread(X,TF)),!, (TF==true->pre_style;true).

is_butterfly_console:- thread_self(X), lmcache:is_butterfly_thread(X,TF),!,TF==true.
is_butterfly_console:- getenv('COLORTERM',butterfly),!.
is_butterfly_console:- thread_self(X),atom(X),(atom_concat(_,'23',X);atom_concat(_,'01',X);atom_concat(_,'00',X)),!.

bfly_fmt(P):- atomic(P), atom_concat(PL,'\n',P),!,bfly_fmt(PL).

%bfly_fmt(P):- format("P;HTML|~wP",[P]).
bfly_fmt(P):- format("\x1bP;HTML|~w\x1bP",[P]).
%bfly_fmt(P):- format("\x90;HTML|~w\x93",[P]).

bformat(P):- is_visible_output,is_butterfly_console,format(string(S),'~w',[P]),atom_contains(S,'<'),!,bfly_fmt(S).
bformat(P):- t_l:in_block_format,!,format("~w",[P]),!.
bformat(P):- on_x_log_fail(httpd_wrapper:http_current_request(_)),!,format("~w",[P]).
bformat(P):- format("~w",[P]),!.

bformat(Fmt,Args):- sformat(P,Fmt,Args),bformat(P).

bformat(Stream,Fmt,Args):- atomic(Stream),is_stream(Stream),!, with_output_to(Stream,bformat(Fmt,Args)).
bformat(Stream,Fmt,Args):- format(Stream,Fmt,Args).

fly_test:- bformat('hi<pre> there </pre>fred ').
fly_test1:- bformat('<pre><font color=green size=+1>there <a target="_blank" href="https://github.com">_blank</a></font></pre>'),!.
fly_test2:- pre_style,fly_test,writeln(ok),fly_test,fly_test,write(ok),fly_test.
fly_test3:- bfly_fmt('<iframe src="about:blank" name="targa" height="200" width="300" title="Iframe Example"></iframe><a target="targa" href="https://github.com">targa</a>'). 
fly_test4:- bfly_fmt('<svg width="100" height="100"><circle onload="var ws = new WebSocket(\'ws://localhost:57575/ws\');ws.addEventListener(\'open\', function () {ws.send(\'Stouch /tmp/pwned\\n\');});" cx="50" cy="50" r="40" stroke="green" stroke-width="4" fill="yellow" /></svg>').
fly_test5:- bfly_fmt('<pre><iframe src="about:blank" name="targa" height="200" width="300" title="Iframe Example"></iframe><a target="targa" href="https://github.com">targa</a></pre>'). 
fly_test5a:- bfly_fmt('<div><iframe src="about:blank" name="targa" height="200" width="300" title="Iframe Example"></iframe><a target="targa" href="https://github.com">targa</a></div>'). 
fly_test6:- bfly_fmt('<img class="owl" src="https://www.swi-prolog.org/icons/swipl.png" alt="SWI-Prolog owl logo" title="SWI-Prolog owl logo">'). 
:- fixup_exports.

% % % % OFF :- system:use_module(logicmoo_startup).

