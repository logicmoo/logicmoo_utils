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
:- module(butterfly,[bformat/1,bformat/2,bformat/3]).
%:- use_module(library(logicmoo/butterfly_console)).
:- thread_local(t_l:in_block_format/0).
pre_style:-!.
pre_style:- format("P;HTML|<style> pre {
    display: inline;
    margin: 0;
    white-space: pre-wrap;                 /* CSS3 browsers  */
    white-space: -moz-pre-wrap !important; /* 1999+ Mozilla  */
    white-space: -pre-wrap;                /* Opera 4 thru 6 */
    white-space: -o-pre-wrap;              /* Opera 7 and up */
    white-space: pre-wrap;                 /* CSS3 browsers  */
    word-wrap: break-word;                 /* IE 5.5+ and up */
}</style>P",[]).

mouse_over_span:- format('P;HTML|<p>Each word will be wrapped in a span.</p><p>A second paragraph here.</p>Word: <span id="word"></span>P',[]).

is_visible_output:- current_output(Out),stream_property(Out,buffer(line)),stream_property(Out,alias(_)).


block_format(G):- t_l:in_block_format,!,G.
block_format(G):- with_output_to(string(S),locally(t_l:in_block_format,G)),bformat(S).


is_butterfly_console:- getenv('COLORTERM',butterfly),!.
is_butterfly_console:- thread_self(X),atom(X),(atom_concat(_,'23',X);atom_concat(_,'01',X)),!.

bfly_fmt(P):- atom_concat(PL,'\n',P),!,bfly_fmt(PL).
bfly_fmt(P):- pre_style,format("P;HTML|~wP",[P]).

bformat(P):- t_l:in_block_format,!,format("~w",[P]),!.
bformat(P):- on_x_log_fail(httpd_wrapper:http_current_request(_)),!,format("~w",[P]).
%bformat(P):- is_visible_output,is_butterfly_console,format(string(S),'~w',[P]),atom_contains(S,'<'),!,bfly_fmt(S).
bformat(P):- format("~w",[P]),!.

bformat(Fmt,Args):- sformat(P,Fmt,Args),bformat(P).

bformat(Stream,Fmt,Args):- atomic(Stream),is_stream(Stream),!, with_output_to(Stream,bformat(Fmt,Args)).
bformat(Stream,Fmt,Args):- format(Stream,Fmt,Args).

hi_there:- bformat('<pre>hi</pre>'),bformat('<pre><font color=green size=+1>there<font></pre>'),!.
hi_there2:- hi_there,writeln(ok),hi_there,hi_there,write(ok),hi_there.

:- fixup_exports.

% % % % OFF :- system:use_module(logicmoo_startup).

