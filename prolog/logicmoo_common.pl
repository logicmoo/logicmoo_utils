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

:- module(logicmoo_common,[]).

:- op(700,xfx,prolog:('univ_safe')).
:- discontiguous logicmoo_utils_all:'$exported_op'/3.


:- system:reexport(library(logicmoo_startup)).

:- system:reexport(library(debuggery/first)).
:- system:reexport(library(logicmoo/util_strings)).
:- system:reexport(library(debuggery/dmsg)).
:- system:reexport(library(debuggery/rtrace)).
:- system:reexport(library(debuggery/bugger)).
:- system:reexport(library(debuggery/dumpst)).
:- system:reexport(library(debuggery/ucatch)).
:- system:reexport(library(debuggery/frames)).

:- system:reexport(library(logicmoo/call_from_module)).
:- system:reexport(library(hook_database)).
:- system:reexport(library(must_sanity)).
:- system:reexport(library(logicmoo/filesystem)).

:- system:reexport(library(logicmoo/misc_terms)).
:- system:reexport(library(logicmoo/lockable_vars)).
:- system:reexport(library(logicmoo/portray_vars)).
:- system:reexport(library(logicmoo/util_varnames)).

:- system:reexport(library(logicmoo/each_call)).
:- system:reexport(library(logicmoo/redo_locally)).
:- system:reexport(library(logicmoo/no_loops)).
:- system:reexport(library(logicmoo/no_repeats)).
:- system:reexport(library(logicmoo/subclause_expansion)).
:- system:reexport(library(xlisting)).


:- system:reexport(library(logicmoo/virtualize_source)).
:- system:reexport(library(file_scope)).
:- system:reexport(library(logicmoo/script_files)).
:- system:reexport(library(logicmoo/predicate_inheritance)).
%:- system:reexport(library(logicmoo/retry_undefined)).



:- system:reexport(library(logicmoo/clause_attvars)).
:- system:reexport(library(logicmoo/with_no_x)).
:- system:reexport(library(logicmoo/filestreams)).
:- system:reexport(library(logicmoo/filesystem)).
/*
:- system:reexport(library(logicmoo/util_dlist)).
:- system:reexport(library(logicmoo/attvar_reader)).
:- system:reexport(library(logicmoo/attvar_serializer)).
:- system:reexport(library(logicmoo/portray_vars)).
:- system:reexport(library(logicmoo/util_engines)).
:- system:reexport(library(logicmoo/util_ctx_frame)).
:- system:reexport(library(logicmoo/butterfly)).
:- system:reexport(library(logicmoo/toplevel_variable_names)).
:- system:reexport(library(logicmoo/util_bb_env)).
:- system:reexport(library(logicmoo/util_structs)).
:- system:reexport(library(logicmoo/util_dra)).
:- system:reexport(library(logicmoo/util_bb_gvar)).
% :- system:reexport(library(xlisting/xlisting_web)).
*/
