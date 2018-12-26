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

:- system:op(700,xfx,prolog:('univ_safe')).

:- reexport(library(logicmoo_startup)).
:- reexport(library(must_sanity)).
:- reexport(library(hook_database)).
:- reexport(library(logicmoo/misc_terms)).
:- reexport(library(logicmoo/each_call)).
:- reexport(library(logicmoo/redo_locally)).
:- reexport(library(logicmoo/no_loops)).
:- reexport(library(logicmoo/no_repeats)).



