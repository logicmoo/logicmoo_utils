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
:- module(logicmoo_util_common,[ /*add_history/1,add_history0/1,make_historial/2,
   normally/1,var_non_attvar/1,
   whenever_flag_permits/2,
   qsave_lm/0,qsave_lm/1,ignore_not_not/1, fixup_exports/0,
          all_source_file_predicates_are_transparent/0,
          all_source_file_predicates_are_transparent/2,
          all_source_file_predicates_are_exported/0,
          all_source_file_predicates_are_exported/2,
          add_file_search_path_safe/2,
          set_prolog_stack_gb/1,
          load_library_system/1,
          if_file_exists/1,
          add_file_search_path_safe/2,
          pack_upgrade/0,run_prologmud/0,init_logicmoo/0,
          shared_vars/3 */]).

:- reexport(library(logicmoo_util_startup)).


% :- use_module(logicmoo_util_startup).

