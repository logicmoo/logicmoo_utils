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

:- module(logicmoo_utils_all,[]).




:- if(gethostname(gitlab)).                                            

:- set_prolog_flag(runtime_debug,3).
:- set_prolog_flag(runtime_safety,3).
:- set_prolog_flag(runtime_speed,0).

:- else.

:- set_prolog_flag(runtime_debug,1).
:- set_prolog_flag(runtime_safety,1).
:- set_prolog_flag(runtime_speed,1).

:- endif.


:- set_prolog_flag(lm_no_autoload,false).
:- set_prolog_flag(lm_pfc_lean,false).


/*
:- set_prolog_flag(stack_limit, 32 000 000 000).
:- set_prolog_stack(global, limit(32*10**9)).
:- set_prolog_stack(local, limit(32*10**9)).
:- set_prolog_stack(trail, limit(32*10**9)).
*/


%:- setenv('DISPLAY', '').
:- use_module(library(plunit)).


% ==============================================
% Enable History
% ==============================================
:- if(\+ current_predicate(setup_hist0/0)).
:- if(exists_source(library(editline))). 
:- if(\+ current_prolog_flag(windows,true)).
:- use_module(library(editline)).
:- endif.
:- else.
:- if(exists_source(library(readline))).
:- use_module(library(readline)).
:- endif.
:- endif.
setup_hist0:-  '$toplevel':setup_history.
:- setup_hist0.
:- endif.
   
% ==============================================
% Add Pack Directories
% ==============================================
:- use_module(library(prolog_pack)).
:- multifile(user:file_search_path/2).
:-   dynamic(user:file_search_path/2).

dir_from(Rel,Y):-
    ((getenv('LOGICMOO_WS',Dir);
      prolog_load_context(directory,Dir);
      'w:/opt/logicmoo_workspace/'=Dir;      
      '~/logicmoo_workspace'=Dir;
      '/opt/logicmoo_workspace/'=Dir;
      fail)),
    absolute_file_name(Rel,Y,[relative_to(Dir),file_type(directory),file_errors(fail)]),
    exists_directory(Y),!.

:- export(add_pack_path/1).
add_pack_path(packs_xtra):-pack_property(logicmoo_nlu,_),!.
add_pack_path(packs_sys):-pack_property(logicmoo_base,_),!.
add_pack_path(Rel):- 
   dir_from(Rel,Y),
   (( \+ user:file_search_path(pack,Y)) ->asserta(user:file_search_path(pack,Y));true).



:- if( \+ exists_source(library(logicmoo_common))).
:- add_pack_path(packs_sys).
:- endif.

:- if( \+ exists_source(library(logicmoo_hyhtn))).
:- add_pack_path(packs_xtra).
:- endif.

%:- ignore(add_pack_path(packs_usr)).
%:- add_pack_path(packs_web).
%:- add_pack_path(packs_xtra).
%:- add_pack_path(packs_lib).

:- initialization(attach_packs,now).

update_packs:- !.
update_packs:-    
   use_module(library(prolog_pack)),
   (pack_property(prologmud_samples,version(Version));
    pack_property(pfc,version(Version))),!,
   use_module(library(git)),
   forall(
   (pack_property(Pack,version(Version)), pack_property(Pack,directory(Dir)),
      directory_file_path(Dir, '.git', GitDir),
      %(exists_file(GitDir);exists_directory(GitDir)),
       access_file(GitDir,read),
       access_file(GitDir,write)),
     ( print_message(informational, pack(git_fetch(Dir))),
     git([fetch], [ directory(Dir) ]),
     git_describe(V0, [ directory(Dir) ]),
     git_describe(V1, [ directory(Dir), commit('origin/master') ]),
     (   V0 == V1
     ->  print_message(informational, pack(up_to_date(Pack)))
     ;   true,
         git([merge, 'origin/master'], [ directory(Dir) ]),
         pack_rebuild(Pack)
     ))),
   initialization(attach_packs,now).

:- update_packs.

:- discontiguous logicmoo_utils_all:'$exported_op'/3.
:- system:reexport(library(logicmoo_common)).

% :- pack_list_installed.


% :- predicate_inheritance:kb_global(plunit:loading_unit/4).


