:- module(pengines_server,
      [ server/1            % ?Port
      ]).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_path)).
:- use_module(library(http/http_authenticate)).
:- use_module(library(http/http_server_files)).
:- use_module(lib/admin/change_passwd).


:- multifile
    user:file_search_path/2,
    http:location/3.
:- dynamic
    user:file_search_path/2.

:- prolog_load_context(directory, Dir),
   asserta(user:file_search_path(app, Dir)).

user:file_search_path(www, app(www)).
user:file_search_path(apps, app(apps)).

http:location(apps, root(apps), []).

:- http_handler(apps(.), serve_files_in_directory(apps), [prefix]).
:- http_handler(root(.), serve_files_in_directory(www), [prefix]).

:- http_handler(/,
		http_redirect(moved_temporary, root(apps/scratchpad/'index.html')), []).



%%    server(?Port) is det.
%
%    Start the web-server on Port.

server(Port) :-
    check_passwd(passwd),
    http_server(http_dispatch,
		[ port(Port),
		  workers(16)
		]).
