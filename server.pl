:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_files)).

:- http_handler(root(.), http_reply_from_files('.', []), [prefix]). %ruta index.html

server(Port) :-
    http_server(http_dispatch, [port(Port)]).

inicio_server :-
    server(8888).

%iniciar servidor automaticamente

:- initialization inicio_server.

