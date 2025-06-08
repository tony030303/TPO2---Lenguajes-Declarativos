:- use_module(library(pengines)).
:- use_module(library(http/http_server)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_files)).
:- use_module(library(http/http_cors)).
:- use_module(library(clpfd)).
:- use_module(library(http/http_pengines)).

:- set_setting(http:cors, [*]).

:- pengine_application(clp).
:- use_module(clp:library(clpfd)).

% Handlers
:- http_handler(root(.), http_reply_file('index.html', []), []).

% Predicado CLP(Z)
clp:solve(Params, Solution) :-
    Sum = Params.get(sum),
    Product = Params.get(product),
    [X, Y] ins 1..10,
    X + Y #= Sum,
    X * Y #= Product,
    label([X, Y]),
    Solution = [X, Y].

% Iniciar servidores
:- initialization start_server.

start_server :-
    attach_pengine_server,
    http_server([port(8080)]).
