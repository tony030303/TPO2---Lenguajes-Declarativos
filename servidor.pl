% servidort

:- use_module(library(http/thread_httpd)).       % Servidor HTTP
:- use_module(library(http/http_dispatch)).      % Routing
:- use_module(library(http/http_client)).        % Cliente HTTP
:- use_module(library(http/http_json)).          % Para manejar JSON
:- use_module(library(http/html_write)).         % Para generar HTML
:- use_module(mesatezapala).                     % Importamos el módulo mesatezapala

% Rutas
:- http_handler(root(.), pagina_principal, []).
:- http_handler(root(soluciones), mostrar_soluciones, []).
:- http_handler('/static', http_reply_from_files('static', []), [prefix]).

% Iniciar el servidor en el puerto 8002
iniciar_servidor :-
    http_server(http_dispatch, [port(8002)]).

% Manejador de la página principal
pagina_principal(_Request) :-
    reply_html_page(
        [title('Problema de la Mesa de Té'),
         meta([charset='UTF-8'], [])],  % Especificar codificación UTF-8
        [
            style([type='text/css'], '
                body {
                    display: flex;
                    flex-direction: column;
                    align-items: center;
                    justify-content: center;
                    text-align: center;
                    min-height: 100vh;
                    margin: 0;
                    font-family: Arial, sans-serif;
                }
                .container {
                    max-width: 800px;
                    padding: 20px;
                }
                h1 {
                    color: #2c3e50;
                }
                form {
                    margin: 20px 0;
                }
                input[type="submit"] {
                    background-color: #3498db;
                    color: white;
                    border: none;
                    padding: 10px 20px;
                    font-size: 16px;
                    cursor: pointer;
                    border-radius: 5px;
                }
                input[type="submit"]:hover {
                    background-color: #2980b9;
                }
                table {
                    border-collapse: collapse;
                    width: 100%;
                    margin-top: 20px;
                }
                th, td {
                    border: 1px solid #ddd;
                    padding: 8px;
                    text-align: center;
                }
                th {
                    background-color: #f2f2f2;
                }
            '),
            div([class='container'],
                [
                    h1('Problema de la Mesa de Té'),
                    p('Una mujer invitó recientemente a tomar el té a cinco personas. Los nombres de las seis mujeres que se sentaron alrededor de una mesa circular eran: Jane, Ada, Katherine, Marie, Grace y Chien.'),
                    form([action='/soluciones', method='POST'],
                        [
                            input([type=submit, value='Mostrar Soluciones'], [])
                        ]),
                    p('Haz clic en el botón para ver todas las posibles soluciones.')
                ])
        ]).

% Manejador para mostrar soluciones
mostrar_soluciones(_Request) :-
    findall(Sol, (solution(Pairs, Sol), label(Sol), maplist(list_to_ord_set, Pairs, _)), Soluciones),
    reply_html_page(
        [title('Soluciones del Problema'),
         meta([charset='UTF-8'], [])],  % Especificar codificación UTF-8
        [
            style([type='text/css'], '
                body {
                    font-family: Arial, sans-serif;
                    max-width: 800px;
                    margin: 0 auto;
                    padding: 20px;
                }
                table {
                    border-collapse: collapse;
                    width: 100%;
                    margin-top: 20px;
                }
                th, td {
                    border: 1px solid #ddd;
                    padding: 8px;
                    text-align: center;
                }
                th {
                    background-color: #f2f2f2;
                }
                a {
                    display: inline-block;
                    margin-top: 20px;
                    color: #3498db;
                    text-decoration: none;
                }
                a:hover {
                    text-decoration: underline;
                }
            '),
            h1('Todas las soluciones posibles'),
            p('A continuación se muestran todas las disposiciones posibles:'),
            \html_solutions(Soluciones),
            a([href='/'], 'Volver al inicio')
        ]).

% Generar tabla HTML para las soluciones
html_solutions([]) --> [].
html_solutions([Sol|Rest]) -->
    html([
        h2('Solución:'),
        table([
            tr([
                th('Jane'), th('Ada'), th('Katherine'),
                th('Marie'), th('Grace'), th('Chien')
            ]),
            tr([
                th('Profesora'), th('Modesta'), th('Odia Polillas'),
                th('Admira Marie'), th('Viajera'), th('Dueña')
            ]),
            \html_row(Sol)
        ]),
        hr([])
    ]),
    html_solutions(Rest).

% Generar una fila de la tabla
html_row([Mujeres, Caracteristicas]) -->
    { ord_intersection(Mujeres, [jane-J, ada-A, katherine-K, marie-M, grace-G, chien-C], _),
      ord_intersection(Caracteristicas, [profesora-P, modesta-Mo, odia_Polilla-O, admira_Marie-Am, viajera-V, duenia-D], _)
    },
    html([
        tr([td(J), td(A), td(K), td(M), td(G), td(C)]),
        tr([td(P), td(Mo), td(O), td(Am), td(V), td(D)])
    ]).