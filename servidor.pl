:- use_module(library(http/thread_httpd)). % Servidor HTTP
:- use_module(library(http/http_dispatch)).      % routing
:- use_module(library(http/http_client)). % Cliente HTTP
:- use_module(library(http/http_json)).          % Para manejar JSON en respuestas AJAX
:- use_module(library(http/html_write)).         % Para generar HTML
%:- consult('mesatezapala.pl').                   % Cargar el archivo mesatezapala.pl
:- use_module(mesatezapala).  % Importamos el módulo con las soluciones


%se inicia el servidor con "iniciar_servidor."


%RUTAS

% Ruta principal: http://localhost:8080/
:- http_handler(root(.), pagina_principal, []).
%Esto registra un manejador HTTP para la URL raíz del servidor (http://localhost:8001/).
    %root(.) indica que es la raíz (/) del servidor.
    %pagina_principal es el predicado que se ejecutará cuando alguien acceda a esa URL.
    %[] son las opciones (en este caso, ninguna).
:- http_handler(root(soluciones), mostrar_soluciones, []).


% Iniciar el servidor en el puerto 8001
iniciar_servidor :-
    http_server(http_dispatch, [port(8002)]).
    %http_server/2 inicia el servidor web de SWI-Prolog.
    %http_dispatch es el módulo que se encarga de redirigir las solicitudes a los manejadores definidos (como pagina_principal).
    %[port(8001)] indica que se usará el puerto 8001 (por lo que visitarás http://localhost:8001/).


% Manejador de la página principal
%Este predicado responde a la solicitud HTTP cuando se visita http://localhost:8001/.
pagina_principal(_Request) :-
    reply_html_page(
        title('Problema de la Mesa de Té'),
        [
            h1('Problema de la Mesa de Té'),
            p('Seis mujeres sentadas alrededor de una mesa con diferentes características.'),
            form([action='/soluciones', method='POST'],
                [
                    input([type=submit, value='Mostrar Soluciones'], [])
                ]),
            p('Haz clic en el botón para ver todas las posibles soluciones.')
        ]).

mostrar_soluciones(_Request) :-
    findall(Sol, (solution(_, Sol), label(Sol)), Soluciones),
    reply_html_page(
        title('Soluciones del Problema'),
        [
            h1('Todas las soluciones posibles'),
            p('A continuación se muestran todas las disposiciones posibles:'),
            pre(\mostrar_soluciones_formateadas(Soluciones)),
            a([href='/'], 'Volver al inicio')
        ]).

% Predicado para formatear las soluciones
mostrar_soluciones_formateadas([]) --> [].
mostrar_soluciones_formateadas([Sol|Sols]) --> 
    html([
        h2('Solución:'),
        pre(Sol),
        hr([])
    ]),
    mostrar_soluciones_formateadas(Sols).