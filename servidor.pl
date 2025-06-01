% servidor, interfaz web

:- use_module(library(http/thread_httpd)).       % Servidor HTTP
:- use_module(library(http/http_dispatch)).      % Routing
:- use_module(library(http/http_client)).        % Cliente HTTP
:- use_module(library(http/http_json)).          % Para manejar JSON
:- use_module(library(http/html_write)).         % Para generar HTML
:- use_module(mesatezapala).                     % Importamos el módulo mesatezapala
:- use_module(library(clpfd)).
% Rutas
:- http_handler(root(.), pagina_principal, []).
:- http_handler(root(soluciones), mostrar_soluciones, []).
:- http_handler('/static', http_reply_from_files('static', []), [prefix]).
:- http_handler(root(manual), pagina_manual, []).
:- http_handler(root(verificar), verificar_manual, []).

% Iniciar el servidor en el puerto 8002
iniciar_servidor :-
    http_server(http_dispatch, [port(8002)]).

% Manejador de la página principal
pagina_principal(_Request) :-
    reply_html_page(
        [title('Problema de la Mesa de Te'),
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
                    h1('Problema de la Mesa de Te'),
                    p('Una mujer invito recientemente a tomar el te a cinco personas. Los nombres de las seis mujeres que se sentaron alrededor de una mesa circular eran: Jane, Ada, Katherine, Marie, Grace y Chien.'),
                    form([action='/soluciones', method='POST'],
                        [
                            input([type=submit, value='Mostrar Soluciones'], [])
                        ]),
                    p('Haz clic en el boton para ver todas las posibles soluciones.'),
                    
                    p('Probar una combinacion manual'),
                    form([action='/manual', method='GET'],
                        [ 
                            input([type=submit, value='Ingresar combinacion manual'], []) 
                        
                        ])                
                ])
        ]).

% Manejador para mostrar soluciones
mostrar_soluciones(_Request) :-
    findall(Pairs, (solution(Pairs, Vs), label(Vs)), Soluciones),

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
            p('A continuacion se muestran todas las disposiciones posibles:'),
            \html_solutions(Soluciones),
            a([href='/'], 'Volver al inicio')
        ]).

% Generar tabla HTML para las soluciones
html_solutions([]) --> [].
html_solutions([Sol|Rest]) -->
    html([
        h2('Solucion:'),
        table([
            tr([th('Mujer 1'), th('Mujer 2'), th('Mujer 3'), th('Mujer 4'), th('Mujer 5'), th('Mujer 6')]),
            \html_row(Sol)
        ]),
        hr([])
    ]),
    html_solutions(Rest).

    html_cells([]) --> [].
html_cells([H|T]) --> 
    html(td(H)), 
    html_cells(T).    


    get_name_by_pos(Pairs, Pos, Name) :-
        member(Pos-Name, Pairs).
    
    html_row([PairsM, PairsC]) -->
        {
            findall(Nombre, (between(1,6,Pos), get_name_by_pos(PairsM, Pos, Nombre)), Mujeres),
            findall(Caracter, (between(1,6,Pos), get_name_by_pos(PairsC, Pos, Caracter)), Caracteristicas)
        },
        html([
            tr(\html_cells(Mujeres)),
            tr(\html_cells(Caracteristicas))
        ]).


%Página con formulario Manual para ingresar las combinaciones.

pagina_manual(_Request) :-
    reply_html_page(
        title('Ingresar combinacion manual'),
        [
            h1('Ingresar combinacion manual para la Mesa de Te'),
            form([action='/verificar', method='POST'], [
                table([
                    tr([th('Posicion'), th('Jane'), th('Ada'), th('Katherine'), th('Marie'), th('Grace'), th('Chien')]),
                    tr([th('Posicion'),
                        td(input([name=pos1_jane, type=number, min=1, max=6, required])),
                        td(input([name=pos1_ada, type=number, min=1, max=6, required])),
                        td(input([name=pos1_katherine, type=number, min=1, max=6, required])),
                        td(input([name=pos1_marie, type=number, min=1, max=6, required])),
                        td(input([name=pos1_grace, type=number, min=1, max=6, required])),
                        td(input([name=pos1_chien, type=number, min=1, max=6, required]))
                    ])
                ]),
                p('Caracteristicas (por posicion):'),
                table([
                    tr([th('Posicion'), th('Profesora'), th('Modesta'), th('Odia Polillas'), th('Admira Marie'), th('Viajera'), th('Duenia')]),
                    tr([th('Posicion'),
                        td(input([name=pos2_profesora, type=number, min=1, max=6, required])),
                        td(input([name=pos2_modesta, type=number, min=1, max=6, required])),
                        td(input([name=pos2_odia, type=number, min=1, max=6, required])),
                        td(input([name=pos2_admira, type=number, min=1, max=6, required])),
                        td(input([name=pos2_viajera, type=number, min=1, max=6, required])),
                        td(input([name=pos2_duenia, type=number, min=1, max=6, required]))
                    ])
                ]),
                input([type=submit, value='Verificar combinacion'], [])
            ]),
            a([href='/'], 'Volver al inicio')
        ]).

%Verificación del formulario ingresado por el usuario. Si esta todo OK, es solución. Sino, INVALIDO.
verificar_manual(Request) :-
    http_read_data(Request, Datos, []),
  % debug
    parse_mujeres(Datos, PairsM),
    parse_caracteristicas(Datos, PairsC),
    pairs_values(PairsM, VsM),
    pairs_values(PairsC, VsC),
    append(VsM, VsC, Vs),
    (   solution([PairsM, PairsC], Vs),
        label(Vs)
    ->  Resultado = valido
    ;   Resultado = invalido
    ),
    %  Construir contenido HTML dinámico aparte
    ( Resultado == valido
    ->  ContenidoResultado = [
            p('La combinacion es valida.'),
            \html_solutions([[PairsM, PairsC]])
        ]
    ;   ContenidoResultado = [
            p('La combinacion NO es valida segun las restricciones del problema.')
        ]
    ),
    %formo lista para replyHTML
    Extra = [
        a([href='/manual'], 'Probar otra combinación'),
        br([]),
        a([href='/'], 'Volver al inicio')
    ],
    append([ [h1('Resultado de la verificacion')], ContenidoResultado, Extra ], CuerpoHTML),
    reply_html_page(
        title('Verificacion de combinacion manual'),
        CuerpoHTML
    ).


parse_mujeres(Datos, PairsM) :-
    mujeres(Mujeres),
    findall(Pos-Name,
        ( member(Name, Mujeres),
          atom_concat('pos1_', Name, KeyAtom),
          atom_string(KeyAtom, Key),
          memberchk(Key=PosString, Datos),
          number_string(Pos, PosString)
        ),
        PairsM).

parse_caracteristicas(Datos, PairsC) :-
    caracteristicas(Caracteristicas),
    findall(Pos-Name,
        ( member(Name, Caracteristicas),
          atom_concat('pos2_', Name, KeyAtom),
          atom_string(KeyAtom, Key),
          memberchk(Key=PosString, Datos),
          number_string(Pos, PosString)
        ),
        PairsC).

mujeres([jane, ada, katherine, marie, grace, chien]).
caracteristicas([profesora, modesta, odia, admira, viajera, duenia]).
