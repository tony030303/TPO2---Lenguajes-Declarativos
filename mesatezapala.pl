%%Una mujer invitó recientemente a tomar el té a cinco personas. 
%%Los nombres de las seis mujeres que se sentaron alrededor de una 
%%mesa circular eran: Jane, Ada, Katherine, Marie, Grace y Chien. 
%%Una de ellas era profesora, otra era modesta, otra odiaba las polillas, 
%%otra admiraba a Marie, otra era viajera y otra era la dueña de casa.
%%La mujer que admiraba a Marie se sentó enfrente de la señora Ada.
%%La mujer que era profesora se sentó enfrente de la señora Katherine, 
%%quien a su vez se sentó entre la mujer que era viajera y la mujer que admiraba a Marie.
%%La mujer que odiaba a las polillas se sentó frente a la señora Jane, 
%%junto a la mujer que era profesora y a la izquierda de la que admiraba a Marie.
%%La mujer que era viajera se sentó entre la señora Katherine y la mujer que se 
%%sentó enfrente de la mujer que admiraba a Marie.
%%La señora Chien, que era buena amiga de todas, se sentó junto a la mujer odia 
%%las polillas y enfrente de la dueña de casa.


:- use_module(library(lists)).
:- use_module(library(clpfd)).
:- use_module(library(pairs)).
:- use_module(library(ordsets)).
:- use_module(library(http/html_write)).


% Predicado para mostrar soluciones en formato HTML
mostrar_solucion_html(Sol) :-
    Mujeres = ['Jane','Ada','Katherine','Marie','Grace','Chien'],
    Caracteristicas = ['Profesora','Modesta','OdiaPolillas','AdmiraMarie','Viajera','DueñaCasa'],
    
    % Crear filas de la tabla
    findall(
        tr([td(M), td(P), td(C)]),
        (nth1(P, Sol, _), nth1(P, Mujeres, M), nth1(P, Caracteristicas, C)),
        Filas
    ),
    
    % Crear tabla HTML completa
    reply_html_page(
        title('Solución Mesa de Té'),
        [
            h1('Disposición en la mesa'),
            table([border='1'],
                [tr([th('Mujer'), th('Posición'), th('Característica')]) | Filas]
            )
        ]).

formatlist([]) :-
    format("|~`-t~15+|~`-t~`-t~15+|~`-t~`-t~15+|~`-t~`-t~15+|~`-t~`-t~15+|~`-t~15+|~n",[]).
formatlist([[A,B,C,D,E,F]|Y]) :-
    format("|~`-t~15+|~`-t~`-t~15+|~`-t~`-t~15+|~`-t~`-t~15+|~`-t~`-t~15+|~`-t~15+|~n",[]),
    format("|~w~t~15+|~w~t~15+|~w~t~15+|~w~t~15+|~w~t~15+|~w~t~15+|~n",[A,B,C,D,E,F]),
    formatlist(Y).

enfrente(H,N) :-
    abs(H-N) #= 3.

al_lado(H,N) :-
    abs(H-N) #= 1.

izquierda(H,N) :-
    (
	N #= 0, H #= 6
    ;
    H #= N - 1
    ).


solution(Pairs,Vs) :-

    Table = [Mujeres,Caracteristicas],

    Mujeres = [Jane,Ada,Katherine,_Marie,_Grace,Chien],
    MuNames = [jane,ada,katherine,marie,grace,chien],
    pairs_keys_values(PairsM,Mujeres,MuNames),

    Caracteristicas = [Profesora,_Modesta,Odia_Polilla,Admira_Marie,Viajera,Duenia],
    CaracterisNames = [profesora,modesta,odia_Polilla,admira_Marie,viajera,duenia],
    pairs_keys_values(PairsC,Caracteristicas,CaracterisNames),

    Pairs = [PairsM,PairsC],

    maplist(all_distinct,Table),
    append(Table,Vs),
    Vs ins 1..6,
%% ESCRIBIR RESTRICCIONES         
    
    enfrente(Admira_Marie,Ada),
    enfrente(Profesora,Katherine),
    al_lado(Katherine,Viajera),
    al_lado(Katherine, Admira_Marie),
    enfrente(Odia_Polilla,Jane),
    al_lado(Odia_Polilla,Profesora),
    izquierda(Odia_Polilla,Admira_Marie),
    al_lado(Viajera,Katherine),
    al_lado(Viajera,Ada),
    enfrente(Ada,Admira_Marie),
    al_lado(Chien,Odia_Polilla),
    enfrente(Chien,Duenia).

main :- solution(Pairs,Vs), label(Vs),maplist(list_to_ord_set,Pairs,M),formatlist(M).

% Predicado principal para mostrar todas las soluciones
mostrar_todas_soluciones :-
    findall(Sol, (solution(_, Sol), label(Sol)), Soluciones),
    length(Soluciones, Count),
    format('Se encontraron ~d soluciones:~n~n', [Count]),
    mostrar_soluciones(Soluciones).

mostrar_soluciones([]).
mostrar_soluciones([Sol|Sols]) :-
    mostrar_solucion(Sol),
    nl,
    mostrar_soluciones(Sols).

mostrar_solucion(Sol) :-
    Mujeres = ['Jane','Ada','Katherine','Marie','Grace','Chien'],
    Caracteristicas = ['Profesora','Modesta','OdiaPolillas','AdmiraMarie','Viajera','DueñaCasa'],
    pairs_keys_values(PairsM, Sol, Mujeres),
    pairs_keys_values(PairsC, Sol, Caracteristicas),
    format('~nSolución:~n', []),
    format('Posiciones: ~w~n', [Sol]),
    format('Mujeres: ~w~n', [PairsM]),
    format('Características: ~w~n', [PairsC]).