% mesatezapala.pl


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

%:- module(mesatezapala, [generar_solucion/2, verificar_solucion/2, main/0, mostrar_todas_soluciones/0]).

:- use_module(library(lists)).
:- use_module(library(clpz)).
:- use_module(library(pairs)).
:- use_module(library(ordsets)).



% Imprime una tabla formateada con separadores y valores
formatlist([]) :-
    format("|~`-t~15+|~`-t~`-t~15+|~`-t~`-t~15+|~`-t~`-t~15+|~`-t~`-t~15+|~`-t~15+|~n",[]).
formatlist([[A,B,C,D,E,F]|Y]) :-
    format("|~`-t~15+|~`-t~`-t~15+|~`-t~`-t~15+|~`-t~`-t~15+|~`-t~`-t~15+|~`-t~15+|~n",[]),
    format("|~w~t~15+|~w~t~15+|~w~t~15+|~w~t~15+|~w~t~15+|~w~t~15+|~n",[A,B,C,D,E,F]),
    formatlist(Y).


% Relación de "enfrente": diferencia absoluta 
% de 3 posiciones (mesa circular de 6 sillas)
enfrente(H,N) :-
    abs(H-N) #= 3.

%Relación de "al lado": diferencia de 1 posición
al_lado(H,N) :-
    abs(H-N) #= 1.

% Relación de "a la izquierda": H está  
% justo antes que N en sentido horario
izquierda(H,N) :-
    (H #= N - 1; N #= 1, H #= 6).

% solución del problema con restricciones impuestas
generar_solucion(Asientos, Categorias) :-
    % Se crean dos listas: una para posiciones de mujeres y otra para características
    Table = [Mujeres, Caracteristicas],

    Mujeres = [Jane, Ada, Katherine, _Marie, _Grace, Chien], % variables para posiciones de mujeres
    MuNames = [jane, ada, katherine, marie, grace, chien], % nombres asociados a las variables
    pairs_keys_values(PairsM, Mujeres, MuNames), % pares variable-nombre

    Caracteristicas = [Profesora, _Modesta, Odia_Polillas, Admira_Marie, Viajera, Duena_Casa], % variables para características
    CaracterisNames = [profesora, modesta, odia_polillas, admira_marie, viajera, duena_casa], % nombres de características
    pairs_keys_values(PairsC, Caracteristicas, CaracterisNames), % pares variable-característica

    % Convertir pares a listas de términos en_asiento/2 y tiene_caracteristica/2
    maplist(en_asiento, PairsM, Asientos),
    maplist(tiene_caracteristica, PairsC, Categorias),

    % todas las posiciones deben ser distintas
    maplist(all_distinct,Table), 

    % se aplanan las listas en una sola
    append(Table,Vs),

    % dominio de valores: 1 a 6 (6 asientos)
    Vs ins 1..6,

    % Restricciones          
    enfrente(Admira_Marie,Ada),
    enfrente(Profesora,Katherine),
    al_lado(Katherine,Viajera),
    al_lado(Katherine, Admira_Marie),
    enfrente(Odia_Polillas,Jane),
    al_lado(Odia_Polillas,Profesora),
    izquierda(Odia_Polillas,Admira_Marie),
    al_lado(Viajera,Katherine),
    al_lado(Viajera,Ada),
    enfrente(Ada,Admira_Marie),
    al_lado(Chien,Odia_Polillas),
    enfrente(Chien,Duena_Casa),
    % Etiquetar las variables de posición para obtener una solución completa
    label(Vs).


% Predicado para verificar una solución dada
verificar_solucion(Asientos, Categorias) :-
    Mujeres = [Jane, Ada, Katherine, Marie, Grace, Chien],
    Caracteristicas = [Profesora, Modesta, Odia_Polillas, Admira_Marie, Viajera, Duena_Casa],
    Table = [Mujeres, Caracteristicas],
    % Assign positions from Asientos
    member(en_asiento(jane, Jane), Asientos),
    member(en_asiento(ada, Ada), Asientos),
    member(en_asiento(katherine, Katherine), Asientos),
    member(en_asiento(marie, Marie), Asientos),
    member(en_asiento(grace, Grace), Asientos),
    member(en_asiento(chien, Chien), Asientos),
    all_distinct(Mujeres),
    % Assign characteristics from Categorias
    member(tiene_caracteristica(ProfName, profesora), Categorias),
    member(tiene_caracteristica(ModName, modesta), Categorias),
    member(tiene_caracteristica(OdiaName, odia_polillas), Categorias),
    member(tiene_caracteristica(AdmiraName, admira_marie), Categorias),
    member(tiene_caracteristica(ViajeraName, viajera), Categorias),
    member(tiene_caracteristica(DueniaName, duena_casa), Categorias),
    % Map names to positions
    member(en_asiento(ProfName, Profesora), Asientos),
    member(en_asiento(ModName, Modesta), Asientos),
    member(en_asiento(OdiaName, Odia_Polillas), Asientos),
    member(en_asiento(AdmiraName, Admira_Marie), Asientos),
    member(en_asiento(ViajeraName, Viajera), Asientos),
    member(en_asiento(DueniaName, Duena_Casa), Asientos),
    all_distinct(Caracteristicas),
    append(Table, Vs),
    Vs ins 1..6,
    % Apply constraints
    enfrente(Admira_Marie, Ada),
    enfrente(Profesora, Katherine),
    al_lado(Katherine, Viajera),
    al_lado(Katherine, Admira_Marie),
    enfrente(Odia_Polillas, Jane),
    al_lado(Odia_Polillas, Profesora),
    izquierda(Odia_Polillas, Admira_Marie),
    al_lado(Viajera, Katherine),
    al_lado(Viajera, Ada),
    enfrente(Ada, Admira_Marie),
    al_lado(Chien, Odia_Polillas),
    enfrente(Chien, Duena_Casa),
    !.

% Predicado principal para mostrar todas las soluciones
mostrar_todas_soluciones :-
    findall(Sol, (generar_solucion(_, Sol), label(Sol)), Soluciones),
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

% Predicados auxiliares para maplist
en_asiento(X-Y, en_asiento(X, Y)).
tiene_caracteristica(X-Y, tiene_caracteristica(X, Y)).

% Predicado main para encontrar e imprimir una solución
main :-
    generar_solucion(Pairs, Vs),
    label(Vs),
    maplist(list_to_ord_set, Pairs, M),
    formatlist(M).
