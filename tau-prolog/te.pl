% mesatezapala_tau.pl
% Predicados dinámicos
:- dynamic(en_asiento/2).
:- dynamic(tiene_caracteristica/2).

% Predicados auxiliares (reemplazo de library(lists))
% all_distinct/1: Verifica que todos los elementos de una lista sean distintos
all_distinct([]).
all_distinct([H|T]) :- \+ member(H, T), all_distinct(T).

% permutation/2: Genera permutaciones de una lista
permutation([], []).
permutation([X|Xs], Ys) :- permutation(Xs, Zs), select(X, Ys, Zs).

% select/3: Selecciona un elemento de una lista
select(X, [X|Xs], Xs).
select(X, [Y|Ys], [Y|Zs]) :- select(X, Ys, Zs).

% maplist/2: Aplica un predicado a cada elemento de una lista
maplist(_, []).
maplist(Pred, [X|Xs]) :- call(Pred, X), maplist(Pred, Xs).

% maplist/3: Aplica un predicado a pares de elementos de dos listas
maplist(_, [], []).
maplist(Pred, [X|Xs], [Y|Ys]) :- call(Pred, X, Y), maplist(Pred, Xs, Ys).

% member/2: Verifica si un elemento está en una lista
member(X, [X|_]).
member(X, [_|T]) :- member(X, T).

% arg/2: Extrae el argumento N de un término (simplificado para nuestro caso)
arg(N, Term, Arg) :- Term =.. [_|Args], nth1(N, Args, Arg).

% nth1/3: Obtiene el elemento en la posición N de una lista
nth1(1, [X|_], X).
nth1(N, [_|Xs], Y) :- N > 1, N1 is N - 1, nth1(N1, Xs, Y).

% Relación de "enfrente": diferencia absoluta de 3 posiciones
enfrente(H, N) :-
    integer(H), integer(N), % Asegura que H y N estén instanciados
    D is abs(H - N),
    D = 3.

% Relación de "al lado": diferencia de 1 posición
al_lado(H, N) :-
    integer(H), integer(N),
    ( D is abs(H - N), D = 1
    ; (H = 1, N = 6)
    ; (H = 6, N = 1)
    ).

% Relación de "a la izquierda": H está justo antes que N en sentido horario
izquierda(H, N) :-
    integer(H), integer(N),
    ( N = 1, H = 6
    ; H is N - 1
    ).

% Definición de las personas
persona(jane). persona(ada). persona(katherine).
persona(marie). persona(grace). persona(chien).

% Definición de las características
caracteristica(profesora). caracteristica(modesta). caracteristica(odia_polillas).
caracteristica(admira_marie). caracteristica(viajera). caracteristica(duena_casa).

% Mapeo de IDs de asientos a números
asiento_id_a_num(asiento1, 1).
asiento_id_a_num(asiento2, 2).
asiento_id_a_num(asiento3, 3).
asiento_id_a_num(asiento4, 4).
asiento_id_a_num(asiento5, 5).
asiento_id_a_num(asiento6, 6).

% Predicado para obtener la persona con una característica dada
persona_con_caracteristica(Persona, Caracteristica) :-
    tiene_caracteristica(Persona, Caracteristica).

% Predicado para obtener el asiento de una persona
asiento_de_persona(Persona, AsientoNum) :-
    en_asiento(Persona, AsientoID),
    asiento_id_a_num(AsientoID, AsientoNum).

% Pistas del problema
pista_1_ok :-
    persona_con_caracteristica(PersonaAdmiraMarie, admira_marie),
    asiento_de_persona(PersonaAdmiraMarie, AsientoAdmiraMarieNum),
    asiento_de_persona(ada, AsientoAdaNum),
    enfrente(AsientoAdmiraMarieNum, AsientoAdaNum).

pista_2_ok :-
    persona_con_caracteristica(PersonaProfesora, profesora),
    asiento_de_persona(PersonaProfesora, AsientoProfesoraNum),
    asiento_de_persona(katherine, AsientoKatherineNum),
    enfrente(AsientoProfesoraNum, AsientoKatherineNum).

pista_3_ok :-
    asiento_de_persona(katherine, AsientoKatherineNum),
    persona_con_caracteristica(PersonaViajera, viajera),
    persona_con_caracteristica(PersonaAdmiraMarie, admira_marie),
    asiento_de_persona(PersonaViajera, AsientoViajeraNum),
    asiento_de_persona(PersonaAdmiraMarie, AsientoAdmiraMarieNum),
    al_lado(AsientoKatherineNum, AsientoViajeraNum),
    al_lado(AsientoKatherineNum, AsientoAdmiraMarieNum),
    PersonaViajera \= PersonaAdmiraMarie.

pista_4_ok :-
    persona_con_caracteristica(PersonaOdiaPolillas, odia_polillas),
    asiento_de_persona(PersonaOdiaPolillas, AsientoOdiaPolillasNum),
    asiento_de_persona(jane, AsientoJaneNum),
    enfrente(AsientoOdiaPolillasNum, AsientoJaneNum).

pista_5_ok :-
    persona_con_caracteristica(PersonaOdiaPolillas, odia_polillas),
    persona_con_caracteristica(PersonaProfesora, profesora),
    persona_con_caracteristica(PersonaAdmiraMarie, admira_marie),
    asiento_de_persona(PersonaOdiaPolillas, AsientoOdiaPolillasNum),
    asiento_de_persona(PersonaProfesora, AsientoProfesoraNum),
    asiento_de_persona(PersonaAdmiraMarie, AsientoAdmiraMarieNum),
    al_lado(AsientoOdiaPolillasNum, AsientoProfesoraNum),
    izquierda(AsientoOdiaPolillasNum, AsientoAdmiraMarieNum).

pista_6_ok :-
    persona_con_caracteristica(PersonaViajera, viajera),
    asiento_de_persona(PersonaViajera, AsientoViajeraNum),
    asiento_de_persona(katherine, AsientoKatherineNum),
    al_lado(AsientoViajeraNum, AsientoKatherineNum).

pista_7_ok :-
    persona_con_caracteristica(PersonaViajera, viajera),
    asiento_de_persona(PersonaViajera, AsientoViajeraNum),
    asiento_de_persona(ada, AsientoAdaNum),
    al_lado(AsientoViajeraNum, AsientoAdaNum).

pista_8_ok :-
    asiento_de_persona(ada, AsientoAdaNum),
    persona_con_caracteristica(PersonaAdmiraMarie, admira_marie),
    asiento_de_persona(PersonaAdmiraMarie, AsientoAdmiraMarieNum),
    enfrente(AsientoAdaNum, AsientoAdmiraMarieNum).

pista_9_ok :-
    asiento_de_persona(chien, AsientoChienNum),
    persona_con_caracteristica(PersonaOdiaPolillas, odia_polillas),
    persona_con_caracteristica(PersonaDuenaCasa, duena_casa),
    asiento_de_persona(PersonaOdiaPolillas, AsientoOdiaPolillasNum),
    asiento_de_persona(PersonaDuenaCasa, AsientoDuenaCasaNum),
    al_lado(AsientoChienNum, AsientoOdiaPolillasNum),
    enfrente(AsientoChienNum, AsientoDuenaCasaNum).

% Verificar que todas las pistas se cumplen
verificar_solucion_completa :-
    pista_1_ok,
    pista_2_ok,
    pista_3_ok,
    pista_4_ok,
    pista_5_ok,
    pista_6_ok,
    pista_7_ok,
    pista_8_ok,
    pista_9_ok.

% Verificar unicidad de asignaciones de características
verificar_unicidad_asignaciones :-
    findall(P-C, tiene_caracteristica(P, C), Asignaciones),
    length(Asignaciones, 6),
    maplist(arg(1), Asignaciones, PersonasAsignadas),
    all_distinct(PersonasAsignadas),
    maplist(arg(2), Asignaciones, CaracteristicasAsignadas),
    all_distinct(CaracteristicasAsignadas).

% Verificar unicidad de asientos
verificar_unicidad_asientos :-
    findall(P-A, en_asiento(P, A), Ubicaciones),
    length(Ubicaciones, 6),
    maplist(arg(1), Ubicaciones, PersonasUbicadas),
    all_distinct(PersonasUbicadas),
    maplist(arg(2), Ubicaciones, AsientosOcupados),
    all_distinct(AsientosOcupados).

% Solución válida
solucion_valida :-
    verificar_unicidad_asignaciones,
    verificar_unicidad_asientos,
    verificar_solucion_completa.

% Generar hechos de asientos
generar_hechos_asientos(PersonasOrdenadas, AsientosIDs, HechosAsientos) :-
    maplist(construit_en_asiento, PersonasOrdenadas, AsientosIDs, HechosAsientos).

construit_en_asiento(Persona, AsientoID, en_asiento(Persona, AsientoID)).

% Generar hechos de características
generar_hechos_caracteristicas(Personas, CaracteristicasOrdenadas, HechosCaracteristicas) :-
    maplist(construit_tiene_caracteristica, Personas, CaracteristicasOrdenadas, HechosCaracteristicas).

construit_tiene_caracteristica(Persona, Caracteristica, tiene_caracteristica(Persona, Caracteristica)).

% Reemplazo de setup_call_cleanup/3
% Usamos un enfoque manual para afirmar y retractar hechos
generar_solucion(AsignacionesAsientos, AsignacionesCaracteristicas) :-
    Personas = [jane, ada, katherine, marie, grace, chien],
    AsientosIDs = [asiento1, asiento2, asiento3, asiento4, asiento5, asiento6],
    Caracteristicas = [profesora, modesta, odia_polillas, admira_marie, viajera, duena_casa],
    permutation(Personas, PermutacionAsientos),
    generar_hechos_asientos(PermutacionAsientos, AsientosIDs, AsignacionesAsientos),
    permutation(Caracteristicas, PermutacionCaracteristicas),
    generar_hechos_caracteristicas(Personas, PermutacionCaracteristicas, AsignacionesCaracteristicas),
    % Afirmar hechos
    maplist(assertz, AsignacionesAsientos),
    maplist(assertz, AsignacionesCaracteristicas),
    % Verificar solución
    ( solucion_valida ->
        true
    ;   % Si falla, retractar y continuar con backtracking
        maplist(retract, AsignacionesAsientos),
        maplist(retract, AsignacionesCaracteristicas),
        fail
    ),
    % Retractar hechos después de encontrar una solución
    maplist(retract, AsignacionesAsientos),
    maplist(retract, AsignacionesCaracteristicas).

% Predicado principal para mostrar la solución
main :-
    generar_solucion(AsignacionesAsientos, AsignacionesCaracteristicas),
    write('Asignaciones de asientos: '), write(AsignacionesAsientos), nl,
    write('Asignaciones de características: '), write(AsignacionesCaracteristicas), nl,
    fail.
main :- write('No hay más soluciones.'), nl.