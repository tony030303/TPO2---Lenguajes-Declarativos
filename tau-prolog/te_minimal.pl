% Cargar el módulo lists para usar all_distinct/1
:- use_module(library(lists)).

% Declarar predicados como dinámicos
:- dynamic(en_asiento/2).
:- dynamic(tiene_caracteristica/2).

% Predicado auxiliar para verificar que todos los elementos de una lista son distintos
all_distinct([]).
all_distinct([H|T]) :-
    \+ member(H, T),
    all_distinct(T).

% Relación de "enfrente": diferencia absoluta de 3 posiciones (mesa circular de 6 sillas)
enfrente(H,N) :-
    D is abs(H-N),
    D = 3.

% Relación de "al lado": diferencia de 1 posición
al_lado(H,N) :-
    ( D is abs(H-N), D = 1  
    ; (H=1, N=6)  % s1 al lado de s6
    ; (H=6, N=1)  % s6 al lado de s1
    ).

% Relación de "a la izquierda": H está justo antes que N en sentido horario
izquierda(H,N) :-
    ( N = 1, H = 6 % Si N es asiento1, H es asiento6
    ; H is N - 1  % Si N es 2, H es 1; Si N es 3, H es 2, etc.
    ).

% Definición de las personas
persona(jane). persona(ada). persona(katherine).
persona(marie). persona(grace). persona(chien).

% Definición de las características
caracteristica(profesora). caracteristica(modesta). caracteristica(odia_polillas).
caracteristica(admira_marie). caracteristica(viajera). caracteristica(duena_casa).

% Mapeo de IDs de asientos HTML a números
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

% --- Pistas del problema ---
% (Tus pistas originales están bien, asumiendo que en_asiento/2 y tiene_caracteristica/2
% están poblados con los hechos correctos)

% Pista 1: La mujer que admiraba a Marie se sentó enfrente de la señora Ada
pista_1_ok :-
    persona_con_caracteristica(PersonaAdmiraMarie, admira_marie),
    asiento_de_persona(PersonaAdmiraMarie, AsientoAdmiraMarieNum),
    asiento_de_persona(ada, AsientoAdaNum),
    enfrente(AsientoAdmiraMarieNum, AsientoAdaNum).

% Pista 2: La mujer que era profesora se sentó enfrente de la señora Katherine
pista_2_ok :-
    persona_con_caracteristica(PersonaProfesora, profesora),
    asiento_de_persona(PersonaProfesora, AsientoProfesoraNum),
    asiento_de_persona(katherine, AsientoKatherineNum),
    enfrente(AsientoProfesoraNum, AsientoKatherineNum).

% Pista 3: Katherine se sentó entre la mujer que era viajera y la mujer que admiraba a Marie
pista_3_ok :-
    asiento_de_persona(katherine, AsientoKatherineNum),
    persona_con_caracteristica(PersonaViajera, viajera),
    persona_con_caracteristica(PersonaAdmiraMarie, admira_marie),
    asiento_de_persona(PersonaViajera, AsientoViajeraNum),
    asiento_de_persona(PersonaAdmiraMarie, AsientoAdmiraMarieNum),
    al_lado(AsientoKatherineNum, AsientoViajeraNum),
    al_lado(AsientoKatherineNum, AsientoAdmiraMarieNum),
    PersonaViajera \= PersonaAdmiraMarie.

% Pista 4: La mujer que odiaba a las polillas se sentó frente a la señora Jane
pista_4_ok :-
    persona_con_caracteristica(PersonaOdiaPolillas, odia_polillas),
    asiento_de_persona(PersonaOdiaPolillas, AsientoOdiaPolillasNum),
    asiento_de_persona(jane, AsientoJaneNum),
    enfrente(AsientoOdiaPolillasNum, AsientoJaneNum).

% Pista 5: La mujer que odiaba a las polillas se sentó junto a la mujer que era profesora y a la izquierda de la que admiraba a Marie
pista_5_ok :-
    persona_con_caracteristica(PersonaOdiaPolillas, odia_polillas),
    persona_con_caracteristica(PersonaProfesora, profesora),
    persona_con_caracteristica(PersonaAdmiraMarie, admira_marie),
    asiento_de_persona(PersonaOdiaPolillas, AsientoOdiaPolillasNum),
    asiento_de_persona(PersonaProfesora, AsientoProfesoraNum),
    asiento_de_persona(PersonaAdmiraMarie, AsientoAdmiraMarieNum),
    al_lado(AsientoOdiaPolillasNum, AsientoProfesoraNum),
    izquierda(AsientoOdiaPolillasNum, AsientoAdmiraMarieNum).

% Pista 6: La mujer que era viajera se sentó al lado de la señora Katherine
pista_6_ok :-
    persona_con_caracteristica(PersonaViajera, viajera),
    asiento_de_persona(PersonaViajera, AsientoViajeraNum),
    asiento_de_persona(katherine, AsientoKatherineNum),
    al_lado(AsientoViajeraNum, AsientoKatherineNum).

% Pista 7: La mujer viajera se sentó al lado de Ada
pista_7_ok :-
    persona_con_caracteristica(PersonaViajera, viajera),
    asiento_de_persona(PersonaViajera, AsientoViajeraNum),
    asiento_de_persona(ada, AsientoAdaNum),
    al_lado(AsientoViajeraNum, AsientoAdaNum).

% Pista 8: Ada se sentó enfrente de la mujer que admiraba a Marie
pista_8_ok :-
    asiento_de_persona(ada, AsientoAdaNum),
    persona_con_caracteristica(PersonaAdmiraMarie, admira_marie),
    asiento_de_persona(PersonaAdmiraMarie, AsientoAdmiraMarieNum),
    enfrente(AsientoAdaNum, AsientoAdmiraMarieNum).

% Pista 9: La señora Chien se sentó junto a la mujer que odia las polillas y enfrente de la dueña de casa
pista_9_ok :-
    asiento_de_persona(chien, AsientoChienNum),
    persona_con_caracteristica(PersonaOdiaPolillas, odia_polillas),
    persona_con_caracteristica(PersonaDuenaCasa, duena_casa),
    asiento_de_persona(PersonaOdiaPolillas, AsientoOdiaPolillasNum),
    asiento_de_persona(PersonaDuenaCasa, AsientoDuenaCasaNum),
    al_lado(AsientoChienNum, AsientoOdiaPolillasNum),
    enfrente(AsientoChienNum, AsientoDuenaCasaNum).

% Regla principal para verificar la solución completa
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

% Verificar que cada persona tiene una característica única
verificar_unicidad_asignaciones :-
    findall(P-C, tiene_caracteristica(P,C), Asignaciones),
    length(Asignaciones, 6), % Asegura que hay 6 asignaciones
    maplist(arg(1), Asignaciones, PersonasAsignadas),
    all_distinct(PersonasAsignadas),
    maplist(arg(2), Asignaciones, CaracteristicasAsignadas),
    all_distinct(CaracteristicasAsignadas).

% Verificar que cada asiento tiene una persona única
verificar_unicidad_asientos :-
    findall(P-A, en_asiento(P,A), Ubicaciones),
    length(Ubicaciones, 6), % Asegura que hay 6 ubicaciones
    maplist(arg(1), Ubicaciones, PersonasUbicadas),
    all_distinct(PersonasUbicadas),
    maplist(arg(2), Ubicaciones, AsientosOcupados),
    all_distinct(AsientosOcupados).

% Predicado final que combina todas las verificaciones
solucion_valida :-
    verificar_unicidad_asignaciones,
    verificar_unicidad_asientos,
    verificar_solucion_completa.

% --- Predicado para generar la solución ---

% Generar una lista de términos en_asiento/2
generar_hechos_asientos(PersonasOrdenadas, AsientosIDs, HechosAsientos) :-
    maplist(construit_en_asiento, PersonasOrdenadas, AsientosIDs, HechosAsientos).

construit_en_asiento(Persona, AsientoID, en_asiento(Persona, AsientoID)).

% Generar una lista de términos tiene_caracteristica/2
generar_hechos_caracteristicas(Personas, CaracteristicasOrdenadas, HechosCaracteristicas) :-
    maplist(construit_tiene_caracteristica, Personas, CaracteristicasOrdenadas, HechosCaracteristicas).

construit_tiene_caracteristica(Persona, Caracteristica, tiene_caracteristica(Persona, Caracteristica)).

% Predicado principal para generar la solución
generar_solucion(AsignacionesAsientos, AsignacionesCaracteristicas) :-
    % Listas base de personas, asientos y características
    Personas = [jane, ada, katherine, marie, grace, chien],
    AsientosIDs = [asiento1, asiento2, asiento3, asiento4, asiento5, asiento6],
    Caracteristicas = [profesora, modesta, odia_polillas, admira_marie, viajera, duena_casa],

    % Generar todas las permutaciones posibles para los asientos
    permutation(Personas, PermutacionAsientos),
    generar_hechos_asientos(PermutacionAsientos, AsientosIDs, AsignacionesAsientos),

    % Generar todas las permutaciones posibles para las características
    permutation(Caracteristicas, PermutacionCaracteristicas),
    generar_hechos_caracteristicas(Personas, PermutacionCaracteristicas, AsignacionesCaracteristicas),

    % Afirmar temporalmente los hechos y luego verificar las pistas
    % setup_call_cleanup/3 es ideal para esto:
    % 1. Inicializa (asserta los hechos)
    % 2. Ejecuta la meta (solucion_valida)
    % 3. Limpia (retracta los hechos)
    setup_call_cleanup(
        (   % Inicialización: afirmar todos los hechos de asientos y características
            maplist(assertz, AsignacionesAsientos),
            maplist(assertz, AsignacionesCaracteristicas)
        ),
        (   % Llamada: verificar si esta configuración satisface las pistas
            solucion_valida
        ),
        (   % Limpieza: retractar los hechos para el siguiente intento de backtracking
            maplist(retract, AsignacionesAsientos),
            maplist(retract, AsignacionesCaracteristicas)
        )
    ).