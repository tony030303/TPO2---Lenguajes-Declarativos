import init, { MachineBuilder } from './scryer-pkg/scryer_prolog.js';

const output = document.getElementById("output");
let scryerMachine = null;

let soluciones = [];
let indiceSolucionActual = -1;

function logMessage(msg) {
    output.textContent += msg + "\n";
    output.scrollTop = output.scrollHeight;
}

async function initializeProlog() {
    try {
        await init();  // Inicializa WebAssembly
        logMessage("Scryer Prolog: Inicializando WebAssembly...");

        // Cargar el archivo Prolog (mesatezapala.pl)
        const mesaTeProlog = await fetch('mesate.pl').then(r => r.text());

        // Construir la máquina Prolog
        const builder = new MachineBuilder();
        // Finalizar y guardar la máquina para usar después
        scryerMachine = builder.build();
        // Consultar el código Prolog cargado
        scryerMachine.consultModuleString('user', mesaTeProlog);


        logMessage("Scryer Prolog: Lógica del juego 'mesatezapala.pl' cargada correctamente.");
        logMessage("Arrastra las personas a los asientos y las características a los slots correspondientes.");
        logMessage("Luego, haz clic en 'Verificar Solución'.");
    } catch (e) {
        logMessage("Scryer Prolog ERROR al cargar la lógica: " + e.message);
        console.error("Scryer Prolog Error:", e);
    }
}


// Llamar a la inicialización
initializeProlog();

// Variables globales para el arrastre
let draggedElement = null; // Elemento HTML que se está arrastrando (la carta)
let draggedElementType = null; // 'persona' o 'caracteristica'

// --- MANEJADORES DE EVENTOS DE DRAG & DROP ---

document.querySelectorAll('.carta').forEach(carta => {
    carta.addEventListener('dragstart', (e) => {
        draggedElement = e.target;
        draggedElementType = e.target.classList.contains('persona-carta') ? 'persona' : 'caracteristica';
        e.dataTransfer.setData('text/plain', e.target.id);
        e.dataTransfer.effectAllowed = 'move';
        setTimeout(() => {
            e.target.classList.add('dragging');
        }, 0);
        logMessage(`Arrastrando: ${e.target.id} (${draggedElementType})`);
    });

    carta.addEventListener('dragend', (e) => {
        e.target.classList.remove('dragging');
        draggedElement = null;
        draggedElementType = null;
        logMessage(`Terminó el arrastre para: ${e.target.id}`);
    });
});

document.querySelectorAll('.asiento-persona-slot, .asiento-caracteristica-slot').forEach(slot => {
    slot.addEventListener('dragover', (e) => {
        e.preventDefault();
        const targetSlotType = slot.dataset.slotType;
        if (draggedElementType === targetSlotType) {
            e.dataTransfer.dropEffect = 'move';
            slot.classList.add('dragover');
        } else {
            e.dataTransfer.dropEffect = 'none';
        }
    });

    slot.addEventListener('dragleave', (e) => {
        slot.classList.remove('dragover');
    });

    slot.addEventListener('drop', (e) => {
        e.preventDefault();
        slot.classList.remove('dragover');

        const idCarta = e.dataTransfer.getData('text/plain');
        const cartaSoltada = document.getElementById(idCarta);
        const targetSlotType = slot.dataset.slotType;

        if (!cartaSoltada || draggedElementType !== targetSlotType) {
            logMessage("Movimiento inválido: Tipo de carta incorrecto para este slot.");
            return;
        }

        const oldParentSlot = cartaSoltada.parentElement;
        if (oldParentSlot && (oldParentSlot.classList.contains('asiento-persona-slot') || oldParentSlot.classList.contains('asiento-caracteristica-slot'))) {
            const parentOfOldSlot = oldParentSlot.parentElement;
            if (parentOfOldSlot && parentOfOldSlot.classList.contains('asiento-zona')) {
                oldParentSlot.textContent = `Coloca ${targetSlotType === 'persona' ? 'Persona' : 'Característica'}`;
            }
        }

        slot.innerHTML = '';
        slot.appendChild(cartaSoltada);
        // Usar el ID del asiento padre como destino
        const slotId = slot.parentElement.id; // Obtener el ID del asiento-zona (asiento1, asiento2, etc.)
        logMessage(`Movida: ${idCarta} a ${slotId} (${targetSlotType})`);
    });
});

// Función auxiliar para capitalizar la primera letra si es necesario (para átomos de Prolog)
function capitalizeFirstLetter(string) {
    return string.charAt(0).toUpperCase() + string.slice(1);
}

// --- Botón de Verificar Solución ---
document.getElementById('checkSolutionButton').addEventListener('click', async () => {
    if (!scryerMachine) {
        logMessage("Scryer Prolog no está inicializado. Por favor, espera o recarga la página.");
        return;
    }
    logMessage("Verificando solución con Scryer Prolog...");

    const asientos = document.querySelectorAll('.asiento-zona');
    const asignacionesAsientos = [];
    const asignacionesCaracteristicas = [];
    let allSlotsFilled = true;

    asientos.forEach(asiento => {
        const idAsiento = asiento.id;
        const personaCarta = asiento.querySelector('.asiento-persona-slot .persona-carta');
        const caracteristicaCarta = asiento.querySelector('.asiento-caracteristica-slot .caracteristica-carta');

        if (personaCarta) {
            const idPersona = personaCarta.id;
            // Scryer Prolog usa 'atom' para identificadores en minúsculas
            asignacionesAsientos.push(`en_asiento(${idPersona}, ${idAsiento})`);
        } else {
            allSlotsFilled = false;
        }

        if (caracteristicaCarta && personaCarta) {
            const idPersona = personaCarta.id;
            const idCaracteristica = caracteristicaCarta.id;
            asignacionesCaracteristicas.push(`tiene_caracteristica(${idPersona}, ${idCaracteristica})`);
        } else if (caracteristicaCarta && !personaCarta) {
            logMessage(`Advertencia: Característica ${caracteristicaCarta.id} en ${idAsiento} sin persona asignada.`);
            allSlotsFilled = false; // Considerar incompleto si una característica se coloca sin una persona
        } else {
            allSlotsFilled = false;
        }
    });

    if (!allSlotsFilled || asignacionesAsientos.length !== 6 || asignacionesCaracteristicas.length !== 6) {
        logMessage("SOLUCIÓN INCOMPLETA. Por favor, asegúrate de que todos los asientos tienen una persona y una característica.");
        return;
    }

    const queryAsientos = `[${asignacionesAsientos.join(', ')}]`;
    const queryCaracteristicas = `[${asignacionesCaracteristicas.join(', ')}]`;

    const query = `verificar_solucion(${queryAsientos}, ${queryCaracteristicas}).`;
    
    console.log('Ejecutando consulta: ', query);
    logMessage(`Consulta a Prolog: ${query}`);

    try {
        const iterator = scryerMachine.runQuery(query);
        console.log('PASAMOS EL RUN!!!!!!!!!!!!!');
        console.log(iterator);

            // Formateamos las soluciones
            for (const solucion of iterator) {
                // Filtramos el resultado final que siempre es 'false' cuando no encuentra mas soluciones
                console.log('LA SOLUCION ES: ',solucion);
                if(solucion !== false) {
                    logMessage("¡SOLUCIÓN CORRECTA! Todas las pistas se cumplen.");
                } else{
                    logMessage("SOLUCIÓN INCORRECTA. No todas las pistas se cumplen.");
                }
            }
    } catch (err) {
        logMessage("Scryer Prolog ERROR al verificar la solución: " + err.message);
        console.error("Scryer Prolog query error:", err);
    }
});


// --- Botón de Generar Solución ---
document.getElementById('generateSolutionButton').addEventListener('click', async () => {

    if (!scryerMachine) {
        logMessage("Scryer Prolog no está inicializado. Por favor, espera o recarga la página.");
        return;
    }
    logMessage("Generando solución con Scryer Prolog...");

    // Paso 1: Limpiar el DOM (devolver cartas a contenedores iniciales)
    const personasContenedor = document.getElementById('personas-disponibles');
    const caracteristicasContenedor = document.getElementById('caracteristicas-disponibles');
    document.querySelectorAll('.persona-carta').forEach(carta => {
        personasContenedor.appendChild(carta);
    });
    document.querySelectorAll('.caracteristica-carta').forEach(carta => {
        caracteristicasContenedor.appendChild(carta);
    });
    document.querySelectorAll('.asiento-persona-slot').forEach(slot => {
        slot.innerHTML = 'Coloca Persona';
    });
    document.querySelectorAll('.asiento-caracteristica-slot').forEach(slot => {
        slot.innerHTML = 'Coloca Característica';
    });

    // Paso 2: Generar todas las soluciones si no se han generado aún
    if (soluciones.length === 0) {
        const query = `generar_solucion(Asientos,Caracteristicas).`;
        logMessage(`Consulta para generar: ${query}`);

        const iterator = scryerMachine.runQuery(query);
        logMessage("Calculando todas las soluciones...");

        for await (const solution of iterator) {
            soluciones.push(solution);
        }

        if (soluciones.length === 0) {
            logMessage("No se encontraron soluciones.");
            return;
        }
        logMessage(`Se encontraron ${soluciones.length} soluciones.`);
    }

    // Avanzar al siguiente índice (cíclico)
    indiceSolucionActual = (indiceSolucionActual + 1) % soluciones.length;
    const firstSolution = soluciones[indiceSolucionActual];
    logMessage(`Mostrando solución ${indiceSolucionActual + 1} de ${soluciones.length}`);

    console.log("Solución actual: ", firstSolution);
    
    const asientos = firstSolution.bindings['Asientos'].list;
    console.log("Asientos: ", asientos);
    const caracteristicas = firstSolution.bindings['Caracteristicas'].list; // Asegúrate de que el nombre de la variable coincida
    console.log("Caracteristicas: ", caracteristicas);


    // Función para parsear los asientos
    const parseAsientos = (asientosList) => {
        return asientosList.map(fact => {
            if (fact.type === 'compound' && fact.functor === 'en_asiento' && fact.args.length === 2) {
                return {
                    asiento: fact.args[0].integer.toString(), // Convertir BigInt a string
                    persona: fact.args[1].atom
                };
            }
            return null;
        }).filter(item => item !== null);
    };

    // Función para parsear las características
    const parseCaracteristicas = (caracteristicasList) => {
        return caracteristicasList.map(fact => {
            if (fact.type === 'compound' && fact.functor === 'tiene_caracteristica' && fact.args.length === 2) {
                return {
                    asiento: fact.args[0].integer.toString(), // Convertir BigInt a string
                    caracteristica: fact.args[1].atom
                };
            }
            return null;
        }).filter(item => item !== null);
    };

    // Parsear las listas
    const parsedAsientos = parseAsientos(asientos);
    const parsedCaracteristicas = parseCaracteristicas(caracteristicas);

    // Actualizar el DOM con la solución
    parsedAsientos.forEach(({ persona, asiento }) => {
        const carta = document.getElementById(persona);
        const asientoElement = document.getElementById(asiento);
        const slot = asientoElement.querySelector(`.asiento-persona-slot`);
        if (carta && slot) {
            slot.innerHTML = '';
            slot.appendChild(carta);
            logMessage(`Asignada: ${persona} a asiento-${asiento} (persona)`);
        } else {
            logMessage(`Error: No se encontró carta para ${persona} o slot para asiento-${asiento}`);
        }
    });

    parsedCaracteristicas.forEach(({ caracteristica, asiento }) => {
        const carta = document.getElementById(caracteristica);
        const asientoElement = document.getElementById(asiento);
        const slot = asientoElement.querySelector(`.asiento-caracteristica-slot`);
        if (carta && slot) {
            slot.innerHTML = '';
            slot.appendChild(carta);
            logMessage(`Asignada: ${caracteristica} a asiento-${asiento} (característica)`);
        } else {
            logMessage(`Error: No se encontró carta para ${caracteristica} o slot para asiento-${asiento}`);
        }
    });

    logMessage("¡Solución generada correctamente!");
    //}
    //catch (err) {
    //    logMessage("Scryer Prolog ERROR al generar la solución: " + err.message);
    //    console.error("Scryer Prolog query error:", err);
    //    if (err.stack) console.error(err.stack);
    //}
});