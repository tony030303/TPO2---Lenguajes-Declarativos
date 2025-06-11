
//importamos componentes para usar WASM
import init, { MachineBuilder } from './scryer-pkg/scryer_prolog.js';

//referencia al contenedor donde se va a mostrar la salida
const output = document.getElementById("output");

// máquina de Prolog
let scryerMachine = null;

//variables aux. para almacenar soluciones y el índice actual.
let soluciones = [];
let indiceSolucionActual = -1;

//--- funciones complementarias para la salida --- 

// agregar un nuevo mensaje a la salida de texto
function logMessage(msg) {
    const p = document.createElement("div");
    p.textContent = msg;
    output.appendChild(p);
    output.scrollTop = output.scrollHeight; //scrollea hasta el final para colocarlo
}

//limpiar la salida
function limpiarOutput() {
    output.innerHTML = "";
}

//función que inicializa Scryer Prolog en la web
async function initializeProlog() {
    try {
        await init();  // inicializa WebAssembly
        
        // carga el archivo Prolog (mesate.pl)
        const mesaTeProlog = await fetch('mesate.pl').then(r => r.text());

        // se procede a construir la máquina Prolog
        const builder = new MachineBuilder();
        
        // finaliza y se guarda la máquina para usar después
        scryerMachine = builder.build();
        
        // se consulta el código Prolog cargado para habilitar sus predicados
        scryerMachine.consultModuleString('user', mesaTeProlog);

        //si no hay errores, podemos usar dichos predicados para generar y verificar soluciones.
        
    } catch (e) {
        logMessage("Scryer Prolog ERROR al cargar la lógica: " + e.message);
        console.error("Scryer Prolog Error:", e);
    }
}

// llama a la inicialización de manera automática ni bien se carga la página
initializeProlog();


// --- BOTONES ---

// --- Botón de Verificar Solución ---
document.getElementById('checkSolutionButton').addEventListener('click', async () => {
    limpiarOutput(); //limpiamos la salida
    
    //antes, se valida que la máquina Prolog esté cargada para evitar errores.
    if (!scryerMachine) {
        logMessage("Scryer Prolog no está inicializado. Por favor, espera o recarga la página.");
        return;
    }
    
    //obtenemos los seis asientos
    const asientos = document.querySelectorAll('.asiento-zona');
    
    //se crean listas que van a contener los hechos que vamos a pasarle al programa Prolog.
    const asignacionesAsientos = [];
    const asignacionesCaracteristicas = [];
    
    //bandera que nos sirve para verificar si todos los asientos y caracteristicas están ocupados.
    let slotsLlenos = true;

    //para cada asiento:
    asientos.forEach(asiento => {
        const idAsiento = asiento.id;
        const personaCarta = asiento.querySelector('.asiento-persona-slot .persona-carta');
        const caracteristicaCarta = asiento.querySelector('.asiento-caracteristica-slot .caracteristica-carta');

        //en caso de encontrar una científica asignada en ese asiento, extraemos los datos
        if (personaCarta) {
            const idPersona = personaCarta.id;
            asignacionesAsientos.push(`en_asiento(${idPersona}, ${idAsiento})`);
        } else {
            //sino, evitamos con la bandera que se pueda verificar la solución
            slotsLlenos = false;
        }

        //en caso de que haya una característica asignada y hay una científica, extraemos los datos
        if (caracteristicaCarta && personaCarta) {
            const idPersona = personaCarta.id;
            const idCaracteristica = caracteristicaCarta.id;
            asignacionesCaracteristicas.push(`tiene_caracteristica(${idPersona}, ${idCaracteristica})`);
        } else if (caracteristicaCarta && !personaCarta) { //sino, se advierte y usamos la bandera
            logMessage(`Advertencia: Característica ${caracteristicaCarta.id} en ${idAsiento} sin persona asignada.`);
            slotsLlenos = false; 
        } else { //misma situación
            slotsLlenos = false;
        }
    });

    //si no están los slots llenos (asientos y caracteristicas), evitamos que continue.
    if (!slotsLlenos || asignacionesAsientos.length !== 6 || asignacionesCaracteristicas.length !== 6) {
        logMessage("SOLUCIÓN INCOMPLETA. Por favor, asegúrate de que todos los asientos tienen una persona y una característica.");
        return;
    }

    //si todos los slots están ocupados (asientos y caracteristicas) ... 
    //preparamos las listas de hechos en formato Prolog
    const queryAsientos = `[${asignacionesAsientos.join(', ')}]`;
    const queryCaracteristicas = `[${asignacionesCaracteristicas.join(', ')}]`;

    //creamos el query que va a llamar al predicado Prolog verificar_solucion/2
    const query = `verificar_solucion(${queryAsientos}, ${queryCaracteristicas}).`;
    
    //ejecutamos la consulta utilizando la máquina Prolog generada antes.
    try {
        const iterator = scryerMachine.runQuery(query);
        
        // formateamos las soluciones
        for (const solucion of iterator) {
            // filtramos el resultado final que siempre es 'false' cuando no encuentra mas soluciones
            
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
    limpiarOutput(); //limpiamos la salida

    //antes, se valida que la máquina Prolog esté cargada para evitar errores.
    if (!scryerMachine) {
        logMessage("Scryer Prolog no está inicializado. Por favor, espera o recarga la página.");
        return;
    }
    
    // primero, se limpia el tablero actual.
    //para esto, se mueven todas las cartas de vuelta a sus contenedores originales.
    const personasContenedor = document.getElementById('personas-disponibles');
    const caracteristicasContenedor = document.getElementById('caracteristicas-disponibles');

    document.querySelectorAll('.persona-carta').forEach(carta => {
        personasContenedor.appendChild(carta);
    });
    document.querySelectorAll('.caracteristica-carta').forEach(carta => {
        caracteristicasContenedor.appendChild(carta);
    });

    //tambien se limpia el texto de cada slot de los asientos
    document.querySelectorAll('.asiento-persona-slot').forEach(slot => {
        slot.innerHTML = 'Coloca Persona';
    });
    document.querySelectorAll('.asiento-caracteristica-slot').forEach(slot => {
        slot.innerHTML = 'Coloca Característica';
    });

    // luego, se generan todas las soluciones si no se han generado aún
    if (soluciones.length === 0) {
        const query = `generar_solucion(Asientos,Caracteristicas).`;
        const iterator = scryerMachine.runQuery(query);
        
        //para cada solución en el conjunto de soluciones retornado por el programa, 
        // las vamos almacenando.
        for await (const solution of iterator) {
            soluciones.push(solution);
        }
        //si no hay ninguna, avisamos al usuario (este caso no sucede con el 'mesate.pl' usado)
        if (soluciones.length === 0) {
            logMessage("No se encontraron soluciones.");
            return;
        }
        
    }

    // se avanza a la sig. solución disponible (esto va a ser ciclico entre la X soluciones)
    indiceSolucionActual = (indiceSolucionActual + 1) % soluciones.length;
    const firstSolution = soluciones[indiceSolucionActual];
    
    //luego, obtenemos las listas Prolog de la solución actual
    const asientos = firstSolution.bindings['Asientos'].list;
    const caracteristicas = firstSolution.bindings['Caracteristicas'].list; 
    

    // función para parsear los asientos desde los términos compuestos de Prolog
    const parseAsientos = (asientosList) => {
        return asientosList.map(fact => {
            if (fact.type === 'compound' && fact.functor === 'en_asiento' && fact.args.length === 2) {
                return {
                    asiento: fact.args[0].integer.toString(), // el asiento es un número (BigInt), lo convertimos a string
                    persona: fact.args[1].atom //la persona es un átomo (string de Prolog)
                };
            }
            return null;
        }).filter(item => item !== null);
    };

    // función para parsear las características
    const parseCaracteristicas = (caracteristicasList) => {
        return caracteristicasList.map(fact => {
            if (fact.type === 'compound' && fact.functor === 'tiene_caracteristica' && fact.args.length === 2) {
                return {
                    asiento: fact.args[0].integer.toString(), 
                    caracteristica: fact.args[1].atom
                };
            }
            return null;
        }).filter(item => item !== null);
    };

    // utilizando las funciones anteriores, parseamos asientos y caracteristicas
    const parsedAsientos = parseAsientos(asientos);
    const parsedCaracteristicas = parseCaracteristicas(caracteristicas);

    // actualizamos el DOM con la solución generada anteriormente
    parsedAsientos.forEach(({ persona, asiento }) => {
        const carta = document.getElementById(persona); //buscamos donde esta la carta de la persona
        const asientoElement = document.getElementById(asiento); //buscamos el asiento
        const slot = asientoElement.querySelector(`.asiento-persona-slot`); //buscamos el slot
        if (carta && slot) {
            slot.innerHTML = '';
            slot.appendChild(carta); //colocamos la carta en el slot que corresponda
        } else { //debug
            //logMessage(`Error: No se encontró carta para ${persona} o slot para asiento-${asiento}`);
        }
    });

    //mismo procedimiento para las caracteristicas
    parsedCaracteristicas.forEach(({ caracteristica, asiento }) => {
        const carta = document.getElementById(caracteristica);
        const asientoElement = document.getElementById(asiento);
        const slot = asientoElement.querySelector(`.asiento-caracteristica-slot`);
        if (carta && slot) {
            slot.innerHTML = '';
            slot.appendChild(carta);
            
        } else { //debug
            //logMessage(`Error: No se encontró carta para ${caracteristica} o slot para asiento-${asiento}`);
        }
    });

    logMessage("¡Solución generada correctamente!");
    
});

// --- Botón de Reinicio ---
document.getElementById('restart').addEventListener('click', async () => {
    
    limpiarOutput(); //limpiamos la salida
    
    //retorna a las cartas (cientificas, caracteristicas) a sus contenedores iniciales.
    
    const personasContenedor = document.getElementById('personas-disponibles');
    const caracteristicasContenedor = document.getElementById('caracteristicas-disponibles');
    
    //devuelve todas las cartas de científicas a su contenedor inicial
    document.querySelectorAll('.persona-carta').forEach(carta => {
        personasContenedor.appendChild(carta);
    });

    //devuelve todas las cartas de características a su contenedor inicial
    document.querySelectorAll('.caracteristica-carta').forEach(carta => {
        caracteristicasContenedor.appendChild(carta);
    });

    //luego, se restablecen los textos que estaban en los slots
    document.querySelectorAll('.asiento-persona-slot').forEach(slot => {
        slot.innerHTML = 'Coloca Persona';
    });
    document.querySelectorAll('.asiento-caracteristica-slot').forEach(slot => {
        slot.innerHTML = 'Coloca Característica';
    });
});