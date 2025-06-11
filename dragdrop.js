

// --- GESTION DE EVENTOS DE ARRASTRE DE CARTAS ---

// Variables globales para arrastrar / soltar cartas
let elementoArrastrado = null; // Elemento HTML que se está arrastrando (la carta)
let tipoElemArrastrado = null; // 'persona' o 'caracteristica'

//se configura los eventos en cada carta
document.querySelectorAll('.carta').forEach(carta => {
    
    //al iniciar arrastre de la carta
    carta.addEventListener('dragstart', (e) => {
        elementoArrastrado = e.target;
        tipoElemArrastrado = e.target.classList.contains('persona-carta') ? 'persona' : 'caracteristica';
        e.dataTransfer.setData('text/plain', e.target.id);
        e.dataTransfer.effectAllowed = 'move';
        
        setTimeout(() => {
            e.target.classList.add('dragging'); //agrega clase visual mientras se arrastra
        }, 0);
       
    });

    //al finalizar arrastre de la carta
    carta.addEventListener('dragend', (e) => {
        e.target.classList.remove('dragging');
        elementoArrastrado = null;
        tipoElemArrastrado = null;
        
    });
});

//configura todos los slots de los asientos para aceptar elemento soltado
document.querySelectorAll('.asiento-persona-slot, .asiento-caracteristica-slot').forEach(slot => {
    
    //mientras el elem está sobre el slot
    slot.addEventListener('dragover', (e) => {
        e.preventDefault();
        const targetSlotType = slot.dataset.slotType;
        if (tipoElemArrastrado === targetSlotType) {
            e.dataTransfer.dropEffect = 'move';
            slot.classList.add('dragover');
        } else {
            e.dataTransfer.dropEffect = 'none';
        }
    });

    //cuando sale el área del slot
    slot.addEventListener('dragleave', (e) => {
        slot.classList.remove('dragover');
    });

    //al soltar la carta sobre el slot
    slot.addEventListener('drop', (e) => {
        e.preventDefault();
        slot.classList.remove('dragover');

        const idCarta = e.dataTransfer.getData('text/plain');
        const cartaSoltada = document.getElementById(idCarta);
        const targetSlotType = slot.dataset.slotType;

        //en caso de que no se haya soltado correctamente la carta 
        // o que el tipo no sea el mismo, será movimiento inválido
        if (!cartaSoltada || tipoElemArrastrado !== targetSlotType) {
            logMessage("Movimiento inválido: Tipo de carta incorrecto para este slot.");
            return;
        }

        //coloca la carta en el nuevo slot
        slot.innerHTML = '';
        slot.appendChild(cartaSoltada);
    });
});
