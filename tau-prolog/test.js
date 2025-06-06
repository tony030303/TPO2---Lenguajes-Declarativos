const pl = require('tau-prolog');
const fs = require('fs');

const session = pl.create();

session.consult(fs.readFileSync('te.pl', 'utf8'), {
    success: function () {
        console.log('Archivo Prolog cargado correctamente.');
        session.query('generar_solucion(Asientos, Categorias).', {
            success: function () {
                console.log('Consulta válida.');
                function getAnswers(answer) {
                    session.answer({
                        success: function (answer) {
                            console.log('Solución encontrada:', session.format_answer(answer));
                            getAnswers(answer);
                        },
                        fail: function () {
                            console.log('No hay más soluciones.');
                        },
                        error: function (err) {
                            console.error('Error en la ejecución:', err.args[0]);
                        },
                        limit: function () {
                            console.log('Límite alcanzado.');
                        }
                    });
                }
                getAnswers(null);
            },
            error: function (err) {
                console.error('Error en la consulta:', err.args[0]);
            }
        });
    },
    error: function (err) {
        console.error('Error al cargar el archivo:', err.args[0]);
    }
});