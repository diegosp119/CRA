%Hechos
numero(1).
numero(2).
numero(3).
numero(4).
numero(5).
numero(6).
numero(7).
numero(8).
numero(9).

posiciones([1,2,3,4,5,6,7,8,9])

cuadrado([.,.,.,.,3,.,.,.,.])

posibilidades([1,2,3,4,5,6,7,8,9])

% Regla para obtener los números presentes en cuadrado y descartarlos de las posibilidades
eliminar_presentes([], Posibles, Posibles). % Caso base: si la lista de cuadrado está vacía, devolver las posibilidades sin cambios.

eliminar_presentes([X | Resto], Posibles, Filtradas) :-
    numero(X), % Si X es un número válido en el Sudoku
    delete(Posibles, X, NuevaLista), % Eliminar X de las posibilidades
    eliminar_presentes(Resto, NuevaLista, Filtradas). % Llamada recursiva

eliminar_presentes([_| Resto], Posibles, Filtradas) :- 
    eliminar_presentes(Resto, Posibles, Filtradas). % Ignorar elementos no numéricos (como '.') y continuar

% Regla principal para obtener las posibilidades filtradas
posibilidades_filtradas(Filtradas) :-
    cuadrado(Cuadrado),
    posibilidades(Posibles),
    eliminar_presentes(Cuadrado, Posibles, Filtradas).
