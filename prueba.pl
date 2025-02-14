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


sudoku([.,.,.,.,3,.,.,.,.,.,.,.,.,4,.,.,.,.,.,.,.,.,5,.,.,.,.,.,.,.,.,6,.,.,.,.]).

posibilidades_cuadrados([[1,2,3,4,5,6,7,8,9],[1,2,3,4,5,6,7,8,9],[1,2,3,4,5,6,7,8,9],[1,2,3,4,5,6,7,8,9]]).

posibilidades_filas([[1,2,3,4,5,6,7,8,9],[1,2,3,4,5,6,7,8,9],[1,2,3,4,5,6,7,8,9],[1,2,3,4,5,6,7,8,9],[1,2,3,4,5,6,7,8,9],[1,2,3,4,5,6,7,8,9]]).

posibilidades_columnas([[1,2,3,4,5,6,7,8,9],[1,2,3,4,5,6,7,8,9],[1,2,3,4,5,6,7,8,9],[1,2,3,4,5,6,7,8,9],[1,2,3,4,5,6,7,8,9],[1,2,3,4,5,6,7,8,9]]).









% Manera de evaluar posibles de un cuadrado

% Regla para obtener los números presentes en cuadrado y descartarlos de las posibilidades


%Para seleccionar la lista de los cuadrados
seleccionar_lista(1, L1, _, _, _, L1).
seleccionar_lista(2, _, L2, _, _, L2).
seleccionar_lista(3, _, _, L3, _, L3).
seleccionar_lista(4, _, _, _, L4, L4).


%Para seleccionar la filtradas
seleccionar_fila(1, L1, _, _, _, _, _, L1).
seleccionar_fila(2, _, L2, _, _, _, _, L2).
seleccionar_fila(3, _, _, L3, _, _, _, L3).
seleccionar_fila(4, _, _, _, L4, _, _, L4).
seleccionar_fila(5, _, _, _, _, L5, _, L5).
seleccionar_fila(6, _, _, _, _, _, L6, L6).

%Para seleccionar la columna
seleccionar_columna(1, L1, _, _, _, _, _, L1).
seleccionar_columna(2, _, L2, _, _, _, _, L2).
seleccionar_columna(3, _, _, L3, _, _, _, L3).
seleccionar_columna(4, _, _, _, L4, _, _, L4).
seleccionar_columna(5, _, _, _, _, L5, _, L5).
seleccionar_columna(6, _, _, _, _, _, L6, L6).




% Caso base: si la lista de cuadrado está vacía, devolver las posibilidades sin cambios
eliminar_presentes([], Posibles_Cuad, Posibles_Fil, Posibles_Col, Posibles_Cuad, Posibles_Fil, Posibles_Col, N):-

eliminar_presentes([X | Resto],Posibles_Cuad, Posibles_Fil, Posibles_Col, Filtradas, N) :-
    N is N + 1;
    numero(X), % Si X es un número válido en el Sudoku

   
    seleccionar_lista(N // 9 + 1, L1,L2,L3,L4, ListaSeleccionadaCuadricula),  %Para la cuadrícula

    seleccionar_fila((N // 18 * 3) + N mod 9 //3 + 1, L1,L2,L3,L4,L5,L6, ListaSeleccionadaFila), %Para la fila

    seleccionar_columna(((N // 9) mod 2 * 3 ) + N mod 3 + 1 , L1,L2,L3,L4,L5,L6, ListaSeleccionadaFila), %Para la fila

    %De momento no se está realizando ningún control de que el número presente esté dentro de las posibilidades
    delete(Posibles_Cuad, X, NuevaListaCuad), % Eliminar X de las posibilidades
    delete (Posibles_Fil, X, NuevaListaFil), % Eliminar X de las posibilidades de filas
    delete(Posibles_Col, X, NuevaListaCol), % Eliminar X de las posibilidades de columnas
    
    eliminar_presentes(Resto, NuevaListaCuad, NuevaListaFil, NuevaListaCol, Filtradas, N).
     % Llamada recursiva

eliminar_presentes([_| Resto], Posibles_Cuad, Posibles_Fil, Posibles_Col, Posibles_Cuad, Posibles_Fil, Posibles_Col, Filtradas, N) :- 
    N is N + 1,
    eliminar_presentes(Resto, Posibles, Filtradas, N).
    % Ignorar elementos no numéricos (como '.') y continuar

% Regla principal para obtener las posibilidades filtradas
posibilidades_filtradas(Filtradas) :-
    sudoku(Sudoku),
    posibilidades_cuadrados(Posibles_Cuad),
    posibilidades_filas(Posibles_Fil),
    posibilidades_columnas(Posibles_Col),
    eliminar_presentes(Sudoku, Posibles_Cuad, Posibles_Fil, Posibles_Col, Filtradas, -1).



