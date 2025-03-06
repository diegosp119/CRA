% Representación del Sudoku como una lista de 81 elementos
sudoku1([
    ., ., 9, 6, ., ., ., 1, .,
    8, ., ., ., ., 1, ., 9, .,
    7, ., ., ., ., ., ., ., 8,
    ., 3, ., ., 6, ., ., ., .,
    ., 4, ., 1, ., 9, ., ., 5,
    9, ., ., ., ., ., ., ., .,
    ., 8, ., 9, ., ., 5, 4, .,
    6, ., ., 7, 1, ., ., ., 3,
    ., ., 5, ., 8, 4, ., ., 9]).

sudoku([
    ., 8, ., 5, 7, 6, 2, ., .,
    ., ., ., 4, ., 2, ., ., .,
    ., ., ., ., 3, 9, 5, 4, 8,
    6, 3, ., 9, ., ., 8, 5, 2,
    ., 9, ., 2, ., ., 3, 7, .,
    8, ., ., ., 5, ., 6, 9, 4,
    2, 5, 7, 6, ., 3, 4, 8, 9,
    3, ., 8, 7, ., ., ., 2, 5,
    ., 4, ., ., ., ., ., ., 6
]).

sudoku2([5, 3, ., ., 7, ., ., ., .,
        6, ., ., 1, 9, 5, ., ., .,
        1, 9, 8, ., ., ., ., 6, .,
        8, ., ., ., 6, ., ., ., 3,
        4, ., ., 8, ., 3, ., ., 1,
        7, ., ., ., 2, ., ., ., 6,
        ., 6, ., ., ., ., 2, 8, .,
        ., ., ., 4, 1, 9, ., ., 5,
        ., ., ., ., 8, ., ., 7, 9
]).

sudoku3([ ., ., 7, ., ., ., 8, ., .,
         ., 4, 5, 7, 6, ., ., ., 2,
         6, ., ., ., 4, ., 3, ., 5,
         8, 6, ., 5, ., ., ., 4, .,
         ., ., 3, 8, ., 4, ., 6, .,
         7, 2, 6, 9, ., ., 8, 3, .,
         ., 5, ., ., ., ., 4, 7, .,
         7, ., 4, ., ., ., ., ., 6,
         3, 4, ., ., 6, ., 6, 2, 8]).

sudoku4([ ., ., 6, ., ., 2, 3, ., 4,
         9, ., 4, 7, 5, ., ., 7, 2,
         ., ., 8, ., ., 6, ., ., 5,
         ., ., 3, ., ., ., ., 4, .,
         2, ., ., 4, ., ., 8, 3, .,
         4, ., 7, 5, ., ., ., ., .,
         ., ., ., 6, ., ., ., ., 8,
         7, ., ., ., 2, ., 4, 5, 3,
         ., ., ., 3, 7, ., ., 6, 9]).

sudoku5([
    ., 8, ., 5, 7, 6, 2, ., .,
    ., ., ., 4, ., 2, ., ., .,
    ., ., ., ., 3, 9, 5, 4, 8,
    6, 3, ., 9, ., ., 8, 5, 2,
    ., 9, ., 2, ., ., 3, 7, .,
    8, ., ., ., 5, ., 6, 9, 4,
    2, 5, 7, 6, ., 3, 4, 8, 9,
    3, ., 8, 7, ., ., ., 2, 5,
    ., 4, ., ., ., ., ., ., 6
]).
sudoku6([
    ., 9, ., ., 2, 4, ., 7, .,
    ., ., ., ., ., ., ., ., .,
    6, 4, 8, 3, ., 7, ., ., .,
    ., 7, ., ., 4, 5, 6, ., 3,
    ., 5, ., ., ., ., ., 2, 7,
    2, ., 6, ., ., ., ., ., .,
    3, ., ., ., 8, ., ., 4, .,
    ., 7, ., 4, 3, ., ., 5, 6,
    4, 2, 5, ., ., ., 8, 3, 9
]).
sudoku7([
    7, 9, ., ., 6, 4, ., ., 2,
    ., ., 3, 6, ., ., 7, ., .,
    ., 4, 8, 1, ., 7, ., ., .,
    ., 7, ., ., 4, 5, 6, 9, 3,
    2, ., ., ., ., ., ., ., 7,
    ., ., 6, ., ., ., ., 2, .,
    4, ., ., ., 8, ., ., 3, .,
    5, 7, ., 4, 3, ., ., ., 6,
    3, 2, 5, ., ., ., 8, 4, 9]).

sudoku8([
    ., 9, ., ., 2, 4, ., 7, .,
    ., ., ., ., ., ., ., ., .,
    6, 4, ., 3, ., 7, ., ., .,
    ., 7, ., ., 4, 5, 6, ., 3,
    ., 5, ., ., ., ., ., 2, 7,
    2, ., 6, ., ., ., ., ., .,
    3, ., ., ., 8, ., ., 4, .,
    ., 7, ., 4, 3, ., ., 5, 6,
    4, 2, 5, ., ., ., 8, 3, 9
]).






indice_filas([1,2,3,10,11,12,19,20,21,4,5,6,13,14,15,22,23,24,7,8,9,16,17,18,25,26,27,28,29,30,37,38,39,46,47,48,31,32,33,40,41,42,49,50,51,34,35,36,43,44,45,52,53,54,55,56,57,64,65,66,73,74,75,58,59,60,67,68,69,76,77,78,61,62,63,70,71,72,79,80,81]).



% Predicado para imprimir el tablero de Sudoku
imprimir_sudoku([]).
imprimir_sudoku(Tablero) :-
    imprimir_fila(Tablero, Resto),
    imprimir_sudoku(Resto).

% Predicado para imprimir una fila del tablero
imprimir_fila(Tablero, Resto) :-
    tomar(9, Tablero, Fila, Resto),
    writeln(Fila).

% Predicado para tomar los primeros N elementos de una lista
tomar(0, L, [], L).
tomar(N, [H|T], [H|R], Resto) :-
    N > 0,
    N1 is N - 1,
    tomar(N1, T, R, Resto).

% -------------------------------
% Predicados Auxiliares
% -------------------------------

% Dividir el tablero en filas (9 elementos cada una).
filas_sudoku([], []).
filas_sudoku(Sudoku, [Fila|Filas]) :-
    tomar(9, Sudoku, Fila, Resto),
    filas_sudoku(Resto, Filas).

% Predicado para saltar N elementos en una lista.
skip(0, L, L).
skip(N, [_|T], Rest) :-
    N > 0,
    N1 is N - 1,
    skip(N1, T, Rest).

% Predicado para extraer una sublista (slicing): desde la posición Start, Count elementos.
slice(List, Start, Count, Slice) :-
    skip(Start, List, Rest),
    tomar(Count, Rest, Slice, _).

% Hechos
numero(1).
numero(2).
numero(3).
numero(4).
numero(5).
numero(6).
numero(7).
numero(8).
numero(9).


% Predicado para verificar si un número está en la misma fila
miembro_fila(Sudoku, Index, Num) :-
    Fila is Index // 9,
    Inicio is Fila * 9,
    Fin is Inicio + 8,
    between(Inicio, Fin, I),
    nth0(I, Sudoku, Num).

% Predicado para verificar si un número está en la misma columna
miembro_columna(Sudoku, Index, Num) :-
    Columna is Index mod 9,
    between(0, 8, Fila),
    I is Fila * 9 + Columna,
    nth0(I, Sudoku, Num).

% -------------------------------
% Predicado para Verificar el Cuadrante (3x3) por Slicing
% -------------------------------

miembro_cuadro(Sudoku, Index, Num) :-
    % Cálculo de fila y columna del elemento (0-based)
    Row is Index // 9,
    Col is Index mod 9,
    % Determinar el inicio (cabezera) del bloque 3×3
    RowStart is (Row // 3) * 3,
    ColStart is (Col // 3) * 3,
    % Convertir el tablero en una lista de filas
    filas_sudoku(Sudoku, Filas),
    % Extraer las 3 filas que componen el bloque:
    nth0(RowStart, Filas, Fila1),
    Row2 is RowStart + 1,
    nth0(Row2, Filas, Fila2),
    Row3 is RowStart + 2,
    nth0(Row3, Filas, Fila3),
    % Extraer el segmento de cada fila correspondiente al bloque (3 celdas)
    slice(Fila1, ColStart, 3, Seccion1),
    slice(Fila2, ColStart, 3, Seccion2),
    slice(Fila3, ColStart, 3, Seccion3),
    % Unir las secciones en una lista con todas las celdas del cuadrante
    append([Seccion1, Seccion2, Seccion3], Cuadro),
    % Verificar si el número ya se encuentra en ese bloque
    member(Num, Cuadro).

% Predicado para verificar si una casilla está vacía
casilla_vacia(Casilla) :-
    \+ number(Casilla).

% Predicado para obtener las posibilidades para cada casilla vacía o generar una lista vacía si la casilla está ocupada
posibilidades_casilla(Sudoku, Index, Posibilidades) :-
    nth0(Index, Sudoku, Casilla),
    (   casilla_vacia(Casilla) -> 
        findall(Num, (numero(Num), \+ miembro_fila(Sudoku, Index, Num), \+ miembro_columna(Sudoku, Index, Num), \+ miembro_cuadro(Sudoku, Index, Num)), Posibilidades)
    ;   Posibilidades = []
    ).

% Predicado para generar las listas de posibilidades para todas las casillas
generar_posibilidades(Sudoku, Posibilidades) :-
    generar_posibilidades(Sudoku, 0, Posibilidades).

generar_posibilidades(_, 81, []).
generar_posibilidades(Sudoku, Index, [Posibilidades|Resto]) :-
    Index < 81,
    posibilidades_casilla(Sudoku, Index, Posibilidades),
    NextIndex is Index + 1,
    generar_posibilidades(Sudoku, NextIndex, Resto).

% Predicado para imprimir las posibilidades
imprimir_posibilidades([]).
imprimir_posibilidades([P|Ps]) :-
    writeln(P),
    imprimir_posibilidades(Ps).


% Predicado para ajustar las posibilidades según el nuevo índice
ajuste_de_posibilidades(Posibilidades, PosibilidadesAjustadas) :-
    indice_filas(Indice_Filas),
    
    length(Posibilidades, Len),
    length(PosibilidadesAjustadas, Len),
    ajuste_de_posibilidades(Posibilidades, Indice_Filas, PosibilidadesAjustadas).




%caso base de ajuste de posibilidades
ajuste_de_posibilidades([], [], PosibilidadesAjustadas).
%caso recursivo de ajuste de posibilidades

ajuste_de_posibilidades([P|Ps], [NuevaPosicion|Indice_Filas], PosibilidadesAjustadas) :-
    nth1(NuevaPosicion, PosibilidadesAjustadas, P),
    ajuste_de_posibilidades(Ps, Indice_Filas, PosibilidadesAjustadas).






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% --- REGLA 1 ---


% Predicado principal
aplicar_regla_1(Posibilidades, NuevaPosibilidades) :-
    aplicar_regla_1_a_todas_filas(Posibilidades, Temp1),
    aplicar_regla_1_a_todas_columnas(Temp1, Temp2),
    aplicar_regla_1_a_todos_cuadros(Temp2, NuevaPosibilidades).


% Regla sobre filas

aplicar_regla_1_a_todas_filas(Posibilidades, NuevaPosibilidades) :-
    aplicar_regla_1_a_todas_filas(Posibilidades, 1, NuevaPosibilidades).

aplicar_regla_1_a_todas_filas(Posibilidades, 10, Posibilidades).
aplicar_regla_1_a_todas_filas(Posibilidades, Fila, NuevaPosibilidades) :-
    obtener_posibilidades_fila(Posibilidades, Fila, PosFila),
    encontrar_determinados(PosFila, Determinados),
    eliminar_determinados_de_fila(Posibilidades, Fila, Determinados, TempPosib),
    FilaSiguiente is Fila + 1,
    aplicar_regla_1_a_todas_filas(TempPosib, FilaSiguiente, NuevaPosibilidades).


% Regla sobre columnas

aplicar_regla_1_a_todas_columnas(Posibilidades, NuevaPosibilidades) :-
    aplicar_regla_1_a_todas_columnas(Posibilidades, 1, NuevaPosibilidades).

aplicar_regla_1_a_todas_columnas(Posibilidades, 10, Posibilidades).
aplicar_regla_1_a_todas_columnas(Posibilidades, Columna, NuevaPosibilidades) :-
    obtener_posibilidades_columna(Posibilidades, Columna, PosCol),
    encontrar_determinados(PosCol, Determinados),
    eliminar_determinados_de_columna(Posibilidades, Columna, Determinados, TempPosib),
    ColumnaSiguiente is Columna + 1,
    aplicar_regla_1_a_todas_columnas(TempPosib, ColumnaSiguiente, NuevaPosibilidades).


% Regla sobre cuadros (bloques 3x3)

aplicar_regla_1_a_todos_cuadros(Posibilidades, NuevaPosibilidades) :-
    aplicar_regla_1_a_todos_cuadros(Posibilidades, 1, NuevaPosibilidades).

aplicar_regla_1_a_todos_cuadros(Posibilidades, 10, Posibilidades).
aplicar_regla_1_a_todos_cuadros(Posibilidades, Cuadro, NuevaPosibilidades) :-
    obtener_posibilidades_cuadro(Posibilidades, Cuadro, PosC),
    encontrar_determinados(PosC, Determinados),
    eliminar_determinados_de_cuadro(Posibilidades, Cuadro, Determinados, TempPosib),
    CuadroSiguiente is Cuadro + 1,
    aplicar_regla_1_a_todos_cuadros(TempPosib, CuadroSiguiente, NuevaPosibilidades).


% Extraer de un grupo (lista de celdas) los números ya determinados

encontrar_determinados(PosibilidadesGrupo, Determinados) :-
    findall(Num, (member(P, PosibilidadesGrupo), length(P, 1), P = [Num]), Determinados).


% Eliminación de determinados en grupo (fila, columna o cuadro)

eliminar_determinados_de_fila(Posibilidades, Fila, Determinados, NuevaPosibilidades) :-
    Inicio is (Fila - 1) * 9 + 1,
    Fin is Fila * 9,
    findall(Index, between(Inicio, Fin, Index), Indices),
    eliminar_determinados_de_indices(Posibilidades, Indices, Determinados, NuevaPosibilidades).

eliminar_determinados_de_columna(Posibilidades, Columna, Determinados, NuevaPosibilidades) :-
    findall(Index, (between(0,8,I), Index is Columna + I * 9), Indices),
    eliminar_determinados_de_indices(Posibilidades, Indices, Determinados, NuevaPosibilidades).

eliminar_determinados_de_cuadro(Posibilidades, Cuadro, Determinados, NuevaPosibilidades) :-
    CuadroFila is ((Cuadro - 1) // 3) * 3,
    CuadroColumna is ((Cuadro - 1) mod 3) * 3,
    findall(Index,
            (between(0,2,I), between(0,2,J),
             Index is (CuadroFila + I) * 9 + CuadroColumna + J + 1
            ), Indices),
    eliminar_determinados_de_indices(Posibilidades, Indices, Determinados, NuevaPosibilidades).

eliminar_determinados_de_indices(Posibilidades, [], _, Posibilidades).
eliminar_determinados_de_indices(Posibilidades, [Indice|RestoIndices], Determinados, FinalPosibilidades) :-
    nth1(Indice, Posibilidades, PosActual),
    eliminar_determinados_de_posibilidad(PosActual, Determinados, PosModificada),
    replace(Posibilidades, Indice, PosModificada, PosTemp),
    eliminar_determinados_de_indices(PosTemp, RestoIndices, Determinados, FinalPosibilidades).

eliminar_determinados_de_posibilidad(Poss, Determinados, PossFinal) :-
    (   length(Poss, 1)
    ->  PossFinal = Poss
    ;   eliminar_elementos(Poss, Determinados, PossFinal)
    ).

eliminar_elementos(Poss, [], Poss).
eliminar_elementos(Poss, [D|Ds], PossFinal) :-
    delete(Poss, D, PossTemp),
    eliminar_elementos(PossTemp, Ds, PossFinal).


% Predicado auxiliar: replace( Lista, Indice, NuevoElem, NuevaLista)

replace([_|T], 1, X, [X|T]).
replace([H|T], I, X, [H|R]) :-
    I > 1,
    I1 is I - 1,
    replace(T, I1, X, R).


% Predicados para obtener las posibilidades de una fila, columna o cuadro

obtener_posibilidades_fila(Posibilidades, Fila, Resultado) :-
    filas_sudoku(Posibilidades, Filas),
    nth1(Fila, Filas, Resultado).

obtener_posibilidades_columna(Posibilidades, Columna, Resultado) :-
    findall(Elem,
            ( between(0,8,I),
              Index is Columna + I * 9,
              nth1(Index, Posibilidades, Elem)
            ),
            Resultado).

obtener_posibilidades_cuadro(Posibilidades, Cuadro, Resultado) :-
    CuadroFila is ((Cuadro - 1) // 3) * 3,
    CuadroColumna is ((Cuadro - 1) mod 3) * 3,
    findall(Elem,
            ( between(0,2,I), between(0,2,J),
              Index is (CuadroFila + I) * 9 + CuadroColumna + J + 1,
              nth1(Index, Posibilidades, Elem)
            ),
            Resultado).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Prueba de Regla 1

probar_regla_1 :-
    sudoku6(Tablero),
    generar_posibilidades(Tablero, Posibilidades),
    writeln("Posibilidades iniciales:"),
    imprimir_sudoku(Posibilidades),
    writeln("Aplicando Regla 3..."),
    aplicar_regla_3(Posibilidades, NuevaPosibilidades),
    
    imprimir_sudoku(NuevaPosibilidades),
    writeln("Posibilidades despues de aplicar la Regla 1 por primera vez:"),
    aplicar_regla_1(NuevaPosibilidades, NuevaPosibilidades2),
    imprimir_sudoku(NuevaPosibilidades2),
    writeln("Posibilidades despues de aplicar la Regla 1 por segunda vez:"),
    aplicar_regla_1(NuevaPosibilidades2, NuevaPosibilidades3),
    imprimir_sudoku(NuevaPosibilidades3),
    writeln("Posibilidades despues de aplicar la Regla 1 por tercera vez:"),
    aplicar_regla_1(NuevaPosibilidades3, NuevaPosibilidades4),
    imprimir_sudoku(NuevaPosibilidades4),
    %%%%%%%%%%%%%%%%%%

    %CREAR FORMA DE DETECTAR QUE 2 LISTAS DE POSIBILIDADES SON IGUALES PARA NO HACER ITERACIONES DE MAS O DE MENOS

    %%%%%%%%%%%%%%%%%

    aplicar_regla_0(Tablero,NuevaPosibilidades4, NuevoTablero),
    writeln("Sudoku resuelto despues de aplicar regla 0:"),
    imprimir_sudoku(NuevoTablero).
    













    
resolver_regla_0(Sudoku, NuevoSudoku) :-
    generar_posibilidades(Sudoku, Posibilidades),
    %resolver_regla_1(Sudoku, NuevoSudoku, Posibilidades, Fin),
    aplicar_regla_0(Sudoku, Posibilidades, NuevoSudoku).

% Predicado para aplicar la Regla 0 una vez
aplicar_regla_0(Sudoku, Posibilidades, NuevoSudoku) :-
    actualizar_sudoku(Sudoku, Posibilidades, SudokuActualizado),
    (   Sudoku \= SudokuActualizado -> 
        resolver_regla_0(SudokuActualizado, NuevoSudoku)  % Continuar aplicando la Regla 0 si hubo cambios
    ;   NuevoSudoku = Sudoku  % Si no hubo cambios, el Sudoku está actualizado
    ).

% Predicado para actualizar el Sudoku con las posibilidades y aplicar la Regla 0
actualizar_sudoku([], [], []).
actualizar_sudoku([C|SudokuResto], [P|PosibilidadesResto], [NuevoC|NuevoSudokuResto]) :-
    (   length(P, 1) -> 
        [NuevoC] = P  % Si solo hay una posibilidad, actualizar la casilla con ese número
    ;   NuevoC = C    % Si no, mantener la casilla original
    ),
    actualizar_sudoku(SudokuResto, PosibilidadesResto, NuevoSudokuResto).

% Predicado para imprimir el sudoku en forma de lista
imprimir_sudoku_lista :-
    sudoku(Tablero),
    writeln(Tablero).

%Predicado para imprimir el sudoku en forma de tablero
imprimir_sudoku_tablero :-
    sudoku(Tablero),
    imprimir_sudoku(Tablero).

%Predicado para generar la lista de posibilidades
generar_lista_posibilidades :-
    sudoku(Tablero),
    generar_posibilidades(Tablero,Posibilidades).

%Predicado para imprimir lista de posibilidades
imprimir_lista_posibilidades :-
    sudoku(Tablero),
    generar_posibilidades(Tablero,Posibilidades),
    %writeln(Posibilidades),
    imprimir_posibilidades(Posibilidades).

%Predicado para probar la Regla 0
probar_regla_0 :-
    sudoku1(Tablero),
    imprimir_sudoku(Tablero),
    generar_posibilidades(Tablero, Posibilidades),
    imprimir_sudoku(Posibilidades),
    writeln("Aplicando Regla 0..."),
    resolver_regla_0(Tablero, NuevoTablero), 
    imprimir_sudoku(NuevoTablero),
    generar_posibilidades(NuevoTablero, PosibilidadesActualizadas),
    imprimir_posibilidades(PosibilidadesActualizadas).



%----------------------REGLA 2------------------------------

% Predicado para obtener las posibilidades de una fila
obtener_posibilidades_fila(Posibilidades, Fila, PosibilidadesFila) :-
    % Calcula el rango de índices correspondientes a la fila
    Inicio is (Fila - 1) * 9 + 1,
    Fin is Fila * 9,
    % Utiliza findall para recopilar las posibilidades de cada celda en la fila
    findall(Posibilidad, (between(Inicio, Fin, Index), nth1(Index, Posibilidades, Posibilidad)), PosibilidadesFila).

% Predicado para obtener las posibilidades de una columna
obtener_posibilidades_columna(Posibilidades, Columna, PosibilidadesColumna) :-
    % Utiliza findall para recopilar las posibilidades de cada celda en la columna
    findall(Posibilidad, (between(0, 8, I), Index is Columna + I * 9, nth1(Index, Posibilidades, Posibilidad)), PosibilidadesColumna).

% Predicado para obtener las posibilidades de un cuadro
obtener_posibilidades_cuadro(Posibilidades, Cuadro, PosibilidadesCuadro) :-
    % Calcula los índices de las celdas en el cuadro
    CuadroFila is (Cuadro - 1) // 3 * 3,
    CuadroColumna is (Cuadro - 1) mod 3 * 3,
    % Utiliza findall para recopilar las posibilidades de cada celda en el cuadro
    findall(Posibilidad, (
        between(0, 2, I), between(0, 2, J),
        Index is (CuadroFila + I) * 9 + CuadroColumna + J + 1,
        nth1(Index, Posibilidades, Posibilidad)
    ), PosibilidadesCuadro).


% Predicado para aplicar la Regla 2
aplicar_regla_2(Posibilidades, NuevaPosibilidades) :-
    % Aplicar la Regla 2 a filas
    aplicar_regla_2_a_todas_filas(Posibilidades, Temp1),
    % Aplicar la Regla 2 a columnas
    aplicar_regla_2_a_todas_columnas(Temp1, Temp2),
    % Aplicar la Regla 2 a cuadros
    aplicar_regla_2_a_todos_cuadros(Temp2, NuevaPosibilidades).

% Predicado para aplicar la Regla 2 a todas las filas
aplicar_regla_2_a_todas_filas(Posibilidades, NuevaPosibilidades) :-
    aplicar_regla_2_a_todas_filas(Posibilidades, 1, NuevaPosibilidades).

% Caso base: cuando se han procesado todas las filas
aplicar_regla_2_a_todas_filas(Posibilidades, 9, Posibilidades).
% Caso recursivo: procesar una fila y continuar con la siguiente
aplicar_regla_2_a_todas_filas(Posibilidades, Fila, NuevaPosibilidades) :-
    % Obtener las posibilidades de la fila actual
    obtener_posibilidades_fila(Posibilidades, Fila, PosibilidadesFila),
    % Encontrar pares únicos en la fila
    encontrar_pares_unicos(PosibilidadesFila, ParesUnicos),
    % Eliminar los pares únicos de la fila
    eliminar_pares_de_fila(Posibilidades, Fila, ParesUnicos, TempPosibilidades),
    % Continuar con la siguiente fila
    FilaSiguiente is Fila + 1,
    aplicar_regla_2_a_todas_filas(TempPosibilidades, FilaSiguiente, NuevaPosibilidades).

% Predicado para aplicar la Regla 2 a todas las columnas
aplicar_regla_2_a_todas_columnas(Posibilidades, NuevaPosibilidades) :-
    aplicar_regla_2_a_todas_columnas(Posibilidades, 1, NuevaPosibilidades).

% Caso base: cuando se han procesado todas las columnas
aplicar_regla_2_a_todas_columnas(Posibilidades, 9, Posibilidades).
% Caso recursivo: procesar una columna y continuar con la siguiente
aplicar_regla_2_a_todas_columnas(Posibilidades, Columna, NuevaPosibilidades) :-
    % Obtener las posibilidades de la columna actual
    obtener_posibilidades_columna(Posibilidades, Columna, PosibilidadesColumna),
    % Encontrar pares únicos en la columna
    encontrar_pares_unicos(PosibilidadesColumna, ParesUnicos),
    % Eliminar los pares únicos de la columna
    eliminar_pares_de_columna(Posibilidades, Columna, ParesUnicos, TempPosibilidades),
    % Continuar con la siguiente columna
    ColumnaSiguiente is Columna + 1,
    aplicar_regla_2_a_todas_columnas(TempPosibilidades, ColumnaSiguiente, NuevaPosibilidades).

% Predicado para aplicar la Regla 2 a todos los cuadros
aplicar_regla_2_a_todos_cuadros(Posibilidades, NuevaPosibilidades) :-
    aplicar_regla_2_a_todos_cuadros(Posibilidades, 1, NuevaPosibilidades).

% Caso base: cuando se han procesado todos los cuadros
aplicar_regla_2_a_todos_cuadros(Posibilidades, 9, Posibilidades).
% Caso recursivo: procesar un cuadro y continuar con el siguiente
aplicar_regla_2_a_todos_cuadros(Posibilidades, Cuadro, NuevaPosibilidades) :-
    % Obtener las posibilidades del cuadro actual
    obtener_posibilidades_cuadro(Posibilidades, Cuadro, PosibilidadesCuadro),
    % Encontrar pares únicos en el cuadro
    encontrar_pares_unicos(PosibilidadesCuadro, ParesUnicos),
    % Eliminar los pares únicos del cuadro
    eliminar_pares_de_cuadro(Posibilidades, Cuadro, ParesUnicos, TempPosibilidades),
    % Continuar con el siguiente cuadro
    CuadroSiguiente is Cuadro + 1,
    aplicar_regla_2_a_todos_cuadros(TempPosibilidades, CuadroSiguiente, NuevaPosibilidades).

% Predicado para encontrar pares de números que aparecen solo en dos casillas
encontrar_pares_unicos(PosibilidadesGrupo, ParesUnicos) :-
    % Utiliza findall para encontrar pares únicos en el grupo
    findall([Num1, Num2], (
        member(Posibilidad, PosibilidadesGrupo),
        length(Posibilidad, 2),
        [Num1, Num2] = Posibilidad,
        contar_ocurrencias(PosibilidadesGrupo, [Num1, Num2], 2)
    ), ParesUnicos).

% Predicado para contar ocurrencias de un par en una lista
contar_ocurrencias([], _, 0).
contar_ocurrencias([H|T], H, N) :-
    contar_ocurrencias(T, H, N1),
    N is N1 + 1.
contar_ocurrencias([_|T], H, N) :-
    contar_ocurrencias(T, H, N).

% Predicado para eliminar pares de una fila
eliminar_pares_de_fila(Posibilidades, Fila, ParesUnicos, NuevaPosibilidades) :-
    % Calcula el rango de índices correspondientes a la fila
    Inicio is (Fila - 1) * 9 + 1,
    Fin is Fila * 9,
    % Utiliza findall para recopilar los índices en el rango
    findall(Index, between(Inicio, Fin, Index), Indices),
    % Llama a eliminar_pares_de_indices para eliminar los pares en esos índices
    eliminar_pares_de_indices(Posibilidades, Indices, ParesUnicos, NuevaPosibilidades).

% Predicado para eliminar pares de una columna
eliminar_pares_de_columna(Posibilidades, Columna, ParesUnicos, NuevaPosibilidades) :-
    % Utiliza findall para recopilar los índices de la columna
    findall(Index, (between(0, 8, I), Index is Columna + I * 9), Indices),
    % Llama a eliminar_pares_de_indices/4 para eliminar los pares en esos índices
    eliminar_pares_de_indices(Posibilidades, Indices, ParesUnicos, NuevaPosibilidades).

% Predicado para eliminar pares de un cuadro
eliminar_pares_de_cuadro(Posibilidades, Cuadro, ParesUnicos, NuevaPosibilidades) :-
    % Calcula los índices de las celdas en el cuadro
    CuadroFila is (Cuadro - 1) // 3 * 3,
    CuadroColumna is (Cuadro - 1) mod 3 * 3,
    % Utiliza findall/3 para recopilar los índices del cuadro
    findall(Index, (
        between(0, 2, I), between(0, 2, J),
        Index is (CuadroFila + I) * 9 + CuadroColumna + J + 1
    ), Indices),
    % Llama a eliminar_pares_de_indices para eliminar los pares en esos índices
    eliminar_pares_de_indices(Posibilidades, Indices, ParesUnicos, NuevaPosibilidades).   

% Predicado para eliminar pares de una lista de índices
eliminar_pares_de_indices(Posibilidades, [], _, Posibilidades).
eliminar_pares_de_indices(Posibilidades, [Indice|RestoIndices], ParesUnicos, NuevaPosibilidades) :-
    % Obtiene la posibilidad actual en el índice dado
    nth1(Indice, Posibilidades, PosibilidadActual),
    % Elimina los pares de la posibilidad actual
    eliminar_pares_de_posibilidad(PosibilidadActual, ParesUnicos, NuevaPosibilidad),
    % Reemplaza la posibilidad actualizada en la lista de posibilidades
    replace(Posibilidades, Indice, NuevaPosibilidad, TempPosibilidades),
    % Continúa con los siguientes índices
    eliminar_pares_de_indices(TempPosibilidades, RestoIndices, ParesUnicos, NuevaPosibilidades).

% Predicado para eliminar pares de una posibilidad
eliminar_pares_de_posibilidad(Posibilidad, [], Posibilidad).
eliminar_pares_de_posibilidad(Posibilidad, [[Num1, Num2]|RestoPares], NuevaPosibilidad) :-
    % Si la posibilidad actual es uno de los pares únicos, no eliminar los números del par
    (   length(Posibilidad, 2), member(Num1, Posibilidad), member(Num2, Posibilidad) ->
        eliminar_pares_de_posibilidad(Posibilidad, RestoPares, NuevaPosibilidad)
    ;   eliminar_numero(Posibilidad, Num1, TempPosibilidad1),
        eliminar_numero(TempPosibilidad1, Num2, TempPosibilidad2),
        eliminar_pares_de_posibilidad(TempPosibilidad2, RestoPares, NuevaPosibilidad)
    ).


% Predicado para eliminar un número de una lista
eliminar_numero([], _, []).
eliminar_numero([H|T], H, T).
eliminar_numero([H|T], Num, [H|R]) :-
    eliminar_numero(T, Num, R).

% Predicado para reemplazar un elemento en una lista en una posición dada
replace([_|T], 1, X, [X|T]).
replace([H|T], N, X, [H|R]) :- N > 1, N1 is N - 1, replace(T, N1, X, R).

% Predicado para probar la Regla 2
probar_regla_2 :-
    % Obtiene el tablero de Sudoku
    sudoku1(Tablero),
    % Genera las posibilidades para el tablero
    generar_posibilidades(Tablero, Posibilidades),
    % Aplica la Regla 2 a las posibilidades
    aplicar_regla_2(Posibilidades, NuevaPosibilidades),
    % Imprime las posibilidades después de aplicar la Regla 2
    writeln("Posibilidades despues de aplicar la Regla 2:"),
    imprimir_sudoku(NuevaPosibilidades).

% Predicado para aplicar las Reglas 0 y 2 de manera iterativa
resolver_reglas_0_y_2(Sudoku, NuevoSudoku) :-
    % Genera las posibilidades para el Sudoku
    generar_posibilidades(Sudoku, Posibilidades),
    % Aplica la Regla 2 a las posibilidades
    aplicar_regla_2(Posibilidades, PosibilidadesRegla2),
    writeln("Posibilidades despues de aplicar la Regla 2:"),
    imprimir_sudoku(PosibilidadesRegla2),
    % Aplica la Regla 0 al Sudoku con las nuevas posibilidades
    aplicar_regla_0(Sudoku, PosibilidadesRegla2, NuevoSudoku).

% Predicado para iterar las Reglas 0 y 2 hasta que no haya mas cambios
iterar_reglas_0_y_2(Sudoku, NuevoSudoku) :-
    % Aplica las Reglas 0 y 2 al Sudoku
    resolver_reglas_0_y_2(Sudoku, SudokuIntermedio),
    % Si hubo cambios en el Sudoku, continúa iterando
    (   Sudoku \= SudokuIntermedio ->
        iterar_reglas_0_y_2(SudokuIntermedio, NuevoSudoku)
    ;   NuevoSudoku = Sudoku  % Si no hubo cambios, el Sudoku está actualizado
    ).

% Predicado para probar las Reglas 0 y 2
probar_reglas_0_y_2 :-
    % Obtiene el tablero de Sudoku
    sudoku1(Tablero),
    imprimir_sudoku(Tablero),
    % Itera las Reglas 0 y 2 hasta que no haya mas cambios
    iterar_reglas_0_y_2(Tablero, NuevoTablero),
    % Imprime el Sudoku después de aplicar las Reglas 0 y 2
    writeln("Sudoku despues de aplicar las Reglas 0 y 2:"),
    imprimir_sudoku(NuevoTablero),
    generar_posibilidades(NuevoTablero, Posibilidades),
    imprimir_sudoku(Posibilidades).


    %----------------------REGLA 3------------------------------

    % Predicado para encontrar tríos de números que aparecen solo en tres casillas
    encontrar_trios_unicos(PosibilidadesGrupo, TriosUnicos) :-
        % Utiliza findall para encontrar tríos únicos en el grupo
        findall([Num1, Num2, Num3], (
            member(Posibilidad, PosibilidadesGrupo),
            length(Posibilidad, 3),
            [Num1, Num2, Num3] = Posibilidad,
            contar_ocurrencias(PosibilidadesGrupo, [Num1, Num2, Num3], 3)
        ), TriosUnicos).

    % Predicado para eliminar tríos de una fila
    eliminar_trios_de_fila(Posibilidades, Fila, TriosUnicos, NuevaPosibilidades) :-
        % Calcula el rango de índices correspondientes a la fila
        Inicio is (Fila - 1) * 9 + 1,
        Fin is Fila * 9,
        % Utiliza findall para recopilar los índices en el rango
        findall(Index, between(Inicio, Fin, Index), Indices),
        % Llama a eliminar_trios_de_indices para eliminar los tríos en esos índices
        eliminar_trios_de_indices(Posibilidades, Indices, TriosUnicos, NuevaPosibilidades).

    % Predicado para eliminar tríos de una columna
    eliminar_trios_de_columna(Posibilidades, Columna, TriosUnicos, NuevaPosibilidades) :-
        % Utiliza findall para recopilar los índices de la columna
        findall(Index, (between(0, 8, I), Index is Columna + I * 9), Indices),
        % Llama a eliminar_trios_de_indices para eliminar los tríos en esos índices
        eliminar_trios_de_indices(Posibilidades, Indices, TriosUnicos, NuevaPosibilidades).

    % Predicado para eliminar tríos de un cuadro
    eliminar_trios_de_cuadro(Posibilidades, Cuadro, TriosUnicos, NuevaPosibilidades) :-
        % Calcula los índices de las celdas en el cuadro
        CuadroFila is (Cuadro - 1) // 3 * 3,
        CuadroColumna is (Cuadro - 1) mod 3 * 3,
        % Utiliza findall para recopilar los índices del cuadro
        findall(Index, (
            between(0, 2, I), between(0, 2, J),
            Index is (CuadroFila + I) * 9 + CuadroColumna + J + 1
        ), Indices),
        % Llama a eliminar_trios_de_indices para eliminar los tríos en esos índices
        eliminar_trios_de_indices(Posibilidades, Indices, TriosUnicos, NuevaPosibilidades).

    % Predicado para eliminar tríos de una lista de índices
    eliminar_trios_de_indices(Posibilidades, [], _, Posibilidades).
    eliminar_trios_de_indices(Posibilidades, [Indice|RestoIndices], TriosUnicos, NuevaPosibilidades) :-
        % Obtiene la posibilidad actual en el índice dado
        nth1(Indice, Posibilidades, PosibilidadActual),
        % Elimina los tríos de la posibilidad actual
        eliminar_trios_de_posibilidad(PosibilidadActual, TriosUnicos, NuevaPosibilidad),
        % Reemplaza la posibilidad actualizada en la lista de posibilidades
        replace(Posibilidades, Indice, NuevaPosibilidad, TempPosibilidades),
        % Continúa con los siguientes índices
        eliminar_trios_de_indices(TempPosibilidades, RestoIndices, TriosUnicos, NuevaPosibilidades).

    % Predicado para eliminar tríos de una posibilidad
    eliminar_trios_de_posibilidad(Posibilidad, [], Posibilidad).
    eliminar_trios_de_posibilidad(Posibilidad, [[Num1, Num2, Num3]|RestoTrios], NuevaPosibilidad) :-
        % Si la posibilidad actual es uno de los tríos únicos, no eliminar los números del trío
        (   length(Posibilidad, 3), member(Num1, Posibilidad), member(Num2, Posibilidad), member(Num3, Posibilidad) ->
            eliminar_trios_de_posibilidad(Posibilidad, RestoTrios, NuevaPosibilidad)
        ;   eliminar_numero(Posibilidad, Num1, TempPosibilidad1),
            eliminar_numero(TempPosibilidad1, Num2, TempPosibilidad2),
            eliminar_numero(TempPosibilidad2, Num3, TempPosibilidad3),
            eliminar_trios_de_posibilidad(TempPosibilidad3, RestoTrios, NuevaPosibilidad)
        ).

    % Predicado para aplicar la Regla 3
    aplicar_regla_3(Posibilidades, NuevaPosibilidades) :-
        % Aplicar la Regla 3 a filas
        aplicar_regla_3_a_todas_filas(Posibilidades, Temp1),
        % Aplicar la Regla 3 a columnas
        aplicar_regla_3_a_todas_columnas(Temp1, Temp2),
        % Aplicar la Regla 3 a cuadros
        aplicar_regla_3_a_todos_cuadros(Temp2, NuevaPosibilidades).

    % Predicado para aplicar la Regla 3 a todas las filas
    aplicar_regla_3_a_todas_filas(Posibilidades, NuevaPosibilidades) :-
        aplicar_regla_3_a_todas_filas(Posibilidades, 1, NuevaPosibilidades).

    % Caso base: cuando se han procesado todas las filas
    aplicar_regla_3_a_todas_filas(Posibilidades, 9, Posibilidades).
    % Caso recursivo: procesar una fila y continuar con la siguiente
    aplicar_regla_3_a_todas_filas(Posibilidades, Fila, NuevaPosibilidades) :-
        % Obtener las posibilidades de la fila actual
        obtener_posibilidades_fila(Posibilidades, Fila, PosibilidadesFila),
        % Encontrar tríos únicos en la fila
        encontrar_trios_unicos(PosibilidadesFila, TriosUnicos),
        % Eliminar los tríos únicos de la fila
        eliminar_trios_de_fila(Posibilidades, Fila, TriosUnicos, TempPosibilidades),
        % Continuar con la siguiente fila
        FilaSiguiente is Fila + 1,
        aplicar_regla_3_a_todas_filas(TempPosibilidades, FilaSiguiente, NuevaPosibilidades).

    % Predicado para aplicar la Regla 3 a todas las columnas
    aplicar_regla_3_a_todas_columnas(Posibilidades, NuevaPosibilidades) :-
        aplicar_regla_3_a_todas_columnas(Posibilidades, 1, NuevaPosibilidades).

    % Caso base: cuando se han procesado todas las columnas
    aplicar_regla_3_a_todas_columnas(Posibilidades, 9, Posibilidades).
    % Caso recursivo: procesar una columna y continuar con la siguiente
    aplicar_regla_3_a_todas_columnas(Posibilidades, Columna, NuevaPosibilidades) :-
        % Obtener las posibilidades de la columna actual
        obtener_posibilidades_columna(Posibilidades, Columna, PosibilidadesColumna),
        % Encontrar tríos únicos en la columna
        encontrar_trios_unicos(PosibilidadesColumna, TriosUnicos),
        % Eliminar los tríos únicos de la columna
        eliminar_trios_de_columna(Posibilidades, Columna, TriosUnicos, TempPosibilidades),
        % Continuar con la siguiente columna
        ColumnaSiguiente is Columna + 1,
        aplicar_regla_3_a_todas_columnas(TempPosibilidades, ColumnaSiguiente, NuevaPosibilidades).

    % Predicado para aplicar la Regla 3 a todos los cuadros
    aplicar_regla_3_a_todos_cuadros(Posibilidades, NuevaPosibilidades) :-
        aplicar_regla_3_a_todos_cuadros(Posibilidades, 1, NuevaPosibilidades).

    % Caso base: cuando se han procesado todos los cuadros
    aplicar_regla_3_a_todos_cuadros(Posibilidades, 9, Posibilidades).
    % Caso recursivo: procesar un cuadro y continuar con el siguiente
    aplicar_regla_3_a_todos_cuadros(Posibilidades, Cuadro, NuevaPosibilidades) :-
        % Obtener las posibilidades del cuadro actual
        obtener_posibilidades_cuadro(Posibilidades, Cuadro, PosibilidadesCuadro),
        % Encontrar tríos únicos en el cuadro
        encontrar_trios_unicos(PosibilidadesCuadro, TriosUnicos),
        % Eliminar los tríos únicos del cuadro
        eliminar_trios_de_cuadro(Posibilidades, Cuadro, TriosUnicos, TempPosibilidades),
        % Continuar con el siguiente cuadro
        CuadroSiguiente is Cuadro + 1,
        aplicar_regla_3_a_todos_cuadros(TempPosibilidades, CuadroSiguiente, NuevaPosibilidades).

    % Predicado para probar la Regla 3
    probar_regla_3 :-
        % Obtiene el tablero de Sudoku
        sudoku1(Tablero),
        % Genera las posibilidades para el tablero
        generar_posibilidades(Tablero, Posibilidades),
        imprimir_sudoku(Posibilidades),
        % Aplica la Regla 3 a las posibilidades
        aplicar_regla_3(Posibilidades, NuevaPosibilidades),
        % Imprime las posibilidades después de aplicar la Regla 3
        writeln("Posibilidades despues de aplicar la Regla 3:"),
        imprimir_sudoku(NuevaPosibilidades).


resolver_reglas_0_y_1_y_2_y_3(Sudoku, NuevoSudoku) :-
    % Genera las posibilidades para el Sudoku
    generar_posibilidades(Sudoku, Posibilidades),


    % Genera las posibilidades para el tablero
    imprimir_sudoku(Posibilidades),
    % Aplica la Regla 3 a las posibilidades
    aplicar_regla_3(Posibilidades, PosibilidadesRegla3),
    % Imprime las posibilidades después de aplicar la Regla 3
    writeln("Posibilidades despues de aplicar la Regla 3:"),
    imprimir_sudoku(PosibilidadesRegla3),
    % Aplica la Regla 2 a las posibilidades
    aplicar_regla_2(PosibilidadesRegla3, PosibilidadesRegla2),

    writeln("Posibilidades despues de aplicar la Regla 2:"),
    imprimir_sudoku(PosibilidadesRegla2),

    aplicar_regla_1(PosibilidadesRegla2, Fin),
    writeln("Posibilidades despues de aplicar la Regla 1:"),
    imprimir_sudoku(Fin),
    % Aplica la Regla 0 al Sudoku con las nuevas posibilidades
    aplicar_regla_0(Sudoku, Fin, NuevoSudoku).



iterar_reglas_0_y_1_y_2_y_3(Sudoku, NuevoSudoku) :-
    % Aplica las Reglas 0 y 2 al Sudoku
    resolver_reglas_0_y_1_y_2_y_3(Sudoku, SudokuIntermedio),
    % Si hubo cambios en el Sudoku, continúa iterando
    (   Sudoku \= SudokuIntermedio ->
        iterar_reglas_0_y_1_y_2_y_3(SudokuIntermedio, NuevoSudoku)
    ;   NuevoSudoku = Sudoku  % Si no hubo cambios, el Sudoku está actualizado
    ).


probar_reglas_0_y_1_y_2_y_3 :-
    % Obtiene el tablero de Sudoku
    sudoku4(Tablero),
    imprimir_sudoku(Tablero),
    % Itera las Reglas 0 y 1 y 2 hasta que no haya mas cambios
    iterar_reglas_0_y_1_y_2_y_3(Tablero, NuevoTablero),
    % Imprime el Sudoku después de aplicar las Reglas 0 y 1 y 2
    writeln("Sudoku despues de aplicar las Reglas 0 y 1 y 2 y 3:"),
    imprimir_sudoku(NuevoTablero),
    generar_posibilidades(NuevoTablero, Posibilidades),
    imprimir_sudoku(Posibilidades).