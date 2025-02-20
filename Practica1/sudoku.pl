% Representación del Sudoku como una lista de 81 elementos


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

% Predicado para verificar si un número está en el mismo cuadro 3x3
miembro_cuadro(Sudoku, Index, Num) :-
    Fila is Index // 9,
    Columna is Index mod 9,
    CuadroFila is (Fila // 3) * 3,
    CuadroColumna is (Columna // 3) * 3,
    FInicio is CuadroFila * 9 + CuadroColumna,
    FFin is FInicio + 2,
    CInicio is CuadroFila * 9 + CuadroColumna + 18,
    CFin is CInicio + 2,
    (between(FInicio, FFin, I); between(CInicio, CFin, I)),
    nth0(I, Sudoku, Num).

% Predicado para verificar si una casilla está vacía
casilla_vacia(Casilla) :-
    \+ number(Casilla).

% Predicado para obtener las posibilidades para cada casilla vacía o generar una lista vacía si la casilla está ocupada
posibilidades_casilla(Sudoku, Index, Posibilidades) :-
    nth0(Index, Sudoku, Casilla),
    (   casilla_vacia(Casilla) -> 
        findall(Num, ((Num), \+ miembro_fila(Sudoku, Index, Num), \+ miembro_columna(Sudoku, Index, Num), \+ miembro_cuadro(Sudoku, Index, Num)), Posibilidades)
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


% Predicado para aplicar la Regla 0 y actualizar el Sudoku
resolver_regla_0(Sudoku, NuevoSudoku) :-
    generar_posibilidades(Sudoku, Posibilidades),
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
    sudoku(Tablero),
    resolver_regla_0(Tablero, NuevoTablero),
    imprimir_sudoku(NuevoTablero),
    generar_posibilidades(NuevoTablero, PosibilidadesActualizadas),
    imprimir_posibilidades(PosibilidadesActualizadas).









%REEEEGLAAAA 111111111111



% Predicado para aplicar la Regla 1 y actualizar el Sudoku
resolver_regla_1(Sudoku, NuevoSudoku) :-
    generar_posibilidades(Sudoku, Posibilidades),
    imprimir_posibilidades(Posibilidades),
    aplicar_regla_1(Sudoku, Posibilidades, NuevoSudoku).

% Predicado para aplicar la Regla 1 una vez
aplicar_regla_1(Sudoku, Posibilidades, NuevoSudoku) :-
    actualizar_sudoku_regla_1(Sudoku, Posibilidades, SudokuActualizado),
    (   Sudoku \= SudokuActualizado -> 
        resolver_regla_1(SudokuActualizado, NuevoSudoku)  % Continuar aplicando la Regla 1 si hubo cambios
    ;   NuevoSudoku = Sudoku  % Si no hubo cambios, el Sudoku está actualizado
    ).

% Predicado para actualizar el Sudoku con las posibilidades y aplicar la Regla 1
actualizar_sudoku_regla_1(Sudoku, Posibilidades, NuevoSudoku) :-
    actualizar_filas(Sudoku, Posibilidades, Sudoku1),
    actualizar_columnas(Sudoku1, Posibilidades, Sudoku2),
    actualizar_cuadros(Sudoku2, Posibilidades, NuevoSudoku).





% Predicado para actualizar las filas del Sudoku aplicando la Regla 1
actualizar_filas(Sudoku, Posibilidades, NuevoSudoku) :-
    actualizar_filas(Sudoku, Posibilidades, 0, NuevoSudoku).

% Caso base: si hemos procesado todas las filas, terminamos
actualizar_filas(Sudoku, _, 9, Sudoku).






% Caso recursivo: procesar cada fila
actualizar_filas(Sudoku, Posibilidades, Fila, NuevoSudoku) :-
    Inicio is Fila * 9,
    Fin is Inicio + 8,
    actualizar_fila(Sudoku, Posibilidades, Inicio, Fin, SudokuActualizado),
    FilaSiguiente is Fila + 1,
    actualizar_filas(SudokuActualizado, Posibilidades, FilaSiguiente, NuevoSudoku).

% Predicado para actualizar una fila
actualizar_fila(Sudoku, Posibilidades, Inicio, Fin, NuevoSudoku) :-
    findall(Num, (numero(Num), contar_ocurrencias(Posibilidades, Inicio, Fin, Num, 1)), Unicos),
    actualizar_fila_con_unicos(Sudoku, Posibilidades, Inicio, Fin, Unicos, NuevoSudoku).

% Predicado para contar las ocurrencias de un número en una fila
contar_ocurrencias(Posibilidades, Inicio, Fin, Num, Count) :-
    findall(Index, (between(Inicio, Fin, Index), nth0(Index, Posibilidades, Lista), member(Num, Lista)), Indices),
    length(Indices, Count).

% Predicado para actualizar una fila con números únicos
actualizar_fila_con_unicos(Sudoku, Posibilidades, Inicio, Fin, [], Sudoku).
actualizar_fila_con_unicos(Sudoku, Posibilidades, Inicio, Fin, [Num|Resto], NuevoSudoku) :-
    findall(Index, (between(Inicio, Fin, Index), nth0(Index, Posibilidades, Lista), member(Num, Lista)), [Index]),
    nth0(Index, Sudoku, .),
    nth0(Index, NuevoSudoku, Num, Sudoku),
    eliminar_numero_de_posibilidades(Posibilidades, Inicio, Fin, Num, PosibilidadesActualizadas),
    actualizar_fila_con_unicos(NuevoSudoku, PosibilidadesActualizadas, Inicio, Fin, Resto, NuevoSudoku).

% Predicado para eliminar un número de las listas de posibilidades en una fila
eliminar_numero_de_posibilidades([], _, _, _, []).
eliminar_numero_de_posibilidades([P|Ps], Inicio, Fin, Num, [PActualizado|Resto]) :-
    nth0(Index, [P|Ps], P),
    (   between(Inicio, Fin, Index) ->
        delete(P, Num, PActualizado)
    ;   PActualizado = P
    ),
    eliminar_numero_de_posibilidades(Ps, Inicio, Fin, Num, Resto).


% Predicado para probar la Regla 1
probar_regla_1 :-
    sudoku(Tablero),
    resolver_regla_1(Tablero, NuevoTablero),
    imprimir_sudoku(NuevoTablero),
    generar_posibilidades(NuevoTablero, PosibilidadesActualizadas),
    imprimir_posibilidades(PosibilidadesActualizadas).



% Predicado para actualizar las columnas
actualizar_columnas(Sudoku, Posibilidades, NuevoSudoku) :-
    % Implementar la lógica para actualizar las columnas
    % ...
    NuevoSudoku = Sudoku.  % Placeholder

% Predicado para actualizar los cuadros
actualizar_cuadros(Sudoku, Posibilidades, NuevoSudoku) :-
    % Implementar la lógica para actualizar los cuadros
    % ...
    NuevoSudoku = Sudoku.  % Placeholder