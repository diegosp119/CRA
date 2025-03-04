% Predicado para aplicar la Regla 0 y actualizar el Sudoku
%FALTARIA UNIR CON REGLA 0
resolver_regla_1(Sudoku, NuevoSudoku, Posibilidades, Fin) :-
    %generar_posibilidades(Sudoku, Posibilidades),
    ajuste_de_posibilidades(Posibilidades, PosibilidadesAjustadas),
    writeln("Posibilidades: "), writeln(Posibilidades),
    aplicar_regla_1(PosibilidadesAjustadas, FilaUnicos, ColumnaUnicos, CuadradoUnicos,FilaDef, ColumnaDef, Cuadrados_Def, NuevaPosibilidades),
    ajuste_de_posibilidades(NuevaPosibilidades, Fin),
    writeln(" Posibilidades Depuradas: "), writeln(Fin).
    
resolver_regla_0(Sudoku, NuevoSudoku) :-
    generar_posibilidades(Sudoku, Posibilidades),
    %LLAMADAREGLA1
    resolver_regla_1(Sudoku, NuevoSudoku, Posibilidades, Fin),
    aplicar_regla_0(Sudoku, Fin, NuevoSudoku).

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
    sudoku2(Tablero),
    %resolver_regla_1(Tablero, NuevoTablero). %ESTE PUNTO AQUI SOBRE RECUERDA
    resolver_regla_0(Tablero, NuevoTablero), 
    imprimir_sudoku(NuevoTablero),
    generar_posibilidades(NuevoTablero, PosibilidadesActualizadas),
    imprimir_posibilidades(PosibilidadesActualizadas).