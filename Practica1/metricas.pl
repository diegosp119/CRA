

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Resolver la Regla 1 con contador
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

resolver_regla_1(Posibilidades, Fin, CountIn, CountOut) :-
    aplicar_regla_1(Posibilidades, _FilaUnicos, _ColumnaUnicos, _CuadradoUnicos,
                    _FilaDef, _ColumnaDef, _Cuadrados_Def, NuevaPosibilidades, CountIn, CountOut),
    Fin = NuevaPosibilidades.

% Wrapper sin contador para resolver_regla_1
resolver_regla_1(Posibilidades, Fin) :-
    resolver_regla_1(Posibilidades, Fin, 0, _).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Resolver las Reglas 0 y 1 de manera iterativa con contador
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

resolver_reglas_0_y_1(Sudoku, NuevoSudoku, CountIn, CountOut) :-
    generar_posibilidades(Sudoku, Posibilidades),
    resolver_regla_1(Posibilidades, Fin, CountIn, CountTemp),
    aplicar_regla_0(Sudoku, Fin, NuevoSudoku, CountTemp, CountOut).

% Wrapper sin contador explícito para resolver_reglas_0_y_1
resolver_reglas_0_y_1(Sudoku, NuevoSudoku) :-
    resolver_reglas_0_y_1(Sudoku, NuevoSudoku, 0, _).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Iterar las Reglas 0 y 1 de manera iterativa con contador
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

iterar_reglas_0_y_1(Sudoku, NuevoSudoku, IterCountIn, IterCountOut) :-
    resolver_reglas_0_y_1(Sudoku, SudokuIntermedio),
    NewIterCount is IterCountIn + 1,
    (   Sudoku \= SudokuIntermedio ->
            iterar_reglas_0_y_1(SudokuIntermedio, NuevoSudoku, NewIterCount, IterCountOut)
    ;   NuevoSudoku = Sudoku,
        IterCountOut = NewIterCount
    ).

% Wrapper que inicia el contador de iteraciones en 0
iterar_reglas_0_y_1(Sudoku, NuevoSudoku, IterCount) :-
    iterar_reglas_0_y_1(Sudoku, NuevoSudoku, 0, IterCount).

