
% -------------------------------------------------------------------------------
% Verificación Extendida Considerando las Posibilidades Precalculadas
% -------------------------------------------------------------------------------
% En region_valida se verifica que para cada casilla vacía la lista
% de posibilidades proporcionada coincida exactamente con las posibilidades calculadas.
% Esto detecta errores como la inclusión de un número cuando no debe aparecer.
% -------------------------------------------------------------------------------
region_valida(Indices, Sudoku, PosList) :-
    % Extraer los números fijos de la región y verificar que no se repitan.
    findall(E, (member(I, Indices), nth0(I, Sudoku, E), number(E)), Fijos),
    sort(Fijos, FijosSorted),
    length(Fijos, LFix),
    length(FijosSorted, LFixSorted),
    LFix =:= LFixSorted,
    % Para cada casilla vacía, comprobamos que su lista de posibilidades
    % es exactamente la que se obtiene de calcularlas.
    forall((member(I, Indices), nth0(I, Sudoku, E), \+ number(E)),
           ( nth0(I, PosList, Poss),
             posibilidades_casilla(Sudoku, I, CorrectPoss),
             sort(Poss, SortedPoss),
             sort(CorrectPoss, SortedCorrect),
             SortedPoss == SortedCorrect
           )
          ).

% -------------------------------------------------------------------------------
% verificar_filas_poss/2: Verifica que todas las filas sean válidas
% considerando las posibilidades precalculadas.
% -------------------------------------------------------------------------------
verificar_filas_poss(Sudoku, PosList) :-
    forall(between(0, 8, Row),
           (
             Inicio is Row * 9,
             Fin is Inicio + 8,
             findall(I, between(Inicio, Fin, I), Indices),
             ( region_valida(Indices, Sudoku, PosList) ->
                 true
             ;   format('Error en la fila ~w~n', [Row])
             )
           )
    ).

% -------------------------------------------------------------------------------
% verificar_columnas_poss/2: Verifica que todas las columnas sean válidas
% considerando las posibilidades precalculadas.
% -------------------------------------------------------------------------------
verificar_columnas_poss(Sudoku, PosList) :-
    forall(between(0, 8, Col),
           (
             findall(I, (between(0,8,Row), I is Row * 9 + Col), Indices),
             ( region_valida(Indices, Sudoku, PosList) ->
                 true
             ;   format('Error en la columna ~w~n', [Col])
             )
           )
    ).

% -------------------------------------------------------------------------------
% verificar_cuadrantes_poss/2: Verifica que todos los bloques 3x3 sean válidos
% considerando las posibilidades precalculadas.
% -------------------------------------------------------------------------------
verificar_cuadrantes_poss(Sudoku, PosList) :-
    forall((member(BR, [0,3,6]), member(BC, [0,3,6])),
           (
             findall(I,
                     (between(0,2,DR), between(0,2,DC),
                      I is (BR+DR)*9+(BC+DC)
                     ),
                     Indices),
             ( region_valida(Indices, Sudoku, PosList) ->
                 true
             ;   format('Error en el cuadro con esquina en (~w,~w)~n', [BR, BC])
             )
           )
    ).

% -------------------------------------------------------------------------------
% verificar_sudoku_poss/0: Predicado principal para verificar el estado actual
% del Sudoku considerando las posibilidades precalculadas.
% Se genera la lista de posibilidades, se imprime, se introduce un error
% añadiendo un 9 extra en la lista de posibilidades de la primera casilla vacía,
% y luego se procede a verificar filas, columnas y bloques.
% -------------------------------------------------------------------------------
verificar_sudoku_poss :-
    sudoku5(Sudoku),
    generar_posibilidades(Sudoku, PosListOriginal),
    writeln('Lista de posibilidades original:'),
    writeln(PosListOriginal),
    
    % Introducir un error: Añadir un 9 a la lista de posibilidades de la primera casilla vacía
    PosListOriginal = [P1 | RestoPosList],
    append(P1, [9], P1Erroneo),
    PosListErronea = [P1Erroneo | RestoPosList],
    
    writeln('Lista de posibilidades modificada (con error):'),
    writeln(PosListErronea),


    
    % Verificar el Sudoku con la lista de posibilidades errónea.
    verificar_filas_poss(Sudoku, PosListErronea),
    verificar_columnas_poss(Sudoku, PosListErronea),
    verificar_cuadrantes_poss(Sudoku, PosListErronea),
    writeln('El Sudoku es válido considerando las posibilidades.').
    

