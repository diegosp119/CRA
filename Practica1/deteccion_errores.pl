% -------------------------------------------------------------------------------
% Definición de Tableros (Ejemplos)
% -------------------------------------------------------------------------------
sudoku2([
    9,2,3,4,5,6,7,8,9,  
    4,5,6,7,8,9,1,2,3,  
    7,8,9,1,2,3,4,5,6,  
    2,3,4,5,6,7,8,9,1,  
    5,6,7,8,9,1,2,3,4,  
    8,9,1,2,3,4,5,6,7,  
    3,4,5,6,7,8,9,1,2,  
    6,7,8,9,1,2,3,4,5,  
    9,1,2,3,4,5,6,7,8
]).

sudoku1([
    ., ., 9, 6, ., ., ., 1, .,
    8, ., ., ., ., 1, ., 9, .,
    7, ., ., ., ., ., ., ., 8,
    ., 3, ., ., 6, ., ., ., .,
    ., 4, ., 1, ., 9, ., ., 5,
    9, ., ., ., ., ., ., ., .,
    ., 8, ., 9, ., ., 5, 4, .,
    6, ., ., 7, 1, ., ., ., 3,
    ., ., 5, ., 8, 4, ., ., 9
]).

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

% -------------------------------------------------------------------------------
% Predicados para gestionar las posibilidades de cada casilla en el Sudoku
% -------------------------------------------------------------------------------

% -------------------------------------------------------------------------------
% numero/1: Hechos que representan los dígitos válidos en un Sudoku (1 a 9).
% -------------------------------------------------------------------------------
numero(1).
numero(2).
numero(3).
numero(4).
numero(5).
numero(6).
numero(7).
numero(8).
numero(9).

% -------------------------------------------------------------------------------
% miembro_fila/3: Comprueba si un número (Num) ya se encuentra en la misma fila
% de la casilla indicada por Index.
% -------------------------------------------------------------------------------
miembro_fila(Sudoku, Index, Num) :-
    Fila is Index // 9,
    Inicio is Fila * 9,
    Fin is Inicio + 8,
    between(Inicio, Fin, I),
    nth0(I, Sudoku, Num).

% -------------------------------------------------------------------------------
% miembro_columna/3: Comprueba si un número (Num) ya se encuentra en la misma
% columna de la casilla indicada por Index.
% -------------------------------------------------------------------------------
miembro_columna(Sudoku, Index, Num) :-
    Columna is Index mod 9,
    between(0, 8, Fila),
    I is Fila * 9 + Columna,
    nth0(I, Sudoku, Num).

% -------------------------------------------------------------------------------
% miembro_cuadro/3: Comprueba si un número (Num) ya se encuentra en el mismo
% cuadro 3x3 de la casilla indicada por Index.
% -------------------------------------------------------------------------------
miembro_cuadro(Sudoku, Index, Num) :-
    Fila is Index // 9,
    Columna is Index mod 9,
    CuadroFila is (Fila // 3) * 3,
    CuadroColumna is (Columna // 3) * 3,
    between(0, 2, DF),
    between(0, 2, DC),
    I is (CuadroFila + DF) * 9 + (CuadroColumna + DC),
    nth0(I, Sudoku, Num).

% -------------------------------------------------------------------------------
% casilla_vacia/1: Verifica si una casilla está vacía (no es un número).
% -------------------------------------------------------------------------------
casilla_vacia(Casilla) :-
    \+ number(Casilla).

% -------------------------------------------------------------------------------
% posibilidades_casilla/3: Obtiene la lista de posibilidades para una casilla.
% -------------------------------------------------------------------------------
posibilidades_casilla(Sudoku, Index, Posibilidades) :-
    nth0(Index, Sudoku, Casilla),
    (   casilla_vacia(Casilla) ->
        findall(Num,
                (numero(Num),
                 \+ miembro_fila(Sudoku, Index, Num),
                 \+ miembro_columna(Sudoku, Index, Num),
                 \+ miembro_cuadro(Sudoku, Index, Num)
                ),
                Posibilidades)
    ;   Posibilidades = []  % Si la casilla ya tiene un número, no hay posibilidades.
    ).

% -------------------------------------------------------------------------------
% generar_posibilidades/2: Genera la lista de posibilidades para cada casilla del Sudoku.
% -------------------------------------------------------------------------------
generar_posibilidades(Sudoku, Posibilidades) :-
    generar_posibilidades(Sudoku, 0, Posibilidades).

generar_posibilidades(_, 81, []).
generar_posibilidades(Sudoku, Index, [P|Resto]) :-
    Index < 81,
    posibilidades_casilla(Sudoku, Index, P),
    NextIndex is Index + 1,
    generar_posibilidades(Sudoku, NextIndex, Resto).

% -------------------------------------------------------------------------------
% Verificación Extendida Considerando las Posibilidades Precalculadas
% -------------------------------------------------------------------------------
% Ahora, en region_valida se verifica que para cada casilla vacía la lista
% de posibilidades proporcionada coincida exactamente con las posibilidades calculadas.
% Esto detecta errores como la inclusión de un 9 cuando no debe aparecer.
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
% del Sudoku (en este caso, sudoku1) considerando las posibilidades precalculadas.
% Se genera la lista de posibilidades, se imprime, se introduce un error
% añadiendo un 9 extra en la lista de posibilidades de la primera casilla vacía,
% y luego se procede a verificar filas, columnas y bloques.
% -------------------------------------------------------------------------------
verificar_sudoku_poss :-
    sudoku1(Sudoku),
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
    
% -------------------------------------------------------------------------------
% generar_lista_posibilidades/0: Genera e imprime por consola la lista de posibilidades
% para cada casilla del Sudoku definido en sudoku/1.
% -------------------------------------------------------------------------------
generar_lista_posibilidades :-
    sudoku1(Tablero),
    generar_posibilidades(Tablero, Posibilidades),
    writeln(Posibilidades).
