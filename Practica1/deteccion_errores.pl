sudoku2([
    1,2,3,4,5,6,7,8,9,  
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

% -------------------------- Predicados para Posibilidades --------------------------

% Hechos para representar los números del 1 al 9.
numero(1).
numero(2).
numero(3).
numero(4).
numero(5).
numero(6).
numero(7).
numero(8).
numero(9).

% Comprueba si un número ya se encuentra en la fila.
miembro_fila(Sudoku, Index, Num) :-
    Fila is Index // 9,
    Inicio is Fila * 9,
    Fin is Inicio + 8,
    between(Inicio, Fin, I),
    nth0(I, Sudoku, Num).

% Comprueba si un número ya se encuentra en la columna.
miembro_columna(Sudoku, Index, Num) :-
    Columna is Index mod 9,
    between(0, 8, Fila),
    I is Fila * 9 + Columna,
    nth0(I, Sudoku, Num).

% Comprueba si un número ya se encuentra en el cuadro 3x3.
miembro_cuadro(Sudoku, Index, Num) :-
    Fila is Index // 9,
    Columna is Index mod 9,
    CuadroFila is (Fila // 3) * 3,
    CuadroColumna is (Columna // 3) * 3,
    between(0, 2, DF),
    between(0, 2, DC),
    I is (CuadroFila + DF) * 9 + (CuadroColumna + DC),
    nth0(I, Sudoku, Num).

% Determina si una casilla está vacía (no es un número).
casilla_vacia(Casilla) :-
    \+ number(Casilla).

% Obtiene la lista de posibilidades para una casilla vacía (o lista vacía si ya tiene número).
posibilidades_casilla(Sudoku, Index, Posibilidades) :-
    nth0(Index, Sudoku, Casilla),
    (   casilla_vacia(Casilla) -> 
        findall(Num, (numero(Num),
                      \+ miembro_fila(Sudoku, Index, Num),
                      \+ miembro_columna(Sudoku, Index, Num),
                      \+ miembro_cuadro(Sudoku, Index, Num)
                     ), Posibilidades)
    ;   Posibilidades = []
    ).

% Genera la lista de posibilidades para todas las casillas del Sudoku.
generar_posibilidades(Sudoku, Posibilidades) :-
    generar_posibilidades(Sudoku, 0, Posibilidades).

generar_posibilidades(_, 81, []).
generar_posibilidades(Sudoku, Index, [P|Resto]) :-
    Index < 81,
    posibilidades_casilla(Sudoku, Index, P),
    NextIndex is Index + 1,
    generar_posibilidades(Sudoku, NextIndex, Resto).

% Genera e imprime la lista de posibilidades para el Sudoku.
generar_lista_posibilidades :-
    sudoku(Tablero),
    generar_posibilidades(Tablero, Posibilidades),
    writeln(Posibilidades).


% ---------------- Verificación Extendida Considerando Posibilidades ----------------

% Predicado que verifica que una región (fila, columna o cuadro) sea válida:
% 1. No hay números repetidos en las casillas fijas.
% 2. Cada casilla vacía debe tener al menos una posibilidad.
% 3. Cada número que falta en la región debe poder colocarse en al menos una casilla vacía.
region_valida(Indices, Sudoku) :-
    % Extrae los números fijos de la región y verifica que no se repitan.
    findall(E, (member(I, Indices), nth0(I, Sudoku, E), number(E)), Fijos),
    sort(Fijos, FijosSorted),
    length(Fijos, LFix),
    length(FijosSorted, LFixSorted),
    LFix =:= LFixSorted,
    % Cada casilla vacía debe tener alguna posibilidad.
    forall((member(I, Indices), nth0(I, Sudoku, E), \+ number(E)),
           (posibilidades_casilla(Sudoku, I, Poss), Poss \= [])),
    % Cada número faltante debe estar entre las posibilidades de alguna casilla vacía.
    forall((between(1, 9, Num), \+ member(Num, FijosSorted)),
           (member(I, Indices),
            nth0(I, Sudoku, E), \+ number(E),
            posibilidades_casilla(Sudoku, I, Poss),
            member(Num, Poss)
           )
    ).

% Verifica todas las filas del Sudoku considerando las posibilidades.
verificar_filas_poss(Sudoku) :-
    forall(between(0, 8, Row),
           (
             Inicio is Row * 9,
             Fin is Inicio + 8,
             findall(I, between(Inicio, Fin, I), Indices),
             ( region_valida(Indices, Sudoku) ->
                 true
             ;   format('Error en la fila ~w: índices ~w~n', [Row, Indices])
             )
           )
    ).

% Verifica todas las columnas del Sudoku considerando las posibilidades.
verificar_columnas_poss(Sudoku) :-
    forall(between(0, 8, Col),
           (
             findall(I, (between(0,8,Row), I is Row * 9 + Col), Indices),
             ( region_valida(Indices, Sudoku) ->
                 true
             ;   format('Error en la columna ~w: índices ~w~n', [Col, Indices])
             )
           )
    ).

% Verifica todos los cuadros (bloques 3x3) del Sudoku considerando las posibilidades.
verificar_cuadrantes_poss(Sudoku) :-
    forall((member(BR, [0,3,6]), member(BC, [0,3,6])),
           (
             findall(I, (between(0,2,DR), between(0,2,DC), I is (BR+DR)*9+(BC+DC)), Indices),
             ( region_valida(Indices, Sudoku) ->
                 true
             ;   format('Error en el cuadro con esquina en (~w,~w): índices ~w~n', [BR, BC, Indices])
             )
           )
    ).

% Predicado principal para verificar el estado actual del Sudoku (parcial o completo)
% considerando las posibilidades de cada casilla.
verificar_sudoku_poss:-
    sudoku(Sudoku),
    verificar_filas_poss(Sudoku),
    verificar_columnas_poss(Sudoku),
    verificar_cuadrantes_poss(Sudoku),
    writeln('El Sudoku es válido (considerando posibilidades).').