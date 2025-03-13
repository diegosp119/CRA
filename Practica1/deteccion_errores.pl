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

% -------------------------------------------------------------------------------
% Predicados para gestionar las posibilidades de cada casilla en el Sudoku
% -------------------------------------------------------------------------------

% -------------------------------------------------------------------------------
% numero/1: Hechos que representan los dígitos válidos en un Sudoku (1 a 9).
% Esto nos permite iterar fácilmente sobre los números posibles.
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
% de la casilla indicada por Index dentro del Sudoku.
%  - Sudoku: Lista con los 81 elementos (9x9) del tablero.
%  - Index: Posición (0 a 80) de la casilla.
%  - Num: Número a verificar.
% -------------------------------------------------------------------------------
miembro_fila(Sudoku, Index, Num) :-
    Fila is Index // 9,         % Calcula la fila a la que pertenece la casilla
    Inicio is Fila * 9,         % Índice inicial de la fila
    Fin is Inicio + 8,          % Índice final de la fila
    between(Inicio, Fin, I),    % Itera sobre los índices de la fila
    nth0(I, Sudoku, Num).       % Verifica si Num está presente en dicha posición

% -------------------------------------------------------------------------------
% miembro_columna/3: Comprueba si un número (Num) ya se encuentra en la misma
% columna de la casilla indicada por Index dentro del Sudoku.
%  - Sudoku: Lista con los 81 elementos del tablero.
%  - Index: Posición (0 a 80) de la casilla.
%  - Num: Número a verificar.
% -------------------------------------------------------------------------------
miembro_columna(Sudoku, Index, Num) :-
    Columna is Index mod 9,     % Calcula la columna a la que pertenece la casilla
    between(0, 8, Fila),       % Itera sobre las filas (0 a 8)
    I is Fila * 9 + Columna,    % Cálculo del índice en la lista para esa columna
    nth0(I, Sudoku, Num).       % Verifica si Num está presente en dicha posición

% -------------------------------------------------------------------------------
% miembro_cuadro/3: Comprueba si un número (Num) ya se encuentra en el mismo
% cuadro 3x3 de la casilla indicada por Index dentro del Sudoku.
%  - Sudoku: Lista con los 81 elementos del tablero.
%  - Index: Posición (0 a 80) de la casilla.
%  - Num: Número a verificar.
% -------------------------------------------------------------------------------
miembro_cuadro(Sudoku, Index, Num) :-
    Fila is Index // 9,                 % Calcula la fila
    Columna is Index mod 9,             % Calcula la columna
    CuadroFila is (Fila // 3) * 3,      % Fila inicial del bloque 3x3
    CuadroColumna is (Columna // 3) * 3,% Columna inicial del bloque 3x3
    between(0, 2, DF),                  % Desplazamiento de fila dentro del bloque
    between(0, 2, DC),                  % Desplazamiento de columna dentro del bloque
    I is (CuadroFila + DF) * 9 + (CuadroColumna + DC),
    nth0(I, Sudoku, Num).               % Verifica si Num está en el bloque 3x3

% -------------------------------------------------------------------------------
% casilla_vacia/1: Verifica si una casilla (Casilla) está vacía,
% es decir, si no contiene un número (no es un integer).
% -------------------------------------------------------------------------------
casilla_vacia(Casilla) :-
    \+ number(Casilla).

% -------------------------------------------------------------------------------
% posibilidades_casilla/3: Obtiene la lista de posibilidades para una casilla
%  - Sudoku: Lista con los 81 elementos del tablero.
%  - Index: Posición (0 a 80) de la casilla.
%  - Posibilidades: Lista de números posibles para esa casilla, considerando
%    que no estén en su fila, columna o bloque 3x3.
% -------------------------------------------------------------------------------
posibilidades_casilla(Sudoku, Index, Posibilidades) :-
    nth0(Index, Sudoku, Casilla),                     % Obtiene el contenido de la casilla
    (   casilla_vacia(Casilla) ->                     % Si la casilla está vacía
        findall(Num, (numero(Num),                    % Buscamos todos los números válidos
                      \+ miembro_fila(Sudoku, Index, Num),
                      \+ miembro_columna(Sudoku, Index, Num),
                      \+ miembro_cuadro(Sudoku, Index, Num)
                     ), Posibilidades)
    ;   Posibilidades = []                             % Si no está vacía, no hay posibilidades
    ).

% -------------------------------------------------------------------------------
% generar_posibilidades/2: Genera la lista de posibilidades para todas las
% casillas del Sudoku (las vacías tendrán una lista de números, las llenas, lista vacía).
% -------------------------------------------------------------------------------
generar_posibilidades(Sudoku, Posibilidades) :-
    generar_posibilidades(Sudoku, 0, Posibilidades).

% Caso base: cuando Index = 81, hemos verificado las 81 posiciones (9x9).
generar_posibilidades(_, 81, []).

% Caso recursivo: calcula las posibilidades para la casilla en 'Index'
% y luego avanza a la siguiente casilla.
generar_posibilidades(Sudoku, Index, [P|Resto]) :-
    Index < 81,
    posibilidades_casilla(Sudoku, Index, P),
    NextIndex is Index + 1,
    generar_posibilidades(Sudoku, NextIndex, Resto).

% -------------------------------------------------------------------------------
% generar_lista_posibilidades/0: Genera e imprime (por consola) la lista de
% posibilidades para cada casilla del Sudoku definido por el predicado sudoku/1.
% -------------------------------------------------------------------------------
generar_lista_posibilidades :-
    sudoku(Tablero),                     % Obtenemos el tablero (definido en sudoku/1)
    generar_posibilidades(Tablero, Posibilidades),
    writeln(Posibilidades).             % Imprime la lista de posibilidades

% -------------------------------------------------------------------------------
% Verificación Extendida Considerando Posibilidades
% -------------------------------------------------------------------------------

% -------------------------------------------------------------------------------
% region_valida/2: Verifica que una región (fila, columna o cuadro 3x3) sea válida
% según las reglas:
%  1. No hay números repetidos en las casillas fijas de la región.
%  2. Cada casilla vacía en la región debe tener al menos una posibilidad.
%  3. Cada número que falta en la región debe poder colocarse en al menos una
%     de las casillas vacías (es decir, aparecer en su lista de posibilidades).
%  - Indices: Lista de posiciones (0 a 80) que forman la región (fila/columna/bloque).
%  - Sudoku: Lista con los 81 elementos del tablero.
% -------------------------------------------------------------------------------
region_valida(Indices, Sudoku) :-
    % Extrae los números fijos de la región y verifica que no se repitan
    findall(E, (member(I, Indices), nth0(I, Sudoku, E), number(E)), Fijos),
    sort(Fijos, FijosSorted),
    length(Fijos, LFix),
    length(FijosSorted, LFixSorted),
    LFix =:= LFixSorted,  % Si hay duplicados, las longitudes no coinciden

    % Cada casilla vacía debe tener alguna posibilidad
    forall((member(I, Indices), nth0(I, Sudoku, E), \+ number(E)),
           (posibilidades_casilla(Sudoku, I, Poss), Poss \= [])),

    % Cada número que falta debe poder ponerse en al menos una de las casillas vacías
    forall((between(1, 9, Num), \+ member(Num, FijosSorted)),
           (member(I, Indices),
            nth0(I, Sudoku, E), \+ number(E),
            posibilidades_casilla(Sudoku, I, Poss),
            member(Num, Poss)
           )
    ).

% -------------------------------------------------------------------------------
% verificar_filas_poss/1: Verifica que todas las filas del Sudoku sean válidas
% con respecto a las posibilidades de cada casilla.
%  - Sudoku: Lista con los 81 elementos del tablero.
% -------------------------------------------------------------------------------
verificar_filas_poss(Sudoku) :-
    forall(between(0, 8, Row),                  % Recorremos las filas (0 a 8)
           (
             Inicio is Row * 9,
             Fin is Inicio + 8,
             findall(I, between(Inicio, Fin, I), Indices),  % Índices de la fila actual
             ( region_valida(Indices, Sudoku) ->
                 true
             ;   format('Error en la fila ~w: índices ~w~n', [Row, Indices])
             )
           )
    ).

% -------------------------------------------------------------------------------
% verificar_columnas_poss/1: Verifica que todas las columnas del Sudoku
% sean válidas según las posibilidades de cada casilla.
%  - Sudoku: Lista con los 81 elementos del tablero.
% -------------------------------------------------------------------------------
verificar_columnas_poss(Sudoku) :-
    forall(between(0, 8, Col),                 % Recorremos las columnas (0 a 8)
           (
             findall(I, (between(0,8,Row), I is Row * 9 + Col), Indices),
             ( region_valida(Indices, Sudoku) ->
                 true
             ;   format('Error en la columna ~w: índices ~w~n', [Col, Indices])
             )
           )
    ).

% -------------------------------------------------------------------------------
% verificar_cuadrantes_poss/1: Verifica que todos los cuadros 3x3 del Sudoku
% sean válidos según las posibilidades de cada casilla.
%  - Sudoku: Lista con los 81 elementos del tablero.
% -------------------------------------------------------------------------------
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

% -------------------------------------------------------------------------------
% verificar_sudoku_poss/0: Predicado principal para verificar el estado actual
% del Sudoku considerando las posibilidades de cada casilla.
% Realiza las verificaciones de filas, columnas y bloques. Si todo es válido,
% muestra un mensaje de confirmación.
% -------------------------------------------------------------------------------
verificar_sudoku_poss:-
    sudoku(Sudoku),
    verificar_filas_poss(Sudoku),
    verificar_columnas_poss(Sudoku),
    verificar_cuadrantes_poss(Sudoku).
