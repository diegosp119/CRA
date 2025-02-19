% Representación del Sudoku como una lista de 81 elementos
sudoku_regla_0([
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

sudoku_regla_0_y_1([
     5, ., ., ., ., ., ., ., 9,  
    ., ., ., ., 7, ., ., ., .,  
    ., ., ., ., ., 3, ., ., .,  
    ., 1, ., ., ., ., ., 6, .,  
    ., ., ., ., ., ., ., ., .,  
    ., 8, ., ., ., ., ., 7, .,  
    ., ., ., 4, ., ., ., ., .,  
    ., ., ., ., 9, ., ., ., .,  
    2, ., ., ., ., ., ., ., 8]).


% Predicado para imprimir el tablero de Sudoku
imprimir_sudoku([]).
imprimir_sudoku(Tablero) :-
    imprimir_fila(Tablero, Resto),
    imprimir_sudoku(Resto).

% Predicado para imprimir una fila del tablero
imprimir_fila(Tablero, Resto) :-
    tomar(9, Tablero, Fila, Resto), %Fila almacena los números de la lista y %Resto lo que queda de sudoku
    writeln(Fila).

% Predicado para tomar los primeros N elementos de una lista
tomar(0, L, [], L).
tomar(N, [H|T], [H|R], Resto) :- % [H|T] es la lista original, [H|R] los N elementos que se quieren tomar, Resto lo que sobra
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
    between(Inicio, Fin, I), %Genera valores para I, comenzando en Inicio y terminando en Fin
    nth0(I, Sudoku, Num). %Busca el elemento en la posición I dentro de la lista Sudoku y lo unifica con Num

% Predicado para verificar si un número está en la misma columna
miembro_columna(Sudoku, Index, Num) :-
    Columna is Index mod 9,
    between(0, 8, Fila), %Genera valores para Fila, comenzando en 0 y terminando en 8
    I is Fila * 9 + Columna, %Cada elemento de la columna es la fila*9 + la posición de la columna
    nth0(I, Sudoku, Num). %Busca el elemento en la posición I dentro de la lista Sudoku y lo unifica con Num

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

%Predicado para probar la Regla 0
probar_regla_0 :-
    sudoku(Tablero),
    resolver_regla_0(Tablero, NuevoTablero),
    imprimir_sudoku(NuevoTablero),
    generar_posibilidades(NuevoTablero, PosibilidadesActualizadas),
    imprimir_posibilidades(PosibilidadesActualizadas).

% Predicado para aplicar la Regla 1: Identificar valores únicos en filas, columnas o cuadros
resolver_regla_1(Sudoku, NuevoSudoku) :-
    generar_posibilidades(Sudoku, Posibilidades),
    aplicar_regla_1(Sudoku, Posibilidades, NuevoSudoku).

% Predicado para aplicar la Regla 1 una vez
aplicar_regla_1(Sudoku, Posibilidades, NuevoSudoku) :-
    actualizar_por_unicidad(Sudoku, Posibilidades, SudokuActualizado),
    (   Sudoku \= SudokuActualizado -> 
        resolver_regla_1(SudokuActualizado, NuevoSudoku)  % Seguir aplicando si hay cambios
    ;   NuevoSudoku = Sudoku  % Si no hubo cambios, devolver el Sudoku final
    ).

% Buscar números únicos en una fila, columna o cuadrante y actualizar la casilla
actualizar_por_unicidad(Sudoku, Posibilidades, NuevoSudoku) :-
    encontrar_unicos(Posibilidades, Unicos),
    reemplazar_unicos(Sudoku, Posibilidades, Unicos, NuevoSudoku).

% Extraer los valores únicos de cada fila, columna y cuadro
encontrar_unicos(Posibilidades, Unicos) :-
    encontrar_unicos_fila(Posibilidades, UF),
    encontrar_unicos_columna(Posibilidades, UC),
    encontrar_unicos_cuadro(Posibilidades, UQ),
    append([UF, UC, UQ], Unicos).

% Extraer valores únicos en cada fila
encontrar_unicos_fila(Posibilidades, Unicos) :-
    findall(Num, (
        between(0, 8, Fila),
        obtener_indices_fila(Fila, Indices),
        extraer_numeros_unicos(Indices, Posibilidades, Num)
    ), Unicos).

% Extraer valores únicos en cada columna
encontrar_unicos_columna(Posibilidades, Unicos) :-
    findall(Num, (
        between(0, 8, Col),
        obtener_indices_columna(Col, Indices),
        extraer_numeros_unicos(Indices, Posibilidades, Num)
    ), Unicos).

% Extraer valores únicos en cada cuadrante
encontrar_unicos_cuadro(Posibilidades, Unicos) :-
    findall(Num, (
        between(0, 8, Cuadro),
        obtener_indices_cuadro(Cuadro, Indices),
        extraer_numeros_unicos(Indices, Posibilidades, Num)
    ), Unicos).

% Obtener índices de una fila
obtener_indices_fila(Fila, Indices) :-
    Inicio is Fila * 9,
    Fin is Inicio + 8,
    findall(I, between(Inicio, Fin, I), Indices).

% Obtener índices de una columna
obtener_indices_columna(Col, Indices) :-
    findall(I, (between(0, 8, Fila), I is Fila * 9 + Col), Indices).

% Obtener índices de un cuadrante
obtener_indices_cuadro(Cuadro, Indices) :-
    FBase is (Cuadro // 3) * 3,
    CBase is (Cuadro mod 3) * 3,
    findall(I, (
        between(0, 2, F),
        between(0, 2, C),
        I is (FBase + F) * 9 + (CBase + C)
    ), Indices).

% Extraer números únicos dentro de una lista de índices de la cuadrícula
extraer_numeros_unicos(Indices, Posibilidades, Num) :-
    findall(N, (
        member(I, Indices),
        nth0(I, Posibilidades, Lista),
        member(N, Lista)
    ), Todos),
    sort(Todos, Unicos),
    findall(U, (member(U, Unicos), count_occurrences(U, Todos, 1)), Num).

% Contar ocurrencias de un número en la lista
count_occurrences(X, List, Count) :-
    include(=(X), List, Matches),
    length(Matches, Count).

% Reemplazar casillas vacías con valores únicos
reemplazar_unicos(Sudoku, Posibilidades, Unicos, NuevoSudoku) :-
    maplist(reemplazar_unico(Posibilidades, Unicos), Sudoku, Posibilidades, NuevoSudoku).

% Si una casilla tiene solo un valor posible y es único en su fila, columna o cuadro, lo reemplazamos
reemplazar_unico(Posibilidades, Unicos, Casilla, PosibilidadesCasilla, NuevoValor) :-
    (Casilla = '.' -> 
        (intersection(PosibilidadesCasilla, Unicos, [Valor]) -> NuevoValor = Valor ; NuevoValor = Casilla)
    ; NuevoValor = Casilla).

probar_regla_0_y_1 :-
    sudoku_regla_0_y_1(Tablero),
    writeln('\n Sudoku a resolver:'),
    imprimir_sudoku(Tablero),

    resolver_regla_0(Tablero, TableroConRegla0),
    writeln('\n Regla 0:'),
    imprimir_sudoku(TableroConRegla0),

    resolver_regla_1(TableroConRegla0, TableroFinal),
    writeln('\n Regla 1:'),
    imprimir_sudoku(TableroFinal).
