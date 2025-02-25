% Representación del Sudoku como una lista de 81 elementos
sudoku1([., ., 6, ., ., 2, 3, ., 4,
        9, ., 4, ., 7, 5, ., 8, 2,
        ., ., ., 8, ., ., ., 6, 5,
        ., 3, ., ., ., ., ., ., 4,
        2, ., 4, ., ., ., 8, ., 3,
        ., 4, ., 7, ., 5, ., ., .,
        ., ., ., ., 6, ., ., ., 8,
        7, ., ., 2, ., 4, 5, ., 3,
        ., 3, 7, ., ., ., 6, ., 9]).


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
    sudoku1(Tablero),
    resolver_regla_0(Tablero, NuevoTablero),
    imprimir_sudoku(NuevoTablero) .


%----------------------REGLA 1------------------------------

%Funciones para calcular los indices de una misma fila, columna o cuadrado:

% Predicado para obtener los índices de la fila en la que está una casilla
%Dado un índice de casilla (Index), obtiene los índices de todas las casillas de la misma fila en el Sudoku.
indices_fila(Index, Indices) :-
    Fila is Index // 9,       % Obtener número de fila (división entera)
    Inicio is Fila * 9,       % Índice de inicio de la fila
    Fin is Inicio + 8,
    findall(I, between(Inicio, Fin, I), Indices). % Generar los índices de toda la fila

% Predicado para obtener los índices de la columna en la que está una casilla
indices_columna(Index, Indices) :-
    Columna is Index mod 9, % Obtener número de columna
    findall(I, (between(0, 8, Fila), I is Fila * 9 + Columna), Indices). % Generar los índices de la columna

% Predicado para obtener los índices del cuadro en el que está una casilla
indices_cuadro(Index, Indices) :-
    Fila is Index // 9,
    Columna is Index mod 9,
    CuadroFila is (Fila // 3) * 3,
    CuadroColumna is (Columna // 3) * 3,
    findall(I, 
        (between(0, 2, DF), between(0, 2, DC), 
         I is (CuadroFila + DF) * 9 + (CuadroColumna + DC)),
        Indices).



% Predicado unificado para obtener los números únicos en una fila, columna o cuadro
numeros_unicos(Tipo, Posibilidades, Index, UnicosPrevios, UnicosFinales) :-
    (   Tipo = fila -> indices_fila(Index, Indices)
    ;   Tipo = columna -> indices_columna(Index, Indices)
    ;   Tipo = cuadro -> indices_cuadro(Index, Indices)
    ),

    % Obtener las posibilidades de todas las casillas en la región (excepto la casilla actual)
    findall(P, (member(I, Indices), I \= Index, nth0(I, Posibilidades, P), P \= []), PosibilidadesRegion),

    % Aplanar la lista de listas en una sola
    flatten(PosibilidadesRegion, TodosNumerosRegion),

    % Obtener las posibilidades de la casilla en Index
    nth0(Index, Posibilidades, PosibilidadesCasilla),
    PosibilidadesCasilla \= [],  % Asegurar que no es una casilla ya resuelta

    % Filtrar los números que NO se repiten en la región
    findall(Num, 
        (member(Num, PosibilidadesCasilla), \+ memberchk(Num, TodosNumerosRegion)), 
        UnicosNuevos
    ),

    % Unir los nuevos únicos con los previos
    append(UnicosPrevios, UnicosNuevos, UnicosFinales).


%Función creada para comprobar que funciona individualmente para cada casilla
%la detección de posibilidades no repetidas en una misma fila, columna o cuadro
probar_numeros_unicos :-
    sudoku(Tablero),
    imprimir_sudoku(Tablero),
    generar_posibilidades(Tablero, PosibilidadesActualizadas),

    Index = 13, 

    numeros_unicos(fila, PosibilidadesActualizadas, Index, [], UnicosFila),
    writeln('Prueba FILA: Numeros unicos en la casilla seleccionada:'),
    writeln(UnicosFila),

    numeros_unicos(columna, PosibilidadesActualizadas, Index, UnicosFila, UnicosColumna),
    writeln('Prueba COLUMNA: Numeros unicos en la casilla seleccionada:'),
    writeln(UnicosColumna),

    numeros_unicos(cuadro, PosibilidadesActualizadas, Index, UnicosColumna, UnicosCuadro),
    writeln('Prueba CUADRO: Numeros unicos en la casilla seleccionada:'),
    writeln(UnicosCuadro).



% Predicado para aplicar la Regla 1 a todo el Sudoku
aplicar_regla_1(Sudoku, Posibilidades, NuevoPosibilidades) :-
    aplicar_regla_1_aux(Sudoku, Posibilidades, 0, NuevoPosibilidades).

% Caso base: cuando hemos revisado todas las casillas (81 en total)
aplicar_regla_1_aux(_, [], 81, []).

% Caso recursivo: revisar cada casilla y aplicar la Regla 1 si se cumple la condición
aplicar_regla_1_aux(Sudoku, [P|Ps], Index, [NuevoP|NuevoPs]) :-
    (   P \= [],  % Si la casilla tiene posibilidades (no está ya resuelta)
        numeros_unicos(fila, Posibilidades, Index, [], UnicosFila),
        numeros_unicos(columna, Posibilidades, Index, UnicosFila, UnicosColumna),
        numeros_unicos(cuadro, Posibilidades, Index, UnicosColumna, UnicosFinales),
        UnicosFinales = [Num|_]  % Si al menos un número es único en fila, columna o cuadro
    ->  NuevoP = [Num]  % Se sustituye por ese número
    ;   NuevoP = P  % Si no cumple la condición, se mantiene la lista de posibilidades
    ),
    NextIndex is Index + 1,
    aplicar_regla_1_aux(Sudoku, Ps, NextIndex, NuevoPs).


% Predicado para aplicar las reglas 0 y 1 iterativamente hasta que el Sudoku ya no cambie
resolver_sudoku(Sudoku, SudokuFinal) :-
    resolver_regla_0(Sudoku, Sudoku0),
    generar_posibilidades(Sudoku0, Posibilidades),
    iterar_reglas(Sudoku0, Posibilidades, SudokuFinal).

iterar_reglas(Sudoku, Posibilidades, SudokuFinal) :-
    aplicar_regla_1(Sudoku, Posibilidades, Posibilidades1),
    actualizar_sudoku(Sudoku, Posibilidades1, Sudoku1),
    resolver_regla_0(Sudoku1, Sudoku2),
    (   Sudoku \= Sudoku2, % Si hubo cambios, seguimos iterando
        length(Sudoku, L1), length(Sudoku2, L2), L1 =\= L2
    ->  generar_posibilidades(Sudoku2, NuevasPosibilidades),
        iterar_reglas(Sudoku2, NuevasPosibilidades, SudokuFinal)
    ;   SudokuFinal = Sudoku2  % Si no hubo cambios, terminamos
    ).


% Predicado para probar la solución completa
probar_resolucion :-
    sudoku(Tablero),
    resolver_sudoku(Tablero, Resultado),
    imprimir_sudoku(Resultado).
