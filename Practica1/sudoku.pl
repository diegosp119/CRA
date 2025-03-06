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


sudoku2([
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

sudoku([5, 3, ., ., 7, ., ., ., .,
        6, ., ., 1, 9, 5, ., ., .,
        1, 9, 8, ., ., ., ., 6, .,
        8, ., ., ., 6, ., ., ., 3,
        4, ., ., 8, ., 3, ., ., 1,
        7, ., ., ., 2, ., ., ., 6,
        ., 6, ., ., ., ., 2, 8, .,
        ., ., ., 4, 1, 9, ., ., 5,
        ., ., ., ., 8,.,.,7,9]).

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
    sudoku(Antiguo_Tablero),
    resolver_regla_0(Antiguo_Tablero, Tablero),
    imprimir_sudoku(Tablero),
    generar_posibilidades(Tablero, PosibilidadesActualizadas),
    imprimir_sudoku(PosibilidadesActualizadas),

    Index = 10, 

    numeros_unicos(fila, PosibilidadesActualizadas, Index, [], UnicosFila),
    writeln('Prueba FILA: Numeros unicos en la casilla seleccionada:'),
    writeln(UnicosFila),

    numeros_unicos(columna, PosibilidadesActualizadas, Index, UnicosFila, UnicosColumna),
    writeln('Prueba COLUMNA: Numeros unicos en la casilla seleccionada:'),
    writeln(UnicosColumna),

    numeros_unicos(cuadro, PosibilidadesActualizadas, Index, UnicosColumna, UnicosCuadro),
    writeln('Prueba CUADRO: Numeros unicos en la casilla seleccionada:'),
    writeln(UnicosCuadro).


%--------------------------------------------------------------------------------------------

% Predicado principal para aplicar la Regla 1 en todas las casillas del Sudoku una vez
aplicar_regla_1(Sudoku, NuevaPosibilidades) :-
    generar_posibilidades(Sudoku, Posibilidades), % Paso 1: Generar la lista de posibilidades inicial
    aplicar_regla_1_casilla(Sudoku, Posibilidades, 0, NuevaPosibilidades).

% Recorrer todas las casillas aplicando la Regla 1
aplicar_regla_1_casilla(_, Posibilidades, 81, Posibilidades). % Caso base: cuando llegamos al final

aplicar_regla_1_casilla(Sudoku, Posibilidades, Index, NuevaPosibilidadesFinal) :-
    nth0(Index, Posibilidades, P),  % Obtener la lista de posibilidades de la casilla actual
    (   P \= [] -> 
        % Reiniciar la lista de únicos en cada iteración
        numeros_unicos(fila, Posibilidades, Index, [], UnicosFila),
        numeros_unicos(columna, Posibilidades, Index, UnicosFila, UnicosColumna),
        numeros_unicos(cuadro, Posibilidades, Index, UnicosColumna, UnicosFinales),
        UnicosFinales \= [] -> % Si hay al menos un número único
        [PrimerUnico | _] = UnicosFinales, % Tomar el primer número único
        reemplazar_elemento(Index, [PrimerUnico], Posibilidades, PosibilidadesActualizadas)
    ;   PosibilidadesActualizadas = Posibilidades  % Si la casilla ya estaba resuelta o no hay números únicos, mantener igual
    ),
    NextIndex is Index + 1,
    aplicar_regla_1_casilla(Sudoku, PosibilidadesActualizadas, NextIndex, NuevaPosibilidadesFinal).

% Predicado para reemplazar un elemento en una lista
reemplazar_elemento(0, X, [_|T], [X|T]).
reemplazar_elemento(N, X, [H|T], [H|R]) :-
    N > 0, N1 is N - 1, 
    reemplazar_elemento(N1, X, T, R).

% Predicado para aplicar la Regla 0 y Regla 1 en bucle hasta que el Sudoku no cambie
resolver_sudoku(Sudoku, SudokuFinal) :-
    resolver_sudoku_iterativo(Sudoku, SudokuFinal).

resolver_sudoku_iterativo(Sudoku, SudokuFinal) :-
    resolver_regla_0(Sudoku, SudokuRegla0),  % Aplicar Regla 0
    generar_posibilidades(SudokuRegla0,Paco)
    aplicar_regla_1(SudokuRegla0, Paco),  % Aplicar Regla 1
    (   SudokuNuevo \= Sudoku ->  % Si el Sudoku cambió, repetir la iteración
        resolver_sudoku_iterativo(SudokuNuevo, SudokuFinal)
    ;   SudokuFinal = SudokuNuevo  % Si no cambió, devolver el Sudoku actual
    ).

% Predicado para probar el nuevo comportamiento del resolver
probar_resolver_sudoku :-
    sudoku(Tablero),
    writeln('Sudoku inicial:'),
    imprimir_sudoku(Tablero),
    (   resolver_sudoku(Tablero, SudokuFinal) ->
        writeln('Sudoku después del intento de resolución:'),
        imprimir_sudoku(SudokuFinal),
        (   member(., SudokuFinal) ->  % Si todavía hay casillas vacías, el Sudoku no se resolvió completamente
            writeln('El Sudoku quedó a medio resolver. No se pudo completar con las reglas implementadas.')
        ;   writeln('¡Sudoku resuelto completamente!')
        )
    ;   writeln('No se pudo hacer ningún progreso en el Sudoku.')
    ).
