% Representación del Sudoku como una lista de 81 elementos
sudoku1([
    ., ., 9, 6, ., ., ., 1, .,
    8, ., ., ., ., 1, ., 9, .,
    7, ., ., ., ., ., ., ., 8,
    ., 3, ., ., 6, ., ., ., .,
    ., 4, ., 1, ., 9, ., ., 5,
    9, ., ., ., ., ., ., ., .,
    ., 8, ., 9, ., ., 5, 4, .,
    6, ., ., 7, 1, ., ., ., 3,
    ., ., 5, ., 8, 4, ., ., 9]).

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
        findall(Num, (numero(Num), \+ miembro_fila(Sudoku, Index, Num), \+ miembro_columna(Sudoku, Index, Num), \+ miembro_cuadro(Sudoku, Index, Num)), Posibilidades)
    ;   Posibilidades = []
    ).




%Cosas de la regla 1

%imprimir repetidos, unicamente lo tengo para ver si funciona bien antes de seguir
imprimir_repetidos([]).
imprimir_repetidos([P|Ps]) :-
    writeln(P),
    imprimir_repetidos(Ps).


% Predicado para inicializar las listas de posibilidades y repetidos para fila, columna y Cuadrado
inicializar_lista_de_listas(N, ListaDeListas) :-
    writeln(N),
    length(ListaDeListas, N),       % Crea una lista con N elementos (variables no instanciadas)
    maplist(=([]), ListaDeListas).  % Asigna a cada elemento una lista vacía



% Predicado para agregar un número a la lista de repetidos de la fila, columna o Cuadrado
agregar_repetido(id, lista, Numero) :-
    % Aquí se debe agregar el número a la lista de repetidos de la fila, columna o Cuadrado
    append (lista[id], [Numero], nuevalista[id]).


% Predicado para agregar un número a la lista de no repetidos de la fila, columna o Cuadrado
% El objetivo de esta lista no es mas que para usarla como base para preguntar por repetidos, pero ahora que lo veo creo que le falta algo para hacer eso
agregar_no_repetido(id, lista, Numero) :-
    % Aquí se debe agregar el número a la lista de no repetidos de la fila, columna o Cuadrado
    append (lista[id], [Numero], nuevalista[id]).


%Predicado para ver si está repetido en la lista fila, columna o cuadrado
repetido_fila(FilaPosibilidades, Fila, Num) :-
    nth0(Fila, FilaPosibilidades, ListaFila), % Obtiene la lista de la fila Fila
    member(Num, ListaFila). % Verifica si Num está en esa lista

repetido_columna(ColumnaPosibilidades, Columna, Num) :-
    nth0(Columna, ColumnaPosibilidades, ListaCol), % Obtiene la lista de la fila Fila
    member(Num, ListaCol). % Verifica si Num está en esa lista

repetido_cuadrado(CuadradoPosibilidades, Cuadrado, Num) :-
    nth0(Cuadrado, CuadradoPosibilidades, ListaCuad), % Obtiene la lista de la fila Fila
    member(Num, ListaCuad). % Verifica si Num está en esa lista



% Predicado recursivo para procesar las posibilidades y agregarlas a las listas correspondientes
procesar_posibilidades_recursivo([], _, _, _, _, _, _, _, _, _, _).  % Caso base: No hay mas posibilidades
procesar_posibilidades_recursivo([H|T], Posibilidades, Fila, Columna, Cuadrado, FilaPosibilidades, ColumnaPosibilidades, CuadradoPosibilidades, FilaRepetidos, ColumnaRepetidos, CuadradoRepetidos) :-
    % Comprobar si el número ya está presente en la lista de repetidos para la fila, columna o Cuadrado
    (   repetido_fila(FilaPosibilidades, Fila, H)  ->
        % Si está agregarlo a los repetidos
        agregar_repetido(Fila, FilaRepetidos, H)
    ;   % Si no está agregarlo a las no repetidas
        agregar_no_repetido(Fila, FilaPosibilidades, H)
    ),
    (
        repetido_columna(ColumnaPosibilidades,Columna, H) ->
        agregar_repetido(Columna, ColumnaRepetidos, H)
    ;   agregar_no_repetido(Columna, ColumnaPosibilidades, H)
    ),
    (
        repetido_cuadro(CuadradoPosibilidades,Cuadrado, H) ->
        agregar_repetido(Cuadrado, CuadradoRepetidos, H)
    ;   agregar_no_repetido(Cuadrado, CuadradoPosibilidades, H)
    ),

    % Llamada recursiva para procesar el resto de las posibilidades
    % Ese posibilidades solo, está para seguir teniendo Posibilidades que no se si con la recursividad se borra el que paso como primer argumento
    procesar_posibilidades_recursivo(T, Posibilidades, Fila, Columna, Cuadrado, FilaPosibilidades, ColumnaPosibilidades, CuadradoPosibilidades, FilaRepetidos, ColumnaRepetidos, CuadradoRepetidos).



% Predicado para procesar las posibilidades
procesar_posibilidades(Index, Posibilidades) :-
    % Calcular fila, columna y Cuadrado
    Columna is  (((Index//9) mod 3) * 3 )+ (Index mod 3),
    Fila is ((Index//27) * 3 )+ ((Index mod 9)//3),
    Cuadricula is Index//9,
    

    % Inicializar listas para las posibilidades y repetidos
    inicializar_listas(9, FilaPosibilidades),
    writeln(Fila).
    %inicializar_listas(9, ColumnaPosibilidades),
    %inicializar_listas(9, CuadradoPosibilidades),
    %inicializar_listas(9, FilaRepetidos),
    %inicializar_listas(9, ColumnaRepetidos),
    %inicializar_listas(9, CuadradoRepetidos),
    

    % Procesar las posibilidades, agregar a las listas correspondientes
    %procesar_posibilidades_recursivo(Posibilidades, Fila, Columna, Cuadrado, FilaPosibilidades, ColumnaPosibilidades, CuadradoPosibilidades, FilaRepetidos, ColumnaRepetidos, CuadradoRepetidos),
    %imprimir_repetidos(FilaRepetidos[Fila]),
    %imprimir_repetidos(ColumnaRepetidos[Columna]),
    %imprimir_repetidos(CuadradoRepetidos[Cuadrado]).


hola(Index, Posibilidades) :-
    writeln(Index).



% Predicado para obtener las posibilidades para cada casilla vacía o generar una lista vacía si la casilla está ocupada
%Esto ignorar
%posibilidades_casilla_regla1(Sudoku, Index, Posibilidades) :-
%    nth0(Index, Sudoku, Casilla),
%    (   casilla_vacia(Casilla) -> 
%        findall(Num, (numero(Num), \+ miembro_fila(Sudoku, Index, Num), \+ miembro_columna(Sudoku, Index, Num), \+ miembro_cuadro(Sudoku, Index, Num)), Posibilidades)
%    ;   Posibilidades = []
%    ),
%    %hola(Index, Posibilidades).
%    procesar_posibilidades(Index, Posibilidades).
    








generar_posibilidades(_, 81, []).
generar_posibilidades(Sudoku, Index, [Posibilidades|Resto]) :-
    Index < 81,
    posibilidades_casilla_regla1(Sudoku, Index, Posibilidades),
    NextIndex is Index + 1,
    generar_posibilidades(Sudoku, NextIndex, Resto),
    


% Predicado para generar las listas de posibilidades para todas las casillas
generar_posibilidades(Sudoku, Posibilidades) :-
    generar_posibilidades(Sudoku, 0, Posibilidades),

    procesar_posibilidades(Sudoku, 0, Posibilidades).



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




















