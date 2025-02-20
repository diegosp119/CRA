<<<<<<< Updated upstream
% Representación del Sudoku como una lista de 81 elementos
sudoku([
    ., ., 9, 6, ., ., ., 1, .,
    8, ., ., ., ., 1, ., 9, .,
    7, ., ., ., ., ., ., ., 8,
    ., 3, ., ., 6, ., ., ., .,
    ., 4, ., 1, ., 9, ., ., 5,
    9, ., ., ., ., ., ., ., .,
    ., 8, ., 9, ., ., 5, 4, .,
    6, ., ., 7, 1, ., ., ., 3,
    ., ., 5, ., 8, 4, ., ., 9]).

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

% Predicado para verificar si una casilla está vacía
casilla_vacia(Casilla) :-
    \+ number(Casilla).

% Predicado para obtener las posibilidades para cada casilla vacía
posibilidades_casilla(Sudoku, Index, Posibilidades) :-
    nth0(Index, Sudoku, Casilla),
    casilla_vacia(Casilla),
    findall(Num, (numero(Num), \+ miembro_fila(Sudoku, Index, Num), \+ miembro_columna(Sudoku, Index, Num), \+ miembro_cuadro(Sudoku, Index, Num)), Posibilidades).

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

% Predicado para generar las listas de posibilidades para todas las casillas vacías
generar_posibilidades(Sudoku, Posibilidades) :-
    findall(P, (nth0(Index, Sudoku, Casilla), casilla_vacia(Casilla), posibilidades_casilla(Sudoku, Index, P)), Posibilidades).

% Predicado para imprimir las posibilidades
imprimir_posibilidades([]).
imprimir_posibilidades([P|Ps]) :-
    writeln(P),
    imprimir_posibilidades(Ps).
=======
%HECHOS

posiciones([1, 2, 3, 4, 5, 6, 7, 8, 9,
            10, 11, 12, 13, 14, 15, 16, 17, 18, 
            19, 20, 21, 22, 23, 24, 25, 26, 27, 
            28, 29, 30, 31, 32, 33, 34, 35, 36, 
            37, 38, 39, 40, 41, 42, 43, 44, 45, 
            46, 47, 48, 49, 50, 51, 52, 53, 54, 
            55, 56, 57, 58, 59, 60, 61, 62, 63, 
            64, 65, 66, 67, 68, 69, 70, 71, 72, 
            73, 74, 75, 76, 77, 78, 79, 80, 81]).

sudoku([.,.,3,5,8,.,6,2,.,
        .,6,.,1,7,.,.,8,.,
        4,.,8,.,6,.,.,.,9,
        .,.,9,.,4,.,.,.,2,
        1,4,6,.,.,.,7,3,.,
        8,3,.,6,.,.,9,4,5,
        .,2,.,8,5,.,4,.,7,
        9,5,4,7,3,2,8,.,6,
        .,8,7,4,.,1,.,.,3]).

%REGLAS

%Imprimir listas

imprimir_listas :-
    posiciones(Posiciones),
    sudoku(Sudoku),
    writeln(Posiciones),
    writeln(Sudoku).

%Recorrido de listas

%IDEA: Crear una lista con posiciones y casilla intercaladas, despues utilizar variable anónima en caso de ser necesaria.

%Generacion de lista

generar_lista_posibilidades(Posiciones,Sudoku):-
>>>>>>> Stashed changes
