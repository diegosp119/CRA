% Representación del Sudoku como una lista de 81 elementos

%Sudoku 1 muy facil
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

%Sudoku 25 muy facil
sudoku1([., ., 7, ., ., 8, ., ., .,
         ., 4, 5, 7, 6, ., ., ., 2,
         6, ., ., ., 4, ., 3, ., 5,
         8, 6, ., 5, ., ., 4, ., .,
         ., ., 3, 8, ., 4, ., 6, .,
         ., 7, 2, 6, 9, ., ., 8, 3,
         ., 5, ., ., ., ., ., 4, 7,
         7, ., ., 4, ., ., ., ., 6,
         ., 3, 4, ., ., 6, 2, ., .]).

%Sudoku 985 muy facil
sudoku2([
    ., 7, 2, 4, ., ., ., ., .,
    ., ., 8, ., ., ., ., ., 4,
    ., ., ., 2, ., ., ., ., 9,
    2, ., 9, 7, ., ., ., ., .,
    ., 3, ., 8, 4, ., 9, 6, 2,
    ., 6, 4, 5, ., 2, 3, 8, .,
    5, ., 6, 3, ., 8, ., 4, .,
    3, 4, 7, ., 2, 6, 5, ., 8,
    ., 2, ., ., ., 4, ., ., .]).

%Sudoku 988 muy facil
sudoku3([
    ., 7, ., ., 5, ., 3, ., .,
    ., ., 6, 3, 2, 7, ., 1, .,
    5, ., ., ., 8, 9, 4, 6, .,
    3, ., 8, ., 6, 2, 9, 7, .,
    7, 6, ., 9, 3, ., 8, 4, 5,
    ., ., 5, 7, 4, 8, 2, ., 6,
    ., ., 4, 8, ., ., ., ., 3,
    ., 8, ., 2, 9, ., ., 5, .,
    6, ., ., 4, 7, 5, ., 8, 2]).

%Sudoku 991 muy facil
sudoku4([
    ., 4, ., 6, ., 2, 8, ., 3,
    3, 8, 2, 9, 7, ., ., 4, 5,
    ., ., 6, 8, ., 4, 2, 9, 7,
    4, 2, 5, 3, 6, ., ., ., .,
    ., ., 3, ., ., 7, ., 6, .,
    ., ., ., ., ., ., ., 3, 8,
    5, 7, ., 2, ., ., 3, 8, .,
    6, 3, ., 4, 9, 5, 7, 2, .,
    ., ., 4, 7, 8, ., ., 5, 6 ]).

%Sudoku 1007 facil
sudoku5([
    ., ., ., ., 9, 7, 4, 6, .,
    ., 4, ., 1, ., ., ., 9, .,
    ., ., ., ., ., 6, 1, 5, 8,
    ., 6, 4, 2, ., ., 9, 3, .,
    ., ., ., ., 4, ., ., ., .,
    ., 7, ., 6, ., 5, ., 8, .,
    4, 9, ., 3, 8, 1, ., ., 5,
    ., ., 6, ., ., ., ., 1, 9,
    ., ., 2, 5, ., ., 7, ., .]).

%Sudoku 1001 facil
sudoku6([ ., ., 6, ., ., 2, 3, ., 4,
         9, ., 4, 7, 5, ., ., 8, 2,
         ., ., 8, ., ., 6, ., ., 5,
         ., ., 3, ., ., ., ., 4, .,
         2, ., ., 4, ., ., 8, 3, .,
         4, ., 7, 5, ., ., ., ., .,
         ., ., ., 6, ., ., ., ., 8,
         7, ., ., ., 2, ., 4, 5, 3,
         ., ., ., 3, 7, ., ., 6, 9]).


%Sudoku 1010 facil
sudoku7([
    ., 3, ., ., ., ., 4, 7, .,
    ., ., 4, ., 7, ., ., ., 8,
    8, 7, ., 6, ., 4, ., ., 2,
    ., ., 5, ., ., 8, 2, ., 6,
    3, ., 2, 4, ., ., 7, ., 5,
    ., ., ., ., 2, 7, ., ., 3,
    5, 4, ., 9, 6, ., 3, 8, .,
    ., ., ., ., 4, 3, ., 6, .,
    ., ., ., ., ., ., ., 2, .]).

%Sudoku 1217 facil
sudoku8([
    ., ., ., ., 3, ., ., ., 4,
    ., 9, ., 4, ., 6, ., 7, .,
    ., 5, ., ., ., ., 3, 8, .,
    ., ., ., ., 7, 8, ., ., 3,
    3, ., ., ., ., ., 6, 9, .,
    5, 4, ., 6, ., ., ., 2, .,
    7, ., 5, ., 2, 4, ., ., .,
    9, 8, 4, ., 6, 5, 2, ., .,
    ., 2, 6, ., 8, ., ., ., 9]).

%Sudoku 1220 facil
sudoku9([
    ., 8, ., ., ., ., 6, 9, 3,
    2, 6, 9, ., ., ., ., ., 5,
    ., 7, ., 9, 6, ., 2, ., 8,
    ., ., ., 4, ., ., 8, ., 9,
    ., ., 8, 6, ., ., 4, 5, .,
    ., 3, 1, ., ., ., ., ., .,
    3, ., 2, 5, 8, ., ., ., 7,
    ., ., ., ., ., 6, ., 1, .,
    7, 4, 6, ., 3, ., ., ., .]).

%Sudoku 1 medio Repositorio Github
sudoku10([
    ., 2, ., 9, ., ., ., ., .,
    ., 4, 8, ., ., ., ., 3, 1,
    ., ., ., ., 6, 3, ., 2, .,
    ., ., 9, 4, ., 7, ., ., 3,
    ., ., 3, ., 8, ., 2, ., .,
    4, ., ., 1, ., 5, 6, ., .,
    ., 3, ., 5, 7, ., ., ., .,
    2, 5, ., ., ., ., 1, 8, .,
    ., ., ., ., ., 6, ., 5, .]).

%Sudoku 2 medio Repositorio Github
sudoku11([
    1, ., ., 8, ., ., 5, 7, .,
    ., ., ., ., ., 9, 2, 1, .,
    ., 9, ., ., 4, ., ., ., .,
    3, ., ., 9, ., ., ., 5, .,
    ., ., 7, ., ., ., 3, ., .,
    ., 2, ., ., ., 6, ., ., 8,
    ., ., ., ., 2, ., ., 4, .,
    ., 7, 1, 4, ., ., ., ., .,
    ., 6, 4, ., ., 7, ., ., 3]).

%Sudoku 3 medio Repositorio Github
sudoku12([
    ., ., 2, ., ., ., 8, ., .,
    ., ., 5, ., 2, ., 1, ., .,
    4, 6, ., ., ., ., ., 2, 9,
    1, 3, ., ., 6, ., ., 5, 2,
    ., ., 9, ., 8, ., 4, ., .,
    ., ., ., 3, ., 2, ., ., .,
    ., ., 6, ., 7, ., 2, ., .,
    7, ., ., ., ., ., ., ., 8,
    ., 2, ., 5, 1, 9, ., 7, .]).

%Sudoku 4 medio Repositorio Github
sudoku13([
    8, ., 2, 6, ., ., ., ., 9,
    ., ., ., ., 5, 8, ., ., .,
    ., ., 6, ., ., ., 4, ., 1,
    ., 9, ., 4, ., 6, ., ., 5,
    ., 2, ., ., ., ., ., 4, .,
    6, ., ., 2, ., 3, ., 9, .,
    2, ., 5, ., ., ., 9, ., .,
    ., ., ., 9, 7, ., ., ., .,
    1, ., ., ., ., 2, 8, ., 4]).

%Sudoku 5 medio Repositorio Github
sudoku14([
    ., 7, ., ., ., ., 1, 2, .,
    1, ., ., ., ., ., ., 6, 7,
    ., ., ., 2, ., ., ., ., 4,
    2, ., ., ., 4, ., ., 7, .,
    7, 1, ., ., 3, ., ., 4, 9,
    ., 9, ., ., 7, ., ., ., 1,
    3, ., ., ., ., 9, ., ., .,
    9, 5, ., ., ., ., ., ., 6,
    ., 6, 7, ., ., ., ., 8, .]).

%Sudoku 1 dificil Repositorio Github
sudoku15([
    ., 8, ., 2, ., ., 4, ., .,
    5, 7, ., ., ., ., 1, ., .,
    ., ., 2, 3, ., ., ., ., .,
    8, 2, ., ., 9, ., ., ., 5,
    ., ., ., 7, 1, 5, ., ., .,
    7, ., ., ., 2, ., ., 4, 1,
    ., ., ., ., ., 6, 7, ., .,
    ., ., 3, ., ., ., ., 1, 8,
    ., ., 7, ., ., 9, ., 5, .]).

%Sudoku 2 dificil Repositorio Github
sudoku16([
    6, ., ., ., 5, ., ., ., 7,
    ., 3, ., ., ., ., ., ., .,
    ., 8, ., 4, ., 9, 2, ., .,
    ., 1, 5, 3, ., ., ., ., .,
    ., ., 8, ., ., ., 3, ., .,
    ., ., ., ., ., 7, 5, 9, .,
    ., ., 9, 5, ., 1, ., 3, .,
    ., ., ., ., ., ., ., 8, .,
    2, ., ., ., 7, ., ., ., 4]).

%Sudoku 3 dificil Repositorio Github
sudoku17([
    2, 1, ., 9, 5, ., ., ., 4,
    ., 9, ., ., 6, ., ., 3, 7,
    ., ., ., 7, ., ., ., ., .,
    ., ., ., ., ., ., 3, ., 8,
    9, 2, ., ., ., ., ., 1, 5,
    8, ., 5, ., ., ., ., ., .,
    ., ., ., ., ., 2, ., ., .,
    6, 8, ., ., 1, ., ., 4, .,
    1, ., ., ., 4, 7, ., 9, 6]).

%Sudoku 4 dificil Repositorio Github
sudoku18([
    ., 2, 4, ., ., ., 6, 5, .,
    1, ., ., ., ., ., ., ., 7,
    ., ., 8, ., 1, ., 9, ., .,
    ., ., ., ., ., ., ., ., .,
    2, 6, ., ., 9, ., ., 8, 3,
    ., 8, ., 5, ., 1, ., 7, .,
    6, ., ., 9, ., 3, ., ., 8,
    ., ., 2, 8, 5, 4, 7, ., .,
    ., ., ., ., 7, ., ., ., .]).

%Sudoku 5 dificil Repositorio Github
sudoku19([
    ., ., ., ., 5, ., ., ., .,
    ., ., ., 2, ., 6, ., ., .,
    ., 6, 4, ., ., ., 3, 9, .,
    ., 4, 5, ., ., ., 8, 1, .,
    ., ., ., ., 2, ., ., ., .,
    ., ., ., 1, ., 7, ., ., .,
    ., 5, 3, ., ., ., 9, 8, .,
    ., 9, ., 8, ., 4, ., 6, .,
    1, ., ., ., 3, ., ., ., 4]).

%Sudoku 1 diabolico Github
sudoku20([
    ., 8, 3, ., 2, ., ., 9, .,
    ., ., ., 8, ., ., 1, ., .,
    ., 2, 9, 3, ., ., ., ., 8,
    ., ., ., ., 9, 8, 7, ., .,
    ., 7, ., ., ., ., ., 6, .,
    ., ., 6, 7, 4, ., ., ., .,
    3, ., ., ., ., 6, 9, 8, .,
    ., ., 2, ., ., 5, ., ., .,
    ., 1, ., ., 3, ., 5, 4, .]).

%Sudoku 2 diabolico Github
sudoku21([
    2, ., ., ., 5, ., ., ., 6,
    ., 1, ., ., ., ., ., 9, .,
    6, ., ., 8, ., 1, ., ., 3,
    ., ., 7, ., 9, ., 6, ., .,
    ., ., ., 7, ., 3, ., ., .,
    9, ., ., ., 8, ., ., ., 2,
    1, ., ., ., ., ., ., ., 5,
    ., 6, ., 9, ., 2, ., 1, .,
    ., ., 3, ., 6, ., 2, ., .]).

%Sudoku 3 diabolico Github
sudoku22([
    5, 9, ., ., ., ., ., ., 7,
    ., 4, ., ., 1, ., ., 8, 3,
    ., ., 8, ., 3, 4, 9, ., .,
    ., ., 1, 4, ., 2, ., ., .,
    ., 6, 9, ., ., ., 8, 2, .,
    ., ., ., 1, ., 9, 3, ., .,
    ., ., 4, 6, 7, ., 2, ., .,
    9, 8, ., ., 4, ., ., 3, .,
    7, ., ., ., ., ., ., 1, 6
]).

% Sudoku 4 diabolico Github
sudoku23([
    ., ., 6, ., ., ., 2, ., .,
    9, ., ., ., ., ., ., ., 4,
    2, 4, 3, ., ., ., 8, 9, 6,
    ., ., ., 5, 9, 1, ., ., .,
    ., ., 2, ., 8, ., 3, ., .,
    4, ., ., 2, ., 3, ., ., 1,
    3, ., ., ., ., ., ., ., 7,
    ., ., ., 9, ., 7, ., ., .,
    ., 1, ., 4, ., 8, ., 2, .]).

% Sudoku 5 diabolico Github
sudoku24([
    ., ., ., ., ., ., ., ., .,
    5, 6, ., ., ., ., ., 3, 2,
    2, 3, ., ., 4, ., ., 7, 9,
    ., ., ., ., 6, ., ., ., .,
    ., 7, ., 5, ., 1, ., 9, .,
    ., ., ., 7, ., 8, ., ., .,
    ., 5, 3, ., ., ., 9, 2, .,
    ., ., 9, 8, ., 6, 5, ., .,
    7, ., ., ., ., ., ., ., 4 ]).

%Sudoku prueba regla 3
sudoku25([
    ., 9, ., ., 2, 4, ., 7, .,
    ., ., ., ., ., ., ., ., .,
    6, 4, ., 3, ., 7, ., ., .,
    ., 7, ., ., 4, 5, 6, ., 3,
    ., 5, ., ., ., ., ., 2, 7,
    2, ., 6, ., ., ., ., ., .,
    3, ., ., ., 8, ., ., 4, .,
    ., 7, ., 4, 3, ., ., 5, 6,
    4, 2, 5, ., ., ., 8, 3, 9
]).







indice_filas([1,2,3,10,11,12,19,20,21,4,5,6,13,14,15,22,23,24,7,8,9,16,17,18,25,26,27,28,29,30,37,38,39,46,47,48,31,32,33,40,41,42,49,50,51,34,35,36,43,44,45,52,53,54,55,56,57,64,65,66,73,74,75,58,59,60,67,68,69,76,77,78,61,62,63,70,71,72,79,80,81]).



% Predicado para imprimir el tablero de Sudoku con formato elegante
imprimir_sudoku(Tablero) :-
    writeln('----------------------'),
    imprimir_filas(Tablero, 1),
    writeln('----------------------').

% Predicado para imprimir filas con separaciones
imprimir_filas([], _).
imprimir_filas(Tablero, Fila) :-
    tomar(9, Tablero, FilaActual, Resto),
    imprimir_fila(FilaActual, 1),
    ( Fila mod 3 =:= 0, Resto \= [] -> writeln('----------------------') ; true ),
    NuevaFila is Fila + 1,
    imprimir_filas(Resto, NuevaFila).

% Predicado para imprimir una fila con separaciones entre bloques 3x3
imprimir_fila([], _) :- write('|'), nl.
imprimir_fila([X|Xs], Columna) :-
    ( Columna =:= 1 -> write('| ') ; true ),  
    ( var(X) -> write('. ') ; format('~w ', [X]) ),
    ( Columna mod 3 =:= 0, Xs \= [] -> write('| ') ; true ),
    NuevaColumna is Columna + 1,
    imprimir_fila(Xs, NuevaColumna).

% Predicado principal para imprimir el Sudoku
imprimir :-
    sudoku25(Tablero),
    imprimir_sudoku(Tablero).

% Predicado para tomar los primeros N elementos de una lista
tomar(0, L, [], L).
tomar(N, [H|T], [H|R], Resto) :-
    N > 0,
    N1 is N - 1,
    tomar(N1, T, R, Resto).

% -------------------------------
% Predicados Auxiliares
% -------------------------------

% Dividir el tablero en filas (9 elementos cada una).
filas_sudoku([], []).
filas_sudoku(Sudoku, [Fila|Filas]) :-
    tomar(9, Sudoku, Fila, Resto),
    filas_sudoku(Resto, Filas).

% Predicado para saltar N elementos en una lista.
skip(0, L, L).
skip(N, [_|T], Rest) :-
    N > 0,
    N1 is N - 1,
    skip(N1, T, Rest).

% Predicado para extraer una sublista (slicing): desde la posición Start, Count elementos.
slice(List, Start, Count, Slice) :-
    skip(Start, List, Rest),
    tomar(Count, Rest, Slice, _).

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

% -------------------------------
% Predicado para Verificar el Cuadrante (3x3) por Slicing
% -------------------------------

miembro_cuadro(Sudoku, Index, Num) :-
    % Cálculo de fila y columna del elemento (0-based)
    Row is Index // 9,
    Col is Index mod 9,
    % Determinar el inicio (cabezera) del bloque 3×3
    RowStart is (Row // 3) * 3,
    ColStart is (Col // 3) * 3,
    % Convertir el tablero en una lista de filas
    filas_sudoku(Sudoku, Filas),
    % Extraer las 3 filas que componen el bloque:
    nth0(RowStart, Filas, Fila1),
    Row2 is RowStart + 1,
    nth0(Row2, Filas, Fila2),
    Row3 is RowStart + 2,
    nth0(Row3, Filas, Fila3),
    % Extraer el segmento de cada fila correspondiente al bloque (3 celdas)
    slice(Fila1, ColStart, 3, Seccion1),
    slice(Fila2, ColStart, 3, Seccion2),
    slice(Fila3, ColStart, 3, Seccion3),
    % Unir las secciones en una lista con todas las celdas del cuadrante
    append([Seccion1, Seccion2, Seccion3], Cuadro),
    % Verificar si el número ya se encuentra en ese bloque
    member(Num, Cuadro).

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


numero_casillas_vacias(_, 81, Contador, Valor_Final):-
    Valor_Final is Contador.

numero_casillas_vacias([Casilla|Resto], Index, Contador, Valor_Final):-
    (casilla_vacia(Casilla)-> 
    Contador_Nuevo is Contador + 1
    ;
    Contador_Nuevo is Contador
    ),
    SigIndex is Index+1,
    numero_casillas_vacias(Resto, SigIndex, Contador_Nuevo, Valor_Final).


resolver_regla_0(Sudoku, NuevoSudoku, CountIn, CountOut) :-
    generar_posibilidades(Sudoku, Posibilidades),
    aplicar_regla_0(Sudoku, Posibilidades, NuevoSudoku, CountIn, CountOut).

% Wrapper sin contador explícito para resolver_regla_0
resolver_regla_0(Sudoku, NuevoSudoku) :-
    resolver_regla_0(Sudoku, NuevoSudoku, 0, _).

aplicar_regla_0(Sudoku, Posibilidades, NuevoSudoku, CountIn, CountOut) :-
    actualizar_sudoku(Sudoku, Posibilidades, SudokuActualizado),
    numero_casillas_vacias(Sudoku, 0, 0, Numero_vacios),
    numero_casillas_vacias(SudokuActualizado, 0, 0, Numero_vacios_act),
    Numero_cambios is Numero_vacios-Numero_vacios_act,
    format('Se realizaron ~w cambios en la iteracion ~w de la Regla 0 ~n', [Numero_cambios, CountIn]),
    NewCount is CountIn + 1,
    (   Sudoku \= SudokuActualizado ->
        resolver_regla_0(SudokuActualizado, NuevoSudoku, NewCount, CountOut)
    ;   NuevoSudoku = Sudoku,
        CountOut = NewCount
    ).

% Wrapper sin contador explícito para aplicar_regla_0
aplicar_regla_0(Sudoku, Posibilidades, NuevoSudoku) :-
    aplicar_regla_0(Sudoku, Posibilidades, NuevoSudoku, 0, _).



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
    sudoku3(Tablero),
    generar_posibilidades(Tablero,Posibilidades),
    %writeln(Posibilidades),
    imprimir_posibilidades(Posibilidades).


probar_regla_0 :-
    sudoku(Tablero),
    resolver_regla_0(Tablero, NuevoTablero, 0, Count),
    writeln("Sudoku después de aplicar la Regla 0:"),
    imprimir_sudoku(NuevoTablero),
    generar_posibilidades(NuevoTablero, PosibilidadesActualizadas),
    imprimir_sudoku(PosibilidadesActualizadas),
    format("Número de iteraciones (resolver_regla_0): ~w~n", [Count]).
    

%----------------------REGLA 1------------------------------




%Cosas de la Regla 1


% Reemplazar un elemento en una lista en una posición dada
replace([_|T], 1, X, [X|T]).
replace([H|T], N, X, [H|R]) :- N > 1, N1 is N - 1, replace(T, N1, X, R).

%Desde Aqui es lo que queda hacer
% Obtener los valores correspondientes a los índices dados en la lista de valores
obtener_valores_por_indices([], _, []).

obtener_valores_por_indices([I|RestIndices], Lista_Original, [Valor|ValoresRestantes]) :-
    nth1(I, Lista_Original, Valor), % Extrae el valor en la posición I de ListaValores
    obtener_valores_por_indices(RestIndices, Lista_Original, ValoresRestantes). % Continúa con el resto de índices
    

% Regla para incrementar el conteo de un número si ya está en la lista
incrementar_conteo(H, [H-N|Resto], [H-N1|Resto]) :-
    N1 is N + 1.

% Si el número no es el actual, seguimos buscando en la lista
incrementar_conteo(H, [X|Resto], [X|NuevoResto]) :-
    incrementar_conteo(H, Resto, NuevoResto).


% Si el número no está en la lista, lo agregamos con un conteo de 1
incrementar_conteo(H, [], [H-1]).

agregar_conteo([], Conteo, Conteo).

agregar_conteo([H|T], ConteoPrevio, ConteoFinal) :-
    incrementar_conteo(H, ConteoPrevio, ConteoActualizado),
    agregar_conteo(T, ConteoActualizado, ConteoFinal).




% Contar la cantidad de veces que aparece cada número en todas las listas
contar_ocurrencias([], []).
contar_ocurrencias([Lista|Resto], ConteoTotal) :-
    contar_ocurrencias(Resto, ConteoParcial),
    agregar_conteo(Lista, ConteoParcial, ConteoTotal).



% Filtrar una lista dejando solo los elementos que aparecen una sola vez en todas las listas
filtrar_unicos([], _, []).
filtrar_unicos([H|T], Conteo, [H|Rest]) :-
    member(H-1, Conteo), % Solo dejamos elementos que aparecen exactamente una vez en la lista completa
    filtrar_unicos(T, Conteo, Rest).
filtrar_unicos([_|T], Conteo, Rest) :-
    filtrar_unicos(T, Conteo, Rest).

% Aplicar el filtrado a todas las sublistas
eliminar_repetidos_global([], _, []).
eliminar_repetidos_global([Lista|Resto], Conteo, [ListaFiltrada|RestoFiltrado]) :-
    filtrar_unicos(Lista, Conteo, ListaFiltrada),
    eliminar_repetidos_global(Resto, Conteo, RestoFiltrado).

% Predicado principal para limpiar la lista de repetidos globales
eliminar_repetidos(Listas, Resultado) :-
    contar_ocurrencias(Listas, Conteo),
    eliminar_repetidos_global(Listas, Conteo, Resultado).


% Eliminar repetidos dentro de las listas de filas, columnas y cuadrados
eliminar_repetidos_de_las_listas(_,10, _,Def, Lista_Definitiva):-
    Lista_Definitiva = Def.
    
eliminar_repetidos_de_las_listas([Lista|Rest], Indice, Valores, Def, Lista_Definitiva) :-
    % Acceder a los valores de la lista a través de los índices
   
    obtener_valores_por_indices(Lista, Valores, Valores_Extraidos),
   
    eliminar_repetidos(Valores_Extraidos, ListaSinRepetidos),

    % Agregar los valores de ListaSinRepetidos a Def
    append(Def, ListaSinRepetidos, DefActualizado),

    Nuevo_Indice is Indice + 1,
    % Continuar con las siguientes listas
    eliminar_repetidos_de_las_listas(Rest, Nuevo_Indice, Valores, DefActualizado, Lista_Definitiva).







% Función que actualiza las posibilidades de acuerdo a los valores únicos
actualizar_posibilidades_con_unicos(Posibilidades, 82,_,_,Posibilidades_Fin) :-
    % Recorrer todas las casillas del Sudoku (de 1 a 81)
    Posibilidades_Fin=Posibilidades.

% Caso recursivo: procesar una casilla y actualizarla si corresponde
actualizar_posibilidades_con_unicos(Posibilidades, Indice, Indice_Guia, Unicos, Posibilidades_Fin) :-
    % Comprobar si hay un valor único en FilaUnicos, ColumnaUnicos o CuadradoUnicos
    (   nth1(Indice, Unicos, ValorUnicos),
        ValorUnicos \= [] ->( % Si la lista no está vacía
            length(ValorUnicos, 1) ->(
                ValorUnicos = [X]  % Solo si tiene exactamente un elemento y no está vacía
                ->  Reemplazo = [X],
                nth1(Indice,Indice_Guia,Valor_indice_en_posibilidades),
                replace(Posibilidades, Valor_indice_en_posibilidades, Reemplazo, NuevaPosibilidadesParcial),
                IndiceSiguiente is Indice + 1,
                actualizar_posibilidades_con_unicos(NuevaPosibilidadesParcial,IndiceSiguiente,Indice_Guia  ,Unicos,Posibilidades_Fin)
            )      
            ;
            IndiceSiguiente is Indice + 1,
            actualizar_posibilidades_con_unicos(Posibilidades,IndiceSiguiente, Indice_Guia ,Unicos,Posibilidades_Fin)

        )
    ;
        % Llamada recursiva para procesar el siguiente índice
        IndiceSiguiente is Indice + 1,
        actualizar_posibilidades_con_unicos(Posibilidades,IndiceSiguiente, Indice_Guia ,Unicos,Posibilidades_Fin)
    ).




% Función principal para iniciar la comparación
verificar_repetidos(Filas, Columnas, Cuadrados, Valores, FilaDef, ColumnaDef, Cuadrados_Def) :-
    % Definir los índices
    Indice_Cuadrado=[[1,2,3,10,11,12,19,20,21],[4,5,6,13,14,15,22,23,24],[7,8,9,16,17,18,25,26,27],[28,29,30,37,38,39,46,47,48],[31,32,33,40,41,42,49,50,51],[34,35,36,43,44,45,52,53,54],[55,56,57,64,65,66,73,74,75],[58,59,60,67,68,69,76,77,78],[61,62,63,70,71,72,79,80,81]],
    Indice_Columnas=[[1,10,19,28,37,46,55,64,73],[2,11,20,29,38,47,56,65,74],[3,12,21,30,39,48,57,66,75],[4,13,22,31,40,49,58,67,76],[5,14,23,32,41,50,59,68,77],[6,15,24,33,42,51,60,69,78],[7,16,25,34,43,52,61,70,79],[8,17,26,35,44,53,62,71,80],[9,18,27,36,45,54,63,72,81]],
    Indice_Filas=[[1,2,3,4,5,6,7,8,9],[10,11,12,13,14,15,16,17,18],[19,20,21,22,23,24,25,26,27],[28,29,30,31,32,33,34,35,36],[37,38,39,40,41,42,43,44,45],[46,47,48,49,50,51,52,53,54],[55,56,57,58,59,60,61,62,63],[64,65,66,67,68,69,70,71,72],[73,74,75,76,77,78,79,80,81]],

    % Eliminar repetidos dentro de cada lista de filas, columnas y cuadrados

    eliminar_repetidos_de_las_listas(Indice_Filas,1, Valores, Filas, FilaDef),

    eliminar_repetidos_de_las_listas(Indice_Columnas, 1,Valores, Columnas, ColumnaDef),

    eliminar_repetidos_de_las_listas(Indice_Cuadrado, 1, Valores, Cuadrados, Cuadrados_Def).
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Aplicar la Regla 1 con contador
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Versión con contador de aplicar_regla_1.
% Se incrementa el contador (CountIn → NewCount) y se propaga (CountOut).
aplicar_regla_1(Posibilidades, FilaUnicos, ColumnaUnicos, CuadradoUnicos,
                FilaDef, ColumnaDef, Cuadrados_Def, NuevaPosibilidades, CountIn, CountOut) :-
    NewCount is CountIn + 1,
    verificar_repetidos(FilaUnicos, ColumnaUnicos, CuadradoUnicos, Posibilidades,
                        FilaDef, ColumnaDef, Cuadrados_Def),
    Posibilidades_Originales = Posibilidades,
    Indice_Cuadrado = [1,2,3,10,11,12,19,20,21,4,5,6,13,14,15,22,23,24,7,8,9,16,17,18,25,26,27,28,29,30,37,38,39,46,47,48,31,32,33,40,41,42,49,50,51,34,35,36,43,44,45,52,53,54,55,56,57,64,65,66,73,74,75,58,59,60,67,68,69,76,77,78,61,62,63,70,71,72,79,80,81],
    Indice_Columnas = [1,10,19,28,37,46,55,64,73,2,11,20,29,38,47,56,65,74,3,12,21,30,39,48,57,66,75,4,13,22,31,40,49,58,67,76,5,14,23,32,41,50,59,68,77,6,15,24,33,42,51,60,69,78,7,16,25,34,43,52,61,70,79,8,17,26,35,44,53,62,71,80,9,18,27,36,45,54,63,72,81],
    Indice_Filas = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81],
    actualizar_posibilidades_con_unicos(Posibilidades, 1, Indice_Filas, FilaDef, Temp1),
    (   Posibilidades_Originales \== Temp1 ->
            NuevaPosibilidades = Temp1,
            CountOut = NewCount
    ;   actualizar_posibilidades_con_unicos(Temp1, 1, Indice_Columnas, ColumnaDef, Temp2),
        (   Temp1 \== Temp2 ->
                NuevaPosibilidades = Temp2,
                CountOut = NewCount
        ;   actualizar_posibilidades_con_unicos(Temp2, 1, Indice_Cuadrado, Cuadrados_Def, NuevaPosibilidades),
            CountOut = NewCount
        )
    ).

% Wrapper sin contador explícito para aplicar_regla_1.
aplicar_regla_1(Posibilidades, FilaUnicos, ColumnaUnicos, CuadradoUnicos,
                FilaDef, ColumnaDef, Cuadrados_Def, NuevaPosibilidades) :-
    aplicar_regla_1(Posibilidades, FilaUnicos, ColumnaUnicos, CuadradoUnicos,
                    FilaDef, ColumnaDef, Cuadrados_Def, NuevaPosibilidades, 0, _).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Resolver la Regla 1 con contador
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
resolver_regla_1(Posibilidades, Fin, CountIn, CountOut) :-
    aplicar_regla_1(Posibilidades, _FilaUnicos, _ColumnaUnicos, _CuadradoUnicos,
                    _FilaDef, _ColumnaDef, _Cuadrados_Def, NuevaPosibilidades, CountIn, CountOut),
    Fin = NuevaPosibilidades.

resolver_regla_1(Posibilidades, Fin) :-
    resolver_regla_1(Posibilidades, Fin, 0, _).

probar_regla_1 :-
    % Obtiene el tablero de Sudoku (asegúrate de que sudoku4/1 esté definido y retorne un tablero válido)
    sudoku4(Tablero),
    % Genera las posibilidades para el tablero
    generar_posibilidades(Tablero, Posibilidades),
    % Aplica la Regla 1 con contador
    resolver_regla_1(Posibilidades, NuevaPosibilidades, 0, Count),
    writeln("Posibilidades después de aplicar la Regla 1:"),
    imprimir_sudoku(NuevaPosibilidades),
    format("Número de llamadas a resolver_regla_1: ~w~n", [Count]).


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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Predicado de prueba para las Reglas 0 y 1 con contador
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

probar_reglas_0_y_1 :-
    sudoku4(Tablero),
    imprimir_sudoku(Tablero),
    iterar_reglas_0_y_1(Tablero, NuevoTablero, IterCount),
    writeln("Sudoku después de aplicar las Reglas 0 y 1:"),
    imprimir_sudoku(NuevoTablero),
    generar_posibilidades(NuevoTablero, Posibilidades),
    imprimir_sudoku(Posibilidades),
    format("Número de iteraciones (resolver_reglas_0_y_1): ~w~n", [IterCount]).


%----------------------REGLA 2------------------------------

% Predicado para obtener las posibilidades de una fila
obtener_posibilidades_fila(Posibilidades, Fila, PosibilidadesFila) :-
    % Calcula el rango de índices correspondientes a la fila
    Inicio is (Fila - 1) * 9 + 1,
    Fin is Fila * 9,
    % Utiliza findall para recopilar las posibilidades de cada celda en la fila
    findall(Posibilidad, (between(Inicio, Fin, Index), nth1(Index, Posibilidades, Posibilidad)), PosibilidadesFila).

% Predicado para obtener las posibilidades de una columna
obtener_posibilidades_columna(Posibilidades, Columna, PosibilidadesColumna) :-
    % Utiliza findall para recopilar las posibilidades de cada celda en la columna
    findall(Posibilidad, (between(0, 8, I), Index is Columna + I * 9, nth1(Index, Posibilidades, Posibilidad)), PosibilidadesColumna).

% Predicado para obtener las posibilidades de un cuadro
obtener_posibilidades_cuadro(Posibilidades, Cuadro, PosibilidadesCuadro) :-
    % Calcula los índices de las celdas en el cuadro
    CuadroFila is (Cuadro - 1) // 3 * 3,
    CuadroColumna is (Cuadro - 1) mod 3 * 3,
    % Utiliza findall para recopilar las posibilidades de cada celda en el cuadro
    findall(Posibilidad, (
        between(0, 2, I), between(0, 2, J),
        Index is (CuadroFila + I) * 9 + CuadroColumna + J + 1,
        nth1(Index, Posibilidades, Posibilidad)
    ), PosibilidadesCuadro).


% Predicado para aplicar la Regla 2
% Versión con contador: cada vez que se llama a aplicar_regla_2 se incrementa el contador
aplicar_regla_2(Posibilidades, NuevaPosibilidades, CountIn, CountOut) :-
    NewCount is CountIn + 1,  % Se incrementa el contador
    % Aplicar la Regla 2 a filas
    aplicar_regla_2_a_todas_filas(Posibilidades, Temp1),
    % Aplicar la Regla 2 a columnas
    aplicar_regla_2_a_todas_columnas(Temp1, Temp2),
    % Aplicar la Regla 2 a cuadros
    aplicar_regla_2_a_todos_cuadros(Temp2, NuevaPosibilidades),
    CountOut = NewCount.

% Versión "wrapper" sin contador explícito: se inicia en 0 y se ignora el valor final
aplicar_regla_2(Posibilidades, NuevaPosibilidades) :-
    aplicar_regla_2(Posibilidades, NuevaPosibilidades, 0, _).

% Predicado para aplicar la Regla 2 a todas las filas
aplicar_regla_2_a_todas_filas(Posibilidades, NuevaPosibilidades) :-
    aplicar_regla_2_a_todas_filas(Posibilidades, 1, NuevaPosibilidades).

% Caso base: cuando se han procesado todas las filas
aplicar_regla_2_a_todas_filas(Posibilidades, 9, Posibilidades).
% Caso recursivo: procesar una fila y continuar con la siguiente
aplicar_regla_2_a_todas_filas(Posibilidades, Fila, NuevaPosibilidades) :-
    % Obtener las posibilidades de la fila actual
    obtener_posibilidades_fila(Posibilidades, Fila, PosibilidadesFila),
    % Encontrar pares únicos en la fila
    encontrar_pares_unicos(PosibilidadesFila, ParesUnicos),
    % Eliminar los pares únicos de la fila
    eliminar_pares_de_fila(Posibilidades, Fila, ParesUnicos, TempPosibilidades),
    % Continuar con la siguiente fila
    FilaSiguiente is Fila + 1,
    aplicar_regla_2_a_todas_filas(TempPosibilidades, FilaSiguiente, NuevaPosibilidades).

% Predicado para aplicar la Regla 2 a todas las columnas
aplicar_regla_2_a_todas_columnas(Posibilidades, NuevaPosibilidades) :-
    aplicar_regla_2_a_todas_columnas(Posibilidades, 1, NuevaPosibilidades).

% Caso base: cuando se han procesado todas las columnas
aplicar_regla_2_a_todas_columnas(Posibilidades, 9, Posibilidades).
% Caso recursivo: procesar una columna y continuar con la siguiente
aplicar_regla_2_a_todas_columnas(Posibilidades, Columna, NuevaPosibilidades) :-
    % Obtener las posibilidades de la columna actual
    obtener_posibilidades_columna(Posibilidades, Columna, PosibilidadesColumna),
    % Encontrar pares únicos en la columna
    encontrar_pares_unicos(PosibilidadesColumna, ParesUnicos),
    % Eliminar los pares únicos de la columna
    eliminar_pares_de_columna(Posibilidades, Columna, ParesUnicos, TempPosibilidades),
    % Continuar con la siguiente columna
    ColumnaSiguiente is Columna + 1,
    aplicar_regla_2_a_todas_columnas(TempPosibilidades, ColumnaSiguiente, NuevaPosibilidades).

% Predicado para aplicar la Regla 2 a todos los cuadros
aplicar_regla_2_a_todos_cuadros(Posibilidades, NuevaPosibilidades) :-
    aplicar_regla_2_a_todos_cuadros(Posibilidades, 1, NuevaPosibilidades).

% Caso base: cuando se han procesado todos los cuadros
aplicar_regla_2_a_todos_cuadros(Posibilidades, 9, Posibilidades).
% Caso recursivo: procesar un cuadro y continuar con el siguiente
aplicar_regla_2_a_todos_cuadros(Posibilidades, Cuadro, NuevaPosibilidades) :-
    % Obtener las posibilidades del cuadro actual
    obtener_posibilidades_cuadro(Posibilidades, Cuadro, PosibilidadesCuadro),
    % Encontrar pares únicos en el cuadro
    encontrar_pares_unicos(PosibilidadesCuadro, ParesUnicos),
    % Eliminar los pares únicos del cuadro
    eliminar_pares_de_cuadro(Posibilidades, Cuadro, ParesUnicos, TempPosibilidades),
    % Continuar con el siguiente cuadro
    CuadroSiguiente is Cuadro + 1,
    aplicar_regla_2_a_todos_cuadros(TempPosibilidades, CuadroSiguiente, NuevaPosibilidades).

% Predicado para encontrar pares de números que aparecen solo en dos casillas
encontrar_pares_unicos(PosibilidadesGrupo, ParesUnicos) :-
    % Utiliza findall para encontrar pares únicos en el grupo
    findall([Num1, Num2], (
        member(Posibilidad, PosibilidadesGrupo),
        length(Posibilidad, 2),
        [Num1, Num2] = Posibilidad,
        contar_ocurrencias(PosibilidadesGrupo, [Num1, Num2], 2)
    ), ParesUnicos).

% Predicado para contar ocurrencias de un par en una lista
contar_ocurrencias([], _, 0).
contar_ocurrencias([H|T], H, N) :-
    contar_ocurrencias(T, H, N1),
    N is N1 + 1.
contar_ocurrencias([_|T], H, N) :-
    contar_ocurrencias(T, H, N).

% Predicado para eliminar pares de una fila
eliminar_pares_de_fila(Posibilidades, Fila, ParesUnicos, NuevaPosibilidades) :-
    % Calcula el rango de índices correspondientes a la fila
    Inicio is (Fila - 1) * 9 + 1,
    Fin is Fila * 9,
    % Utiliza findall para recopilar los índices en el rango
    findall(Index, between(Inicio, Fin, Index), Indices),
    % Llama a eliminar_pares_de_indices para eliminar los pares en esos índices
    eliminar_pares_de_indices(Posibilidades, Indices, ParesUnicos, NuevaPosibilidades).

% Predicado para eliminar pares de una columna
eliminar_pares_de_columna(Posibilidades, Columna, ParesUnicos, NuevaPosibilidades) :-
    % Utiliza findall para recopilar los índices de la columna
    findall(Index, (between(0, 8, I), Index is Columna + I * 9), Indices),
    % Llama a eliminar_pares_de_indices/4 para eliminar los pares en esos índices
    eliminar_pares_de_indices(Posibilidades, Indices, ParesUnicos, NuevaPosibilidades).

% Predicado para eliminar pares de un cuadro
eliminar_pares_de_cuadro(Posibilidades, Cuadro, ParesUnicos, NuevaPosibilidades) :-
    % Calcula los índices de las celdas en el cuadro
    CuadroFila is (Cuadro - 1) // 3 * 3,
    CuadroColumna is (Cuadro - 1) mod 3 * 3,
    % Utiliza findall/3 para recopilar los índices del cuadro
    findall(Index, (
        between(0, 2, I), between(0, 2, J),
        Index is (CuadroFila + I) * 9 + CuadroColumna + J + 1
    ), Indices),
    % Llama a eliminar_pares_de_indices para eliminar los pares en esos índices
    eliminar_pares_de_indices(Posibilidades, Indices, ParesUnicos, NuevaPosibilidades).   

% Predicado para eliminar pares de una lista de índices
eliminar_pares_de_indices(Posibilidades, [], _, Posibilidades).
eliminar_pares_de_indices(Posibilidades, [Indice|RestoIndices], ParesUnicos, NuevaPosibilidades) :-
    % Obtiene la posibilidad actual en el índice dado
    nth1(Indice, Posibilidades, PosibilidadActual),
    % Elimina los pares de la posibilidad actual
    eliminar_pares_de_posibilidad(PosibilidadActual, ParesUnicos, NuevaPosibilidad),
    % Reemplaza la posibilidad actualizada en la lista de posibilidades
    replace(Posibilidades, Indice, NuevaPosibilidad, TempPosibilidades),
    % Continúa con los siguientes índices
    eliminar_pares_de_indices(TempPosibilidades, RestoIndices, ParesUnicos, NuevaPosibilidades).

% Predicado para eliminar pares de una posibilidad
eliminar_pares_de_posibilidad(Posibilidad, [], Posibilidad).
eliminar_pares_de_posibilidad(Posibilidad, [[Num1, Num2]|RestoPares], NuevaPosibilidad) :-
    % Si la posibilidad actual es uno de los pares únicos, no eliminar los números del par
    (   length(Posibilidad, 2), member(Num1, Posibilidad), member(Num2, Posibilidad) ->
        eliminar_pares_de_posibilidad(Posibilidad, RestoPares, NuevaPosibilidad)
    ;   eliminar_numero(Posibilidad, Num1, TempPosibilidad1),
        eliminar_numero(TempPosibilidad1, Num2, TempPosibilidad2),
        eliminar_pares_de_posibilidad(TempPosibilidad2, RestoPares, NuevaPosibilidad)
    ).


% Predicado para eliminar un número de una lista
eliminar_numero([], _, []).
eliminar_numero([H|T], H, T).
eliminar_numero([H|T], Num, [H|R]) :-
    eliminar_numero(T, Num, R).


probar_regla_2 :-
    % Obtiene el tablero de Sudoku (asegúrate de que sudoku1/1 esté definido)
    sudoku1(Tablero),
    % Genera las posibilidades para el tablero
    generar_posibilidades(Tablero, Posibilidades),
    % Aplica la Regla 2 con contador
    aplicar_regla_2(Posibilidades, NuevaPosibilidades, 0, Count),
    writeln("Posibilidades después de aplicar la Regla 2:"),
    imprimir_sudoku(NuevaPosibilidades),
    format("Número de llamadas a aplicar_regla_2: ~w~n", [Count]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Predicado para aplicar las Reglas 0 y 2 en una iteración con contadores
%% Se cuentan:
%%   - R0Count: veces que se aplica la Regla 0
%%   - R2Count: veces que se aplica la Regla 2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

resolver_reglas_0_y_2(Sudoku, NuevoSudoku, R0CountIn, R0CountOut, R2CountIn, R2CountOut) :-
    % Genera las posibilidades a partir del Sudoku
    generar_posibilidades(Sudoku, Posibilidades),
    % Aplica la Regla 2 con contador
    aplicar_regla_2(Posibilidades, PosibilidadesRegla2, R2CountIn, R2CountTemp),
    % Aplica la Regla 0 con contador
    aplicar_regla_0(Sudoku, PosibilidadesRegla2, NuevoSudoku, R0CountIn, R0CountTemp),
    % Propaga los contadores resultantes
    R0CountOut = R0CountTemp,
    R2CountOut = R2CountTemp.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Predicado para iterar las Reglas 0 y 2 hasta que no haya mas cambios,
%% acumulando:
%%   - IterCount: número de iteraciones realizadas.
%%   - R0Count: total de llamadas a la Regla 0.
%%   - R2Count: total de llamadas a la Regla 2.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

iterar_reglas_0_y_2(Sudoku, NuevoSudoku, IterCountIn, IterCountOut, R0CountIn, R0CountOut, R2CountIn, R2CountOut) :-
    resolver_reglas_0_y_2(Sudoku, SudokuIntermedio, R0CountIn, R0CountTemp, R2CountIn, R2CountTemp),
    NewIterCount is IterCountIn + 1,
    (   Sudoku \= SudokuIntermedio ->
        iterar_reglas_0_y_2(SudokuIntermedio, NuevoSudoku, NewIterCount, IterCountOut, R0CountTemp, R0CountOut, R2CountTemp, R2CountOut)
    ;   NuevoSudoku = Sudoku,
        IterCountOut = NewIterCount,
        R0CountOut = R0CountTemp,
        R2CountOut = R2CountTemp
    ).

% Wrapper que inicia ambos contadores en 0 y devuelve IterCount, R0Count y R2Count
iterar_reglas_0_y_2(Sudoku, NuevoSudoku, IterCount) :-
    iterar_reglas_0_y_2(Sudoku, NuevoSudoku, 0, IterCount, 0, R0Count, 0, R2Count).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Predicado de prueba para las Reglas 0 y 2 con contadores
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

probar_reglas_0_y_2 :-
    % Obtiene el tablero de Sudoku (asegúrate de que sudoku4/1 esté definido y retorne un tablero válido)
    sudoku4(Tablero),
    imprimir_sudoku(Tablero),
    % Itera las reglas 0 y 2 acumulando el número de iteraciones y las llamadas a cada regla
    iterar_reglas_0_y_2(Tablero, NuevoTablero, IterCount, R0Count, R2Count),
    writeln("Sudoku después de aplicar las Reglas 0 y 2:"),
    imprimir_sudoku(NuevoTablero),
    generar_posibilidades(NuevoTablero, Posibilidades),
    imprimir_sudoku(Posibilidades),
    format("Número de iteraciones (resolver_reglas_0_y_2): ~w~n", [IterCount]),
    format("Número de llamadas a aplicar_regla_0: ~w~n", [R0Count]),
    format("Número de llamadas a aplicar_regla_2: ~w~n", [R2Count]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Predicado para aplicar las Reglas 0, 1 y 2 en una iteración con contadores
%% Se cuentan:
%%   - R0Count: veces que se aplica la Regla 0 (actualizar el Sudoku)
%%   - R1Count: veces que se aplica la Regla 1 (actualización de posibilidades)
%%   - R2Count: veces que se aplica la Regla 2 (actualización de posibilidades según pares)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

resolver_reglas_0_y_1_y_2(Sudoku, NuevoSudoku, 
                           R0CountIn, R0CountOut, 
                           R1CountIn, R1CountOut, 
                           R2CountIn, R2CountOut) :-
    % Genera las posibilidades a partir del Sudoku
    generar_posibilidades(Sudoku, Posibilidades),
    % Aplica la Regla 2 con contador (ya definido con contador)
    aplicar_regla_2(Posibilidades, PosibilidadesRegla2, R2CountIn, R2CountTemp),
    % Aplica la Regla 1 con contador (se asume que resolver_regla_1/4 ya fue definido)
    resolver_regla_1(PosibilidadesRegla2, Fin, R1CountIn, R1CountTemp),
    % Aplica la Regla 0 con contador (ya definido)
    aplicar_regla_0(Sudoku, Fin, NuevoSudoku, R0CountIn, R0CountTemp),
    % Propaga los contadores (simplemente se asigna el valor resultante)
    R2CountOut = R2CountTemp,
    R1CountOut = R1CountTemp,
    R0CountOut = R0CountTemp.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Predicado iterativo que repite la aplicación de las reglas hasta
%% que no haya mas cambios, acumulando los contadores
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
iterar_reglas_0_y_1_y_2(Sudoku, NuevoSudoku, 
                        IterCountIn, IterCountOut, 
                        R0CountIn, R0CountOut, 
                        R1CountIn, R1CountOut, 
                        R2CountIn, R2CountOut) :-
    resolver_reglas_0_y_1_y_2(Sudoku, SudokuIntermedio, 
                              R0CountIn, R0CountTemp, 
                              R1CountIn, R1CountTemp, 
                              R2CountIn, R2CountTemp),
    NewIterCount is IterCountIn + 1,
    (   Sudoku \= SudokuIntermedio ->
        iterar_reglas_0_y_1_y_2(SudokuIntermedio, NuevoSudoku, 
                                NewIterCount, IterCountOut, 
                                R0CountTemp, R0CountOut, 
                                R1CountTemp, R1CountOut, 
                                R2CountTemp, R2CountOut)
    ;   NuevoSudoku = Sudoku,
        IterCountOut = NewIterCount,
        R0CountOut = R0CountTemp,
        R1CountOut = R1CountTemp,
        R2CountOut = R2CountTemp
    ).

% Wrapper que inicia todos los contadores en 0
iterar_reglas_0_y_1_y_2(Sudoku, NuevoSudoku, IterCount, R0Count, R1Count, R2Count) :-
    iterar_reglas_0_y_1_y_2(Sudoku, NuevoSudoku, 0, IterCount, 0, R0Count, 0, R1Count, 0, R2Count).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Predicado de prueba para las Reglas 0, 1 y 2 con contadores
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
probar_reglas_0_y_1_y_2 :-
    % Obtiene el tablero de Sudoku (asegúrate de que sudoku4/1 esté definido y retorne un formato adecuado)
    sudoku4(Tablero),
    imprimir_sudoku(Tablero),
    % Itera las reglas acumulando el número de iteraciones y las llamadas a cada regla
    iterar_reglas_0_y_1_y_2(Tablero, NuevoTablero, IterCount, R0Count, R1Count, R2Count),
    writeln("Sudoku después de aplicar las Reglas 0, 1 y 2:"),
    imprimir_sudoku(NuevoTablero),
    generar_posibilidades(NuevoTablero, Posibilidades),
    imprimir_sudoku(Posibilidades),
    format("Número de iteraciones: ~w~n", [IterCount]),
    format("Número de llamadas a aplicar_regla_0: ~w~n", [R0Count]),
    format("Número de llamadas a aplicar_regla_1: ~w~n", [R1Count]),
    format("Número de llamadas a aplicar_regla_2: ~w~n", [R2Count]).

    %----------------------REGLA 3------------------------------

    % Predicado para encontrar tríos de números que aparecen solo en tres casillas
    encontrar_trios_unicos(PosibilidadesGrupo, TriosUnicos) :-
        % Utiliza findall para encontrar tríos únicos en el grupo
        findall([Num1, Num2, Num3], (
            member(Posibilidad, PosibilidadesGrupo),
            length(Posibilidad, 3),
            [Num1, Num2, Num3] = Posibilidad,
            contar_ocurrencias(PosibilidadesGrupo, [Num1, Num2, Num3], 3)
        ), TriosUnicos).

    % Predicado para eliminar tríos de una fila
    eliminar_trios_de_fila(Posibilidades, Fila, TriosUnicos, NuevaPosibilidades) :-
        % Calcula el rango de índices correspondientes a la fila
        Inicio is (Fila - 1) * 9 + 1,
        Fin is Fila * 9,
        % Utiliza findall para recopilar los índices en el rango
        findall(Index, between(Inicio, Fin, Index), Indices),
        % Llama a eliminar_trios_de_indices para eliminar los tríos en esos índices
        eliminar_trios_de_indices(Posibilidades, Indices, TriosUnicos, NuevaPosibilidades).

    % Predicado para eliminar tríos de una columna
    eliminar_trios_de_columna(Posibilidades, Columna, TriosUnicos, NuevaPosibilidades) :-
        % Utiliza findall para recopilar los índices de la columna
        findall(Index, (between(0, 8, I), Index is Columna + I * 9), Indices),
        % Llama a eliminar_trios_de_indices para eliminar los tríos en esos índices
        eliminar_trios_de_indices(Posibilidades, Indices, TriosUnicos, NuevaPosibilidades).

    % Predicado para eliminar tríos de un cuadro
    eliminar_trios_de_cuadro(Posibilidades, Cuadro, TriosUnicos, NuevaPosibilidades) :-
        % Calcula los índices de las celdas en el cuadro
        CuadroFila is (Cuadro - 1) // 3 * 3,
        CuadroColumna is (Cuadro - 1) mod 3 * 3,
        % Utiliza findall para recopilar los índices del cuadro
        findall(Index, (
            between(0, 2, I), between(0, 2, J),
            Index is (CuadroFila + I) * 9 + CuadroColumna + J + 1
        ), Indices),
        % Llama a eliminar_trios_de_indices para eliminar los tríos en esos índices
        eliminar_trios_de_indices(Posibilidades, Indices, TriosUnicos, NuevaPosibilidades).

    % Predicado para eliminar tríos de una lista de índices
    eliminar_trios_de_indices(Posibilidades, [], _, Posibilidades).
    eliminar_trios_de_indices(Posibilidades, [Indice|RestoIndices], TriosUnicos, NuevaPosibilidades) :-
        % Obtiene la posibilidad actual en el índice dado
        nth1(Indice, Posibilidades, PosibilidadActual),
        % Elimina los tríos de la posibilidad actual
        eliminar_trios_de_posibilidad(PosibilidadActual, TriosUnicos, NuevaPosibilidad),
        % Reemplaza la posibilidad actualizada en la lista de posibilidades
        replace(Posibilidades, Indice, NuevaPosibilidad, TempPosibilidades),
        % Continúa con los siguientes índices
        eliminar_trios_de_indices(TempPosibilidades, RestoIndices, TriosUnicos, NuevaPosibilidades).

    % Predicado para eliminar tríos de una posibilidad
    eliminar_trios_de_posibilidad(Posibilidad, [], Posibilidad).
    eliminar_trios_de_posibilidad(Posibilidad, [[Num1, Num2, Num3]|RestoTrios], NuevaPosibilidad) :-
        % Si la posibilidad actual es uno de los tríos únicos, no eliminar los números del trío
        (   length(Posibilidad, 3), member(Num1, Posibilidad), member(Num2, Posibilidad), member(Num3, Posibilidad) ->
            eliminar_trios_de_posibilidad(Posibilidad, RestoTrios, NuevaPosibilidad)
        ;   eliminar_numero(Posibilidad, Num1, TempPosibilidad1),
            eliminar_numero(TempPosibilidad1, Num2, TempPosibilidad2),
            eliminar_numero(TempPosibilidad2, Num3, TempPosibilidad3),
            eliminar_trios_de_posibilidad(TempPosibilidad3, RestoTrios, NuevaPosibilidad)
        ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Aplicar la Regla 3 con contador
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Se aplica la Regla 3 a filas, columnas y cuadros.
aplicar_regla_3(Posibilidades, NuevaPosibilidades, CountIn, CountOut) :-
    NewCount is CountIn + 1,
    % Aplicar la Regla 3 a todas las filas:
    aplicar_regla_3_a_todas_filas(Posibilidades, Temp1),
    % Aplicar la Regla 3 a todas las columnas:
    aplicar_regla_3_a_todas_columnas(Temp1, Temp2),
    % Aplicar la Regla 3 a todos los cuadros:
    aplicar_regla_3_a_todos_cuadros(Temp2, NuevaPosibilidades),
    CountOut = NewCount.

% Wrapper sin contador explícito para aplicar_regla_3.
aplicar_regla_3(Posibilidades, NuevaPosibilidades) :-
    aplicar_regla_3(Posibilidades, NuevaPosibilidades, 0, _).

    % Predicado para aplicar la Regla 3 a todas las filas
    aplicar_regla_3_a_todas_filas(Posibilidades, NuevaPosibilidades) :-
        aplicar_regla_3_a_todas_filas(Posibilidades, 1, NuevaPosibilidades).

    % Caso base: cuando se han procesado todas las filas
    aplicar_regla_3_a_todas_filas(Posibilidades, 9, Posibilidades).
    % Caso recursivo: procesar una fila y continuar con la siguiente
    aplicar_regla_3_a_todas_filas(Posibilidades, Fila, NuevaPosibilidades) :-
        % Obtener las posibilidades de la fila actual
        obtener_posibilidades_fila(Posibilidades, Fila, PosibilidadesFila),
        % Encontrar tríos únicos en la fila
        encontrar_trios_unicos(PosibilidadesFila, TriosUnicos),
        % Eliminar los tríos únicos de la fila
        eliminar_trios_de_fila(Posibilidades, Fila, TriosUnicos, TempPosibilidades),
        % Continuar con la siguiente fila
        FilaSiguiente is Fila + 1,
        aplicar_regla_3_a_todas_filas(TempPosibilidades, FilaSiguiente, NuevaPosibilidades).

    % Predicado para aplicar la Regla 3 a todas las columnas
    aplicar_regla_3_a_todas_columnas(Posibilidades, NuevaPosibilidades) :-
        aplicar_regla_3_a_todas_columnas(Posibilidades, 1, NuevaPosibilidades).

    % Caso base: cuando se han procesado todas las columnas
    aplicar_regla_3_a_todas_columnas(Posibilidades, 9, Posibilidades).
    % Caso recursivo: procesar una columna y continuar con la siguiente
    aplicar_regla_3_a_todas_columnas(Posibilidades, Columna, NuevaPosibilidades) :-
        % Obtener las posibilidades de la columna actual
        obtener_posibilidades_columna(Posibilidades, Columna, PosibilidadesColumna),
        % Encontrar tríos únicos en la columna
        encontrar_trios_unicos(PosibilidadesColumna, TriosUnicos),
        % Eliminar los tríos únicos de la columna
        eliminar_trios_de_columna(Posibilidades, Columna, TriosUnicos, TempPosibilidades),
        % Continuar con la siguiente columna
        ColumnaSiguiente is Columna + 1,
        aplicar_regla_3_a_todas_columnas(TempPosibilidades, ColumnaSiguiente, NuevaPosibilidades).

    % Predicado para aplicar la Regla 3 a todos los cuadros
    aplicar_regla_3_a_todos_cuadros(Posibilidades, NuevaPosibilidades) :-
        aplicar_regla_3_a_todos_cuadros(Posibilidades, 1, NuevaPosibilidades).

    % Caso base: cuando se han procesado todos los cuadros
    aplicar_regla_3_a_todos_cuadros(Posibilidades, 9, Posibilidades).
    % Caso recursivo: procesar un cuadro y continuar con el siguiente
    aplicar_regla_3_a_todos_cuadros(Posibilidades, Cuadro, NuevaPosibilidades) :-
        % Obtener las posibilidades del cuadro actual
        obtener_posibilidades_cuadro(Posibilidades, Cuadro, PosibilidadesCuadro),
        % Encontrar tríos únicos en el cuadro
        encontrar_trios_unicos(PosibilidadesCuadro, TriosUnicos),
        % Eliminar los tríos únicos del cuadro
        eliminar_trios_de_cuadro(Posibilidades, Cuadro, TriosUnicos, TempPosibilidades),
        % Continuar con el siguiente cuadro
        CuadroSiguiente is Cuadro + 1,
        aplicar_regla_3_a_todos_cuadros(TempPosibilidades, CuadroSiguiente, NuevaPosibilidades).


   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Resolver la Regla 3 (similar a la Regla 2) con contador
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

resolver_regla_3(Sudoku, NuevoSudoku, CountIn, CountOut) :-
    generar_posibilidades(Sudoku, Posibilidades),
    aplicar_regla_3(Posibilidades, NuevaPosibilidades, CountIn, CountOut),
    aplicar_regla_0(Sudoku, NuevaPosibilidades, NuevoSudoku).

% Wrapper sin contador explícito para resolver_regla_3
resolver_regla_3(Sudoku, NuevoSudoku) :-
    resolver_regla_3(Sudoku, NuevoSudoku, 0, _).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Predicado para probar la Regla 3 (actualizado)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

probar_regla_3 :-
    % Obtiene el tablero de Sudoku (asegúrate de que sudoku8/1 esté definido y retorne un tablero válido)
    sudoku8(Tablero),
    imprimir_sudoku(Tablero),
    % Aplica resolver_regla_3 iniciando el contador en 0
    resolver_regla_3(Tablero, NuevoTablero, 0, Count),
    writeln("Sudoku después de aplicar la Regla 3:"),
    imprimir_sudoku(NuevoTablero),
    generar_posibilidades(NuevoTablero, Posibilidades),
    imprimir_sudoku(Posibilidades),
    format("Número de llamadas a resolver_regla_3: ~w~n", [Count]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Resolver las Reglas 0 y 3 con contador
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Se genera la lista de posibilidades a partir del Sudoku, se aplica la Regla 3 (con contador)
% y luego se aplica la Regla 0 (con contador). Se cuentan:
%   R3Count: llamadas a la Regla 3.
%   R0Count: llamadas a la Regla 0.
resolver_reglas_0_y_3(Sudoku, NuevoSudoku, 
                       R0CountIn, R0CountOut, 
                       R3CountIn, R3CountOut) :-
    generar_posibilidades(Sudoku, Posibilidades),
    aplicar_regla_3(Posibilidades, PosibilidadesRegla3, R3CountIn, R3CountTemp),
    aplicar_regla_0(Sudoku, PosibilidadesRegla3, NuevoSudoku, R0CountIn, R0CountTemp),
    R3CountOut = R3CountTemp,
    R0CountOut = R0CountTemp.

% Wrapper sin contador explícito para resolver_reglas_0_y_3.
resolver_reglas_0_y_3(Sudoku, NuevoSudoku) :-
    resolver_reglas_0_y_3(Sudoku, NuevoSudoku, 0, _, 0, _).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Iterar las Reglas 0 y 3 con contador
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Se itera resolver_reglas_0_y_3 hasta que el Sudoku no cambie,
% acumulando:
%   IterCount: número de iteraciones,
%   R0Count: total de llamadas a la Regla 0,
%   R3Count: total de llamadas a la Regla 3.
iterar_reglas_0_y_3(Sudoku, NuevoSudoku, 
                    IterCountIn, IterCountOut, 
                    R0CountIn, R0CountOut, 
                    R3CountIn, R3CountOut) :-
    resolver_reglas_0_y_3(Sudoku, SudokuIntermedio, R0CountIn, R0CountTemp, R3CountIn, R3CountTemp),
    NewIterCount is IterCountIn + 1,
    (   Sudoku \= SudokuIntermedio ->
        iterar_reglas_0_y_3(SudokuIntermedio, NuevoSudoku, NewIterCount, IterCountOut, 
                            R0CountTemp, R0CountOut, R3CountTemp, R3CountOut)
    ;   NuevoSudoku = Sudoku,
        IterCountOut = NewIterCount,
        R0CountOut = R0CountTemp,
        R3CountOut = R3CountTemp
    ).

% Wrapper que inicia todos los contadores en 0
iterar_reglas_0_y_3(Sudoku, NuevoSudoku, IterCount) :-
    iterar_reglas_0_y_3(Sudoku, NuevoSudoku, 0, IterCount, 0, R0Count, 0, R3Count).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Predicado de prueba para las Reglas 0 y 3 con contadores
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

probar_reglas_0_y_3 :-
    % Obtiene el tablero de Sudoku (asegúrate de que sudoku8/1 esté definido)
    sudoku8(Tablero),
    imprimir_sudoku(Tablero),
    % Itera las Reglas 0 y 3 acumulando los contadores
    iterar_reglas_0_y_3(Tablero, NuevoTablero, IterCount, R0Count, R3Count),
    writeln("Sudoku después de aplicar las Reglas 0 y 3:"),
    imprimir_sudoku(NuevoTablero),
    generar_posibilidades(NuevoTablero, Posibilidades),
    imprimir_sudoku(Posibilidades),
    format("Número de iteraciones: ~w~n", [IterCount]),
    format("Número de llamadas a aplicar_regla_0: ~w~n", [R0Count]),
    format("Número de llamadas a aplicar_regla_3: ~w~n", [R3Count]).

    
    
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Resolver las Reglas 0, 1, 2 y 3 (una iteración) con contadores
%% Se cuentan:
%%   - R0Count: veces que se aplica la Regla 0 (actualización del Sudoku)
%%   - R1Count: veces que se aplica la Regla 1 (actualización de posibilidades)
%%   - R2Count: veces que se aplica la Regla 2 (filas, columnas y cuadros)
%%   - R3Count: veces que se aplica la Regla 3 (filas, columnas y cuadros)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

resolver_reglas_0_y_1_y_2_y_3(Sudoku, NuevoSudoku, 
    R0CountIn, R0CountOut, 
    R1CountIn, R1CountOut, 
    R2CountIn, R2CountOut, 
    R3CountIn, R3CountOut) :-
    generar_posibilidades(Sudoku, Posibilidades),
    % Aplica la Regla 3 con contador:
    aplicar_regla_3(Posibilidades, PosRegla3, R3CountIn, R3CountTemp),
    % Aplica la Regla 2 con contador:
    aplicar_regla_2(PosRegla3, PosRegla2, R2CountIn, R2CountTemp),
    % Aplica la Regla 1 con contador:
    resolver_regla_1(PosRegla2, PosRegla1, R1CountIn, R1CountTemp),
    % Aplica la Regla 0 con contador:
    aplicar_regla_0(Sudoku, PosRegla1, NuevoSudoku, R0CountIn, R0CountTemp),
    % Propaga los contadores resultantes:
    R3CountOut = R3CountTemp,
    R2CountOut = R2CountTemp,
    R1CountOut = R1CountTemp,
    R0CountOut = R0CountTemp.

% Wrapper sin contadores explícitos para resolver_reglas_0_y_1_y_2_y_3:
resolver_reglas_0_y_1_y_2_y_3(Sudoku, NuevoSudoku) :-
    resolver_reglas_0_y_1_y_2_y_3(Sudoku, NuevoSudoku, 0, _, 0, _, 0, _, 0, _).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Iterar las Reglas 0, 1, 2 y 3 hasta que el Sudoku no cambie, 
%% acumulando:
%%   - IterCount: número de iteraciones.
%%   - R0Count, R1Count, R2Count, R3Count: totales de llamadas a cada regla.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

iterar_reglas_0_y_1_y_2_y_3(Sudoku, NuevoSudoku, 
    IterCountIn, IterCountOut, 
    R0CountIn, R0CountOut, 
    R1CountIn, R1CountOut, 
    R2CountIn, R2CountOut, 
    R3CountIn, R3CountOut) :-
    resolver_reglas_0_y_1_y_2_y_3(Sudoku, SudokuIntermedio,
         R0CountIn, R0CountTemp,
         R1CountIn, R1CountTemp,
         R2CountIn, R2CountTemp,
         R3CountIn, R3CountTemp),
    NewIterCount is IterCountIn + 1,
    (   Sudoku \= SudokuIntermedio ->
         iterar_reglas_0_y_1_y_2_y_3(SudokuIntermedio, NuevoSudoku,
              NewIterCount, IterCountOut,
              R0CountTemp, R0CountOut,
              R1CountTemp, R1CountOut,
              R2CountTemp, R2CountOut,
              R3CountTemp, R3CountOut)
    ;   NuevoSudoku = Sudoku,
         IterCountOut = NewIterCount,
         R0CountOut = R0CountTemp,
         R1CountOut = R1CountTemp,
         R2CountOut = R2CountTemp,
         R3CountOut = R3CountTemp
    ).

% Wrapper que inicia todos los contadores en 0:
iterar_reglas_0_y_1_y_2_y_3(Sudoku, NuevoSudoku, IterCount, R0Count, R1Count, R2Count, R3Count) :-
    iterar_reglas_0_y_1_y_2_y_3(Sudoku, NuevoSudoku, 0, IterCount, 0, R0Count, 0, R1Count, 0, R2Count, 0, R3Count).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Predicado de prueba para las Reglas 0, 1, 2 y 3 con contadores
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

probar_reglas_0_y_1_y_2_y_3 :-
    % Obtiene el tablero de Sudoku (asegúrate de que sudoku6/1 esté definido)
    sudoku6(Tablero),
    imprimir_sudoku(Tablero),
    % Itera las reglas acumulando contadores
    iterar_reglas_0_y_1_y_2_y_3(Tablero, NuevoTablero, IterCount, R0Count, R1Count, R2Count, R3Count),
    writeln("Sudoku después de aplicar las Reglas 0, 1, 2 y 3:"),
    imprimir_sudoku(NuevoTablero),
    generar_posibilidades(NuevoTablero, Posibilidades),
    imprimir_sudoku(Posibilidades),
    format("Número de iteraciones: ~w~n", [IterCount]),
    format("Número de llamadas a aplicar_regla_0: ~w~n", [R0Count]),
    format("Número de llamadas a resolver_regla_1: ~w~n", [R1Count]),
    format("Número de llamadas a aplicar_regla_2: ~w~n", [R2Count]),
    format("Número de llamadas a aplicar_regla_3: ~w~n", [R3Count]).


% Predicado para verificar si un Sudoku está completo
sudoku_completo(Sudoku) :-
    \+ (member(Casilla, Sudoku), \+ number(Casilla)).

% Predicado para contar el número de Sudokus completos e incompletos
contar_sudokus([], 0, 0).
contar_sudokus([Sudoku|Resto], Completos, Incompletos) :-
    contar_sudokus(Resto, CompletosResto, IncompletosResto),
    (   sudoku_completo(Sudoku) ->
        Completos is CompletosResto + 1,
        Incompletos is IncompletosResto
    ;   Completos is CompletosResto,
        Incompletos is IncompletosResto + 1
    ).

% Predicado para resolver la Regla 0 para una lista de Sudokus

resolver_regla_0_para_lista([], []).
resolver_regla_0_para_lista([Sudoku|Resto], [SudokuResuelto|RestoResueltos]) :-
    resolver_regla_0(Sudoku, SudokuResuelto),
    resolver_regla_0(Sudoku, SudokuResuelto,0,Count),
    resolver_regla_0_para_lista(Resto, RestoResueltos).

%Las iteraciones aquí funcionan de manera que se llama a la regla 1, 2 o 3 y luego la regla 0 hace multiples iteraciones sobre si misma hasta que no hay cambios
resolver_regla_1_para_lista([], []).
resolver_regla_1_para_lista([Sudoku|Resto], [SudokuResuelto|RestoResueltos]) :-
    iterar_reglas_0_y_1(Sudoku, SudokuResuelto, IterCount),
    iterar_reglas_0_y_1(Sudoku, SudokuResuelto, IterCount),
    resolver_regla_1_para_lista(Resto, RestoResueltos).

resolver_regla_2_para_lista([], []).
resolver_regla_2_para_lista([Sudoku|Resto], [SudokuResuelto|RestoResueltos]) :-
    iterar_reglas_0_y_2(Sudoku, SudokuResuelto, IterCount),
    iterar_reglas_0_y_2(Sudoku, SudokuResuelto, IterCount),
    resolver_regla_2_para_lista(Resto, RestoResueltos).

resolver_regla_3_para_lista([], []).
resolver_regla_3_para_lista([Sudoku|Resto], [SudokuResuelto|RestoResueltos]) :-
    iterar_reglas_0_y_3(Sudoku, SudokuResuelto, IterCount),
    iterar_reglas_0_y_3(Sudoku, SudokuResuelto, IterCount),
    resolver_regla_3_para_lista(Resto, RestoResueltos).


% Predicado para imprimir barras
imprimir_barras(0).
imprimir_barras(N) :-
    N > 0,
    write('|'),
    N1 is N - 1,
    imprimir_barras(N1).

% Predicado para imprimir el número de Sudokus completos e incompletos con barras
imprimir_conteo_sudokus(Completos, Incompletos) :-
    format('Numero de Sudokus completos  : ~w ', [Completos]),
    imprimir_barras(Completos), nl,
    format('Numero de Sudokus incompletos: ~w ', [Incompletos]),
    imprimir_barras(Incompletos), nl.


% Predicado para probar la cuenta de Sudokus completos e incompletos
probar_contar_sudokus_regla_0 :-
    Sudokus = [sudoku, sudoku1, sudoku2, sudoku3, sudoku4, sudoku5, sudoku6, sudoku7, sudoku8, sudoku9, sudoku10, sudoku11, sudoku12, sudoku13,sudoku14, sudoku15, sudoku16, sudoku17, sudoku18, sudoku19, sudoku20, sudoku21, sudoku22, sudoku23, sudoku24, sudoku25 ],
    probar_contar_sudokus_regla_0(Sudokus).

probar_contar_sudokus_regla_0(Sudokus) :-
    findall(Sudoku, (member(Pred, Sudokus), call(Pred, Sudoku)), ListaSudokus),
    resolver_regla_0_para_lista(ListaSudokus, ListaSudokusResueltos),
    contar_sudokus(ListaSudokusResueltos, Completos, Incompletos),
    %format('Numero de Sudokus completos despues de aplicar la Regla 0: ~w~n', [Completos]),
    %format('Numero de Sudokus incompletos despues de aplicar la Regla 0: ~w~n', [Incompletos]),
    imprimir_conteo_sudokus(Completos, Incompletos).

% Predicado para probar la cuenta de Sudokus completos e incompletos
probar_contar_sudokus_regla_1 :-
    Sudokus = [sudoku, sudoku1, sudoku2, sudoku3, sudoku4, sudoku5, sudoku6, sudoku7, sudoku8, sudoku9, sudoku10, sudoku11, sudoku12, sudoku13,sudoku14, sudoku15, sudoku16, sudoku17, sudoku18, sudoku19, sudoku20, sudoku21, sudoku22, sudoku23, sudoku24, sudoku25 ],
    probar_contar_sudokus_regla_1(Sudokus).

probar_contar_sudokus_regla_1(Sudokus) :-
    findall(Sudoku, (member(Pred, Sudokus), call(Pred, Sudoku)), ListaSudokus),
    resolver_regla_1_para_lista(ListaSudokus, ListaSudokusResueltos),
    contar_sudokus(ListaSudokusResueltos, Completos, Incompletos),
    %format('Numero de Sudokus completos despues de aplicar la Regla 0: ~w~n', [Completos]),
    %format('Numero de Sudokus incompletos despues de aplicar la Regla 0: ~w~n', [Incompletos]),
    imprimir_conteo_sudokus(Completos, Incompletos).

% Predicado para probar la cuenta de Sudokus completos e incompletos
probar_contar_sudokus_regla_2 :-
    Sudokus = [sudoku, sudoku1, sudoku2, sudoku3, sudoku4, sudoku5, sudoku6, sudoku7, sudoku8, sudoku9, sudoku10, sudoku11, sudoku12, sudoku13,sudoku14, sudoku15, sudoku16, sudoku17, sudoku18, sudoku19, sudoku20, sudoku21, sudoku22, sudoku23, sudoku24, sudoku25 ],
    probar_contar_sudokus_regla_2(Sudokus).

probar_contar_sudokus_regla_2(Sudokus):-
    findall(Sudoku, (member(Pred, Sudokus), call(Pred, Sudoku)), ListaSudokus),
    resolver_regla_2_para_lista(ListaSudokus, ListaSudokusResueltos),
    contar_sudokus(ListaSudokusResueltos, Completos, Incompletos),
    %format('Numero de Sudokus completos despues de aplicar la Regla 0: ~w~n', [Completos]),
    %format('Numero de Sudokus incompletos despues de aplicar la Regla 0: ~w~n', [Incompletos]),
    imprimir_conteo_sudokus(Completos, Incompletos).


probar_contar_sudokus_regla_3 :-
    Sudokus = [sudoku, sudoku1, sudoku2, sudoku3, sudoku4, sudoku5, sudoku6, sudoku7, sudoku8, sudoku9, sudoku10, sudoku11, sudoku12, sudoku13,sudoku14, sudoku15, sudoku16, sudoku17, sudoku18, sudoku19, sudoku20, sudoku21, sudoku22, sudoku23, sudoku24, sudoku25 ],
    probar_contar_sudokus_regla_3(Sudokus).

probar_contar_sudokus_regla_3(Sudokus) :-
    findall(Sudoku, (member(Pred, Sudokus), call(Pred, Sudoku)), ListaSudokus),
    resolver_regla_3_para_lista(ListaSudokus, ListaSudokusResueltos),
    contar_sudokus(ListaSudokusResueltos, Completos, Incompletos),
    %format('Numero de Sudokus completos despues de aplicar la Regla 0: ~w~n', [Completos]),
    %format('Numero de Sudokus incompletos despues de aplicar la Regla 0: ~w~n', [Incompletos]),
    imprimir_conteo_sudokus(Completos, Incompletos).

probar_contar_sudokus_por_nivel :-

    SudokusMuyFacil = [sudoku, sudoku1, sudoku2, sudoku3, sudoku4],
    SudokusFacil = [sudoku5, sudoku6, sudoku7, sudoku8, sudoku9],
    SudokusMedio = [sudoku10, sudoku11, sudoku12, sudoku13, sudoku14],
    SudokusDificil = [sudoku15, sudoku16, sudoku17, sudoku18, sudoku19],
    SudokusMuyDificil = [sudoku20, sudoku21, sudoku22, sudoku23, sudoku24],
    SudokuPrueba3 = [sudoku25],

    writeln('Conteo de Sudokus completos e incompletos por nivel regla 0:'),
    writeln('Muy Facil:'),
    probar_contar_sudokus_regla_0(SudokusMuyFacil),
    writeln('Facil:'),
    probar_contar_sudokus_regla_0(SudokusFacil),
    writeln('Medio:'),
    probar_contar_sudokus_regla_0(SudokusMedio),
    writeln('Dificil:'),
    probar_contar_sudokus_regla_0(SudokusDificil),
    writeln('Muy Dificil:'),
    probar_contar_sudokus_regla_0(SudokusMuyDificil),
    writeln('Prueba 3:'),
    probar_contar_sudokus_regla_0(SudokuPrueba3),

    writeln('Conteo de Sudokus completos e incompletos por nivel regla 1:'),
    writeln('Muy Facil:'),
    probar_contar_sudokus_regla_1(SudokusMuyFacil),
    writeln('Facil:'),
    probar_contar_sudokus_regla_1(SudokusFacil),
    writeln('Medio:'),
    probar_contar_sudokus_regla_1(SudokusMedio),
    writeln('Dificil:'),
    probar_contar_sudokus_regla_1(SudokusDificil),
    writeln('Muy Dificil:'),
    probar_contar_sudokus_regla_1(SudokusMuyDificil),
    writeln('Prueba 3:'),
    probar_contar_sudokus_regla_1(SudokuPrueba3),

    writeln('Conteo de Sudokus completos e incompletos por nivel regla 2:'),
    writeln('Muy Facil:'),
    probar_contar_sudokus_regla_2(SudokusMuyFacil),
    writeln('Facil:'),
    probar_contar_sudokus_regla_2(SudokusFacil),
    writeln('Medio:'),
    probar_contar_sudokus_regla_2(SudokusMedio),
    writeln('Dificil:'),
    probar_contar_sudokus_regla_2(SudokusDificil),
    writeln('Muy Dificil:'),
    probar_contar_sudokus_regla_2(SudokusMuyDificil),
    writeln('Prueba 3:'),
    probar_contar_sudokus_regla_2(SudokuPrueba3),

    writeln('Conteo de Sudokus completos e incompletos por nivel regla 3:'),
    writeln('Muy Facil:'),
    probar_contar_sudokus_regla_3(SudokusMuyFacil),
    writeln('Facil:'),
    probar_contar_sudokus_regla_3(SudokusFacil),
    writeln('Medio:'),
    probar_contar_sudokus_regla_3(SudokusMedio),
    writeln('Dificil:'),
    probar_contar_sudokus_regla_3(SudokusDificil),
    writeln('Muy Dificil:'),
    probar_contar_sudokus_regla_3(SudokusMuyDificil),
    writeln('Prueba 3:'),
    probar_contar_sudokus_regla_3(SudokuPrueba3).


% Predicado que valida cada elemento de la lista.
validar_sudoku([]).
validar_sudoku([H|T]) :-
    ( (integer(H), H >= 1, H =< 9)
    ; H == '.' ),
    validar_sudoku(T).

interfaz_interactiva :-
    writeln('Ingrese el Sudoku como una lista de 81 elementos (usar . para celdas vacías):'),
    read(Sudoku),
    (   length(Sudoku, 81)
    ->  (   validar_sudoku(Sudoku)
        ->  interfaz_menu(Sudoku)
        ;   writeln('Error: La lista contiene elementos no válidos. Solo se permiten números entre 1 y 9 o el caracter ".".')
        )
    ;   writeln('Error: La lista ingresada debe tener 81 elementos.')
    ).

%------------------------------------------
% Bucle principal de la interfaz
%------------------------------------------
interfaz_menu(Sudoku) :-
    writeln('------------------------------'),
    writeln('Estado actual del Sudoku:'),
    imprimir_sudoku(Sudoku),
    writeln('------------------------------'),
    writeln('Seleccione la regla a aplicar:'),
    writeln('   0 - Regla 0 (actualiza celdas con posibilidad única)'),
    writeln('   1 - Regla 1 (eliminación de únicos globales)'),
    writeln('   2 - Regla 2 (eliminación de pares únicos)'),
    writeln('   3 - Regla 3 (eliminación de tríos únicos)'),
    writeln('   t - Terminar la resolución'),
    read(Opcion),
    (   Opcion == t
    ->  writeln('Resolución terminada.')
    ;   aplicar_regla_interactiva(Opcion, Sudoku, NuevoSudoku),
        interfaz_menu(NuevoSudoku)
    ).

%------------------------------------------
% Aplicación de cada regla (con contador)
%------------------------------------------

aplicar_regla_interactiva(Opcion, Sudoku, NuevoSudoku) :-
    ( Opcion =:= 0 ->
        writeln('Aplicando Regla 0...'),
        resolver_regla_0(Sudoku, NuevoSudoku, 0, Count0),
        format('Se realizaron ~w iteraciones de la Regla 0.~n', [Count0])
    ; Opcion =:= 1 ->
        writeln('Aplicando Regla 1...'),
        iterar_reglas_0_y_1(Sudoku, NuevoSudoku, Count1),
        format('Se realizaron ~w iteraciones de la Regla 1.~n', [Count1])
    ; Opcion =:= 2 ->
        writeln('Aplicando Regla 2...'),
        iterar_reglas_0_y_2(Sudoku, NuevoSudoku, Count2),
        format('Se realizaron ~w iteraciones de la Regla 2.~n', [Count2])
    ; Opcion =:= 3 ->
        writeln('Aplicando Regla 3...'),
        iterar_reglas_0_y_3(Sudoku, NuevoSudoku, Count3),
        format('Se realizaron ~w iteraciones de la Regla 3.~n', [Count3])
    ;   writeln('Opción no válida, no se aplicó ninguna regla.'),
        NuevoSudoku = Sudoku
    ).