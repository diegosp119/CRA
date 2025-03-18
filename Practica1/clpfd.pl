:- use_module(library(clpfd)).

% Predicado para resolver el Sudoku
resolver_sudoku(Filas) :-
    % La matriz tiene 9 filas
    length(Filas, 9),
    % Cada fila tiene 9 elementos
    maplist(same_length(Filas), Filas),
    % Convertimos la matriz en una lista de variables
    append(Filas, Variables),
    % Las variables deben estar en el dominio de 1 a 9
    Variables ins 1..9,

    % Restricción: cada fila debe tener valores únicos
    maplist(all_distinct, Filas),

    % Restricción: cada columna debe tener valores únicos
    transpose(Filas, Columnas),
    maplist(all_distinct, Columnas),

    % Restricción: cada subcuadrícula 3x3 debe tener valores únicos
    Filas = [A,B,C, D,E,F, G,H,I],
    bloques_3x3(A, B, C),
    bloques_3x3(D, E, F),
    bloques_3x3(G, H, I),

    % Buscar una solución válida
    maplist(label, Filas).

% Predicado para extraer los bloques 3x3 y asegurarse de que tengan valores únicos
bloques_3x3([], [], []).
bloques_3x3([A,B,C | R1], [D,E,F | R2], [G,H,I | R3]) :-
    all_distinct([A,B,C,D,E,F,G,H,I]),  % Cada bloque 3x3 debe ser único
    bloques_3x3(R1, R2, R3).  % Llamado recursivo para los siguientes bloques

% Predicado para mostrar el Sudoku de forma bonita
imprimir_sudoku([]).
imprimir_sudoku([Fila | Resto]) :-
    writeln(Fila),
    imprimir_sudoku(Resto).

% Predicado de ejemplo con un Sudoku parcialmente lleno
ejemplo :-
    

    Sudoku3=[[_, _, _, 2, 5, 3, _, _, _],
                [1, _, _, _, _, 7, _, _, _],
                [7, _, 9, _, _, 4, _, _, _],
                [_, _, 5, _, _, _, _, _, _],
                [_, 7, _, _, _, _, _, 5, 3],
                [_, _, 3, 4, _, _, _, _, 2],
                [_, _, _, _, 2, _, 4, _, _],
                [_, _, _, 6, _, _, _, 7, _],
                [8, 6, _, _, _, _, 9, _, _]],

    Sudoku4 = [[_, 5, _, 9, _, 8, 6, _, _],
    [8, _, _, _, _, 6, _, _, 7],
    [_, _, 6, _, 2, _, _, _, _],
    [_, _, 9, _, _, _, _, 7, _],
    [2, _, 3, _, _, _, 8, _, 9],
    [_, 1, _, _, _, _, 4, _, _],
    [_, _, _, _, 3, _, 7, _, _],
    [9, _, _, 8, _, _, _, _, 4],
    [_, _, 5, 6, _, 4, _, 3, _]],

    Sudoku2=[[_, _, 7, _, _, 5, 8, _, _],
            [_, 4, 5, 7, 1, _, _, _, 2],
            [6, _, _, _, 4, _, 3, _, 5],
            [8, 6, _, 5, _, _, _, 4, _],
            [_, _, 3, 8, _, 4, _, 6, _],
            [1, 2, _, 9, _, _, 5, 3, _],
            [_, 5, _, _, _, _, 4, 7, _],
            [7, _, _, _, _, _, _, _, 6],
            [3, 4, _, _, 6, _, _, 2, 8]],

    Sudoku=[[_, 9, _, 7, _, _, 3, _, _],
            [_, _, _, 2, _, _, _, _, 4],
            [5, _, _, _, 3, 4, _, _, _],
            [_, _, 5, _, _, _, 7, 6, _],
            [_, _, 9, _, 5, _, 1, _, _],
            [6, 3, _, _, _, _, 4, _, _],
            [_, _, _, 5, 6, _, _, _, 3],
            [4, _, _, _, 2, _, _, _, _],
            [_, _, 8, _, _, 9, _, 1, _]],

    resolver_sudoku(Sudoku),
    imprimir_sudoku(Sudoku).
