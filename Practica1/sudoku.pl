% Representación del Sudoku como una lista de 81 elementos
sudoku([., ., 3, ., 2, ., 7, ., .,
    5, ., ., ., ., 4, ., 3, .,
    ., ., 3, ., ., ., 2, 5, .,
    ., 5, ., 1, ., 6, ., ., .,
    ., 4, 8, ., 7, ., ., ., 2,
    3, 7, 6, ., 4, 8, ., ., .,
    8, ., ., ., 2, ., 7, ., 3,
    ., ., 4, ., ., 2, ., 8, .,
    ., 9, ., ., ., ., 6, .]).

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
% Predicado principal para ejecutar el programa
main :-
    sudoku(Tablero),
    imprimir_sudoku(Tablero).