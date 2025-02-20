% Dígitos permitidos
numero(1).
numero(2).
numero(3).
numero(4).
numero(5).
numero(6).
numero(7).
numero(8).
numero(9).

% posibilidades_sudoku/2: 
% Entrada: Sudoku de 81 elementos (números fijos o 'vacio')
% Salida: Lista de 81 elementos; cada celda fija se mantiene y cada 'vacio'
% se reemplaza por la lista de dígitos posibles.
posibilidades_sudoku(Sudoku, ListaPosibilidades) :-
    length(Sudoku, 81),
    generar_posibilidades(Sudoku, 0, ListaPosibilidades).

% generar_posibilidades/3: recorre cada celda del Sudoku
generar_posibilidades(_, 81, []) :- !.
generar_posibilidades(Sudoku, Index, [Pos | Resto]) :-
    nth0(Index, Sudoku, Celda),
    (   Celda == vacio
    ->  posibles_para_casilla(Sudoku, Index, Pos)
    ;   Pos = Celda
    ),
    NextIndex is Index + 1,
    generar_posibilidades(Sudoku, NextIndex, Resto).

% posibles_para_casilla/3: calcula los dígitos (1..9) que pueden ir en la celda vacía
posibles_para_casilla(Sudoku, Index, Posibles) :-
    Row is Index // 9,
    Col is Index mod 9,
    fila(Sudoku, Row, DigitosFila),
    columna(Sudoku, Col, DigitosCol),
    caja(Sudoku, Row, Col, DigitosCaja),
    append(DigitosFila, DigitosCol, RC),
    append(RC, DigitosCaja, Usados),
    sort(Usados, UsadosSinDuplicados),
    findall(N, (numero(N), \+ member(N, UsadosSinDuplicados)), Posibles).

% fila/3: extrae los dígitos fijos de la fila Row
fila(Sudoku, Row, DigitosFila) :-
    Start is Row * 9,
    End is Start + 8,
    findall(X, (between(Start, End, I), nth0(I, Sudoku, X), numero(X)), DigitosFila).

% columna/3: extrae los dígitos fijos de la columna Col
columna(Sudoku, Col, DigitosCol) :-
    findall(X, (between(0, 8, R), I is R * 9 + Col, nth0(I, Sudoku, X), numero(X)), DigitosCol).

% caja/4: extrae los dígitos fijos de la caja 3x3 que contiene la celda (Row, Col)
caja(Sudoku, Row, Col, DigitosCaja) :-
    BoxRow is (Row // 3) * 3,
    BoxCol is (Col // 3) * 3,
    findall(X,
       ( between(0, 2, ROffset),
         between(0, 2, COffset),
         R is BoxRow + ROffset,
         C is BoxCol + COffset,
         Index is R * 9 + C,
         nth0(Index, Sudoku, X),
         numero(X)
       ),
       DigitosCaja).

% ejemplo_sudoku/1: ejemplo de un Sudoku 9x9
ejemplo_sudoku([
   vacio, vacio, 9, 6, vacio,     vacio, vacio, 1, vacio,
   8, vacio, vacio, vacio, vacio,     1, vacio, 9, vacio,
   7, vacio, vacio, vacio, vacio, vacio, vacio, vacio, 8,
   vacio, 3, vacio, vacio, 6, vacio, vacio, vacio, vacio,
   vacio, 4, vacio, 1, vacio, 9, vacio, vacio,     5,
   9, vacio, vacio, vacio, vacio, vacio, vacio, vacio, vacio,
   vacio, 8, vacio, 9, vacio, vacio, 5, 4, vacio,
   6, vacio, vacio, 7, 1, vacio, vacio, vacio, 3,
   vacio, vacio, 5, vacio, 8, 4, vacio, vacio, 9
]).
