sudoku2([
    1, 2, 3, 4, 5, 6, 7, 8, 9,  
    4, 5, 6, 7, 8, 9, 1, 2, 3,  
    7, 8, 9, 1, 2, 3, 4, 5, 6,  
    2, 3, 4, 5, 6, 7, 8, 9, 1,  
    5, 6, 7, 8, 9, 1, 2, 3, 4,  
    8, 9, 1, 2, 3, 4, 5, 6, 7,  
    3, 4, 5, 6, 7, 8, 9, 1, 2,  
    6, 7, 8, 9, 1, 2, 3, 4, 5,  
    9, 1, 2, 3, 4, 5, 6, 7, 8
]).

% Predicado para obtener los índices de la fila en la que está una casilla
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

%-------------------------------Código nuevo----------------------------------------------

lista_correcta(Lista) :-
    exclude(==(.), Lista, Numeros),  % Filtrar los valores no numéricos
    sort(Numeros, NumerosOrdenados), %ordena los valores númericos eliminando repetidos
    length(Numeros, L), % Obtiene la longitud de la lista original sin puntos
    length(NumerosOrdenados, L).  %Obtiene la longitud de la lista ordenada que debe ser igual que la anterior para sacar true

% Predicado para verificar las filas del Sudoku
verificar_filas :- 
    sudoku2(Sudoku),
    forall(between(0, 80, Index), (
        indices_fila(Index, FilaIndices), % Obtener los índices de la fila en la que está el índice actual
        findall(Element, (member(I, FilaIndices), nth0(I, Sudoku, Element)), Fila), % Extraer los elementos de la fila
        (lista_correcta(Fila) -> 
            true;
            format('Error en la fila con índice ~w: ~w~n', [Index, Fila])) % Mostrar error si la fila no es correcta
    )).

% Predicado para verificar las columnas del Sudoku
verificar_columnas :- 
    sudoku2(Sudoku),
    forall(between(0, 8, Columna), ( 
        indices_columna(Columna, ColumnaIndices), % Obtener los índices de la columna
        findall(Element, (member(I, ColumnaIndices), nth0(I, Sudoku, Element)), Columna), % Extraer los elementos de la columna
        (lista_correcta(Columna) -> 
            true;
            format('Error en la columna con índice ~w: ~w~n', [Columna, Columna])) % Mostrar error si la columna no es correcta
    )).

% Predicado para verificar los cuadrantes del Sudoku
verificar_cuadrantes :- 
    sudoku2(Sudoku),
    forall(between(0, 8, Cuadro), ( 
        indices_cuadro(Cuadro, CuadroIndices), % Obtener los índices del cuadrante
        findall(Element, (member(I, CuadroIndices), nth0(I, Sudoku, Element)), Cuadrante), % Extraer los elementos del cuadrante
        (lista_correcta(Cuadrante) -> 
            true;
            format('Error en el cuadrante con índice ~w: ~w~n', [Cuadro, Cuadrante])) % Mostrar error si el cuadrante no es correcto
    )).


% Predicado principal para verificar todo el Sudoku
verificar_sudoku :-
    verificar_filas,
    verificar_columnas,
    verificar_cuadrantes,
    writeln('El Sudoku es válido.').
