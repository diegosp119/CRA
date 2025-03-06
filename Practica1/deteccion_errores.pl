sudoku2([
    ., 8, ., 5, 7, 6, 2, ., 8,
    ., ., ., 4, ., 2, ., ., .,
    ., ., ., ., 3, 9, 5, 4, 8,
    6, 3, ., 9, ., ., 8, 5, 2,
    ., 9, ., 2, ., ., 3, 7, .,
    8, ., ., ., 5, ., 6, 9, 4,
    2, 5, 7, 6, ., 3, 4, 8, 9,
    3, ., 8, 7, ., ., ., 2, 5,
    ., 4, ., ., ., 6, ., ., 6
]).

lista_correcta(Lista) :-
    exclude(==(.), Lista, Numeros),  % Filtrar los valores no numéricos
    sort(Numeros, NumerosOrdenados),
    length(Numeros, L),
    length(NumerosOrdenados, L).  % Verificar que no hay repetidos

extraer_filas([], []).
extraer_filas(Sudoku, [Fila | Resto]) :-
    length(Fila, 9),
    append(Fila, RestoSudoku, Sudoku),
    extraer_filas(RestoSudoku, Resto).

extraer_columnas(Sudoku, Columnas) :-
    findall(Columna, (between(0, 8, N),
                      findall(Elem, (between(0, 8, M),
                                     Index is M * 9 + N,
                                     nth0(Index, Sudoku, Elem)), Columna)), Columnas).

extraer_cuadrantes(Sudoku, Cuadrantes) :-
    findall(Cuadrante, (
        member(R, [0, 3, 6]), member(C, [0, 3, 6]),
        findall(Elem, (
            between(0, 2, DR), between(0, 2, DC),
            Index is (R + DR) * 9 + (C + DC),
            nth0(Index, Sudoku, Elem)
        ), Cuadrante)
    ), Cuadrantes).

verificar_secciones(Extraer, Nombre) :-
    sudoku2(Sudoku),
    call(Extraer, Sudoku, Secciones),
    verificar_secciones_aux(Secciones, 1, [], Nombre).

verificar_secciones_aux([], _, [], _) :- writeln('Bien').
verificar_secciones_aux([], _, Errores, Nombre) :-
    format('Hay error(es) en la(s) ~w(s): ~w~n', [Nombre, Errores]).
verificar_secciones_aux([Seccion | Resto], N, Errores, Nombre) :-
    (   lista_correcta(Seccion)
    ->  N1 is N + 1,
        verificar_secciones_aux(Resto, N1, Errores, Nombre)
    ;   N1 is N + 1,
        append(Errores, [N], NuevosErrores),
        verificar_secciones_aux(Resto, N1, NuevosErrores, Nombre)).

verificar_sudoku :-
    verificar_secciones(extraer_filas, 'fila'),
    verificar_secciones(extraer_columnas, 'columna'),
    verificar_secciones(extraer_cuadrantes, 'cuadrante').
