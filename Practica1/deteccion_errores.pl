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

lista_correcta(Lista) :-
    exclude(==(.), Lista, Numeros),  % Filtrar los valores no numéricos
    sort(Numeros, NumerosOrdenados),
    length(Numeros, L),
    length(NumerosOrdenados, L).  % Verificar que no hay repetidos

extraer_filas([], []).
extraer_filas(Sudoku, [Fila | Resto]) :-
    length(Fila, 9),
    append(Fila, RestoSudoku, Sudoku), %Fila: toma los primeros 9 elementos de Sudoku y RestoSudoku: contiene los elementos restantes (los siguientes 72, luego 63, 54, etc.).
    extraer_filas(RestoSudoku, Resto). %Llamada recursiva con la parte restante (RestoSudoku).

extraer_columnas(Sudoku, Columnas) :-
    findall(Columna, (between(0, 8, N), %N toma valores de 0 a 8, representando los índices de las 9 columnas
                      findall(Elem, (between(0, 8, M), %between(0, 8, M): Itera sobre las 9 filas
                                     Index is M * 9 + N, %Index is M * 9 + N: Calcula el índice del elemento en la lista unidimensional
                                     nth0(Index, Sudoku, Elem)), Columna)), Columnas). %findall almacena todas las listas generadas en Columnas, que será la lista de columnas del Sudoku

extraer_cuadrantes(Sudoku, Cuadrantes) :-
    findall(Cuadrante, ( %findall recolecta todas las listas generadas en Cuadrantes, que será la lista de los 9 cuadrantes
        member(R, [0, 3, 6]), member(C, [0, 3, 6]), %R y C representan las coordenadas de inicio de cada cuadrante 3x3
        findall(Elem, (                            %Se toman los valores [0, 3, 6], que son las filas y columnas donde comienzan los 9 cuadrantes
            between(0, 2, DR), between(0, 2, DC), %between(0, 2, DR): Itera sobre las 3 filas dentro del cuadrante y between(0, 2, DC): Itera sobre las 3 columnas dentro del cuadrante
            Index is (R + DR) * 9 + (C + DC),  %Index is (R + DR) * 9 + (C + DC): Calcula la posición del elemento en la lista unidimensional
            nth0(Index, Sudoku, Elem) %nth0(Index, Sudoku, Elem): Extrae el elemento de la lista en la posición Index
        ), Cuadrante)
    ), Cuadrantes).

verificar_secciones(Extraer, Nombre) :- %Extraer: Un predicado que extrae las secciones del Sudoku (puede ser extraer_filas/2, extraer_columnas/2 o extraer_cuadrantes/2)
    sudoku2(Sudoku),
    call(Extraer, Sudoku, Secciones), %Llama dinámicamente al predicado de extracción (call(Extraer, Sudoku, Secciones)) para obtener todas las filas, columnas o cuadrantes
    verificar_secciones_aux(Secciones, 1, [], Nombre). %Nombre: Un string que indica el tipo de sección a verificar ('fila', 'columna' o 'cuadrante')
    %Secciones: Lista de las secciones extraídas
    %1: Número inicial de sección (para identificación en caso de error)
    %[]: Lista vacía de errores (se irá llenando si hay problemas)
    %Nombre: Tipo de sección (se usa para imprimir mensajes)

verificar_secciones_aux([], _, [], _) :- writeln('Bien'). %Condición: Se han procesado todas las secciones ([]) y no se encontraron errores ([])

verificar_secciones_aux([], _, Errores, Nombre) :- %Se imprime un mensaje con las secciones incorrectas:
    format('Hay error(es) en la(s) ~w(s): ~w~n', [Nombre, Errores]). %~w representa un valor en la cadena de texto y [Nombre, Errores] inserta el tipo de sección (fila, columna o cuadrante) y la lista de errores

verificar_secciones_aux([Seccion | Resto], N, Errores, Nombre) :-  
%Condición: Aún quedan secciones por verificar ([Seccion | Resto])
%Acción: Se extrae la primera sección (Seccion) y se continúa con el resto (Resto)
%N indica el número de la sección actual, y Errores acumula los errores encontrados
    (   lista_correcta(Seccion) %Si la seccion es correcta
    ->  N1 is N + 1, %Se incrementa el contador de la sección: N1 is N + 1
        verificar_secciones_aux(Resto, N1, Errores, Nombre) %se llama recursivamente a verificar_secciones_aux/4 con el resto de secciones
    ;   N1 is N + 1, %Si no es correcta, se incrementa el contador de seccion
        append(Errores, [N], NuevosErrores), %Se agrega el número de la sección actual (N) a la lista de errores
        verificar_secciones_aux(Resto, N1, NuevosErrores, Nombre)). %Se llama recursivamente a verificar_secciones_aux/4 con la nueva lista de errores

verificar_sudoku :-
    verificar_secciones(extraer_filas, 'fila'),
    verificar_secciones(extraer_columnas, 'columna'),
    verificar_secciones(extraer_cuadrantes, 'cuadrante').