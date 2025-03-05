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

sudoku2([5, 3, ., ., 7, ., ., ., .,
        6, ., ., 1, 9, 5, ., ., .,
        1, 9, 8, ., ., ., ., 6, .,
        8, ., ., ., 6, ., ., ., 3,
        4, ., ., 8, ., 3, ., ., 1,
        7, ., ., ., 2, ., ., ., 6,
        ., 6, ., ., ., ., 2, 8, .,
        ., ., ., 4, 1, 9, ., ., 5,
        ., ., ., ., 8, ., ., 7, 9
]).

sudoku3([ ., ., 7, ., ., ., 8, ., .,
         ., 4, 5, 7, 6, ., ., ., 2,
         6, ., ., ., 4, ., 3, ., 5,
         8, 6, ., 5, ., ., ., 4, .,
         ., ., 3, 8, ., 4, ., 6, .,
         7, 2, 6, 9, ., ., 8, 3, .,
         ., 5, ., ., ., ., 4, 7, .,
         7, ., 4, ., ., ., ., ., 6,
         3, 4, ., ., 6, ., 6, 2, 8]).

sudoku4([ ., ., 6, ., ., 2, 3, ., 4,
         9, ., 4, 7, 5, ., ., 7, 2,
         ., ., 8, ., ., 6, ., ., 5,
         ., ., 3, ., ., ., ., 4, .,
         2, ., ., 4, ., ., 8, 3, .,
         4, ., 7, 5, ., ., ., ., .,
         ., ., ., 6, ., ., ., ., 8,
         7, ., ., ., 2, ., 4, 5, 3,
         ., ., ., 3, 7, ., ., 6, 9]).

sudoku5([
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
sudoku6([
    ., 9, ., ., 2, 4, ., 7, .,
    ., ., ., ., ., ., ., ., .,
    6, 4, 8, 3, ., 7, ., ., .,
    ., 7, ., ., 4, 5, 6, ., 3,
    ., 5, ., ., ., ., ., 2, 7,
    2, ., 6, ., ., ., ., ., .,
    3, ., ., ., 8, ., ., 4, .,
    ., 7, ., 4, 3, ., ., 5, 6,
    4, 2, 5, ., ., ., 8, 3, 9
]).
sudoku7([
    7, 9, ., ., 6, 4, ., ., 2,
    ., ., 3, 6, ., ., 7, ., .,
    ., 4, 8, 1, ., 7, ., ., .,
    ., 7, ., ., 4, 5, 6, 9, 3,
    2, ., ., ., ., ., ., ., 7,
    ., ., 6, ., ., ., ., 2, .,
    4, ., ., ., 8, ., ., 3, .,
    5, 7, ., 4, 3, ., ., ., 6,
    3, 2, 5, ., ., ., 8, 4, 9]).

sudoku8([
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


% Predicado para ajustar las posibilidades según el nuevo índice
ajuste_de_posibilidades(Posibilidades, PosibilidadesAjustadas) :-
    indice_filas(Indice_Filas),
    
    length(Posibilidades, Len),
    length(PosibilidadesAjustadas, Len),
    ajuste_de_posibilidades(Posibilidades, Indice_Filas, PosibilidadesAjustadas).

%caso base de ajuste de posibilidades
ajuste_de_posibilidades([], [], PosibilidadesAjustadas).
%caso recursivo de ajuste de posibilidades

ajuste_de_posibilidades([P|Ps], [NuevaPosicion|Indice_Filas], PosibilidadesAjustadas) :-
    nth1(NuevaPosicion, PosibilidadesAjustadas, P),
    ajuste_de_posibilidades(Ps, Indice_Filas, PosibilidadesAjustadas).





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
    writeln("Lista Filtrada: "), writeln(ListaFiltrada),
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
    
    % Mostrar la lista sin repetidos
    %writeln('Lista sin repetidos:'),
    writeln(ListaSinRepetidos),

    % Agregar los valores de ListaSinRepetidos a Def
    append(Def, ListaSinRepetidos, DefActualizado),

    % Imprimir la lista de acumulación
    %writeln('Def actualizada:'),
    writeln(DefActualizado),

    %CONTROLCHECKHASTAAQUI
    Nuevo_Indice is Indice + 1,
    % Continuar con las siguientes listas
    eliminar_repetidos_de_las_listas(Rest, Nuevo_Indice, Valores, DefActualizado, Lista_Definitiva).







% Función que actualiza las posibilidades de acuerdo a los valores únicos
actualizar_posibilidades_con_unicos(Posibilidades, 82,_,Posibilidades_Fin) :-
    % Recorrer todas las casillas del Sudoku (de 1 a 81)
    Posibilidades_Fin=Posibilidades.

% Caso recursivo: procesar una casilla y actualizarla si corresponde
actualizar_posibilidades_con_unicos(Posibilidades, Indice, Unicos, Posibilidades_Fin) :-
    % Extraer el valor de las posibles posibilidades en el índice actual
    nth1(Indice, Posibilidades, ListaPosibilidades),
    %writeln("ListaPosibilidades"), writeln(ListaPosibilidades),
    % Comprobar si hay un valor único en FilaUnicos, ColumnaUnicos o CuadradoUnicos
    (   nth1(Indice, Unicos, ValorUnicos),
        ValorUnicos = [X]  % Solo si tiene exactamente un elemento y no está vacía
        ->  Reemplazo = [X],
        % Reemplazar el valor en la lista de Posibilidades
        replace(Posibilidades, Indice, Reemplazo, NuevaPosibilidadesParcial),

        % Llamada recursiva para procesar el siguiente índice
        IndiceSiguiente is Indice + 1,
        actualizar_posibilidades_con_unicos(NuevaPosibilidadesParcial,IndiceSiguiente ,Unicos,Posibilidades_Fin)
    ;
        % Llamada recursiva para procesar el siguiente índice
        Reemplazo = ListaPosibilidades,
        replace(Posibilidades, Indice, Reemplazo, NuevaPosibilidadesParcial),
        IndiceSiguiente is Indice + 1,
        actualizar_posibilidades_con_unicos(NuevaPosibilidadesParcial,IndiceSiguiente ,Unicos,Posibilidades_Fin)
    ).








% Función principal para iniciar la comparación
verificar_repetidos(Filas, Columnas, Cuadrados, Valores, FilaDef, ColumnaDef, Cuadrados_Def) :-
    % Definir los índices
    Indice_Filas=[[1,2,3,10,11,12,19,20,21],[4,5,6,13,14,15,22,23,24],[7,8,9,16,17,18,25,26,27],[28,29,30,37,38,39,46,47,48],[31,32,33,40,41,42,49,50,51],[34,35,36,43,44,45,52,53,54],[55,56,57,64,65,66,73,74,75],[58,59,60,67,68,69,76,77,78],[61,62,63,70,71,72,79,80,81]],
    Indice_Columnas=[[1,4,7,28,31,34,55,58,61],[2,5,8,29,32,35,56,59,62],[3,6,9,30,33,36,57,60,63],[10,13,16,37,40,43,64,67,70],[11,14,17,38,41,44,65,68,71],[12,15,18,39,42,45,66,69,72],[19,22,25,46,49,52,73,76,79],[20,23,26,47,50,53,74,77,80],[21,24,27,48,51,54,75,78,81]],
    Indice_Cuadrado=[[1,2,3,4,5,6,7,8,9],[10,11,12,13,14,15,16,17,18],[19,20,21,22,23,24,25,26,27],[28,29,30,31,32,33,34,35,36],[37,38,39,40,41,42,43,44,45],[46,47,48,49,50,51,52,53,54],[55,56,57,58,59,60,61,62,63],[64,65,66,67,68,69,70,71,72],[73,74,75,76,77,78,79,80,81]],

    % Eliminar repetidos dentro de cada lista de filas, columnas y cuadrados

    eliminar_repetidos_de_las_listas(Indice_Filas,1, Valores, Filas, FilaDef),
    
    eliminar_repetidos_de_las_listas(Indice_Columnas, 1,Valores, Columnas, ColumnaDef),
    eliminar_repetidos_de_las_listas(Indice_Cuadrado, 1, Valores, Cuadrados, Cuadrados_Def).
    




% Caso base: cuando hemos procesado todas las casillas
aplicar_regla_1(Posibilidades, FilaUnicos, ColumnaUnicos, CuadradoUnicos,FilaDef, ColumnaDef, Cuadrados_Def, NuevaPosibilidades) :-
    verificar_repetidos(FilaUnicos, ColumnaUnicos, CuadradoUnicos, Posibilidades, FilaDef, ColumnaDef, Cuadrados_Def),

    Posibilidades_Originales=Posibilidades,

    actualizar_posibilidades_con_unicos(Posibilidades, 1,FilaDef, Temp1),
    
    (Posibilidades_Originales\==Temp1
    -> NuevaPosibilidades=Temp1
    ;
        Temp1_Original=Temp1,
        actualizar_posibilidades_con_unicos(Temp1, 1, ColumnaDef, Temp2),
        (Temp1_Original\==Temp2
        -> NuevaPosibilidades=Temp2
        ;
        actualizar_posibilidades_con_unicos(Temp2, 1,Cuadrados_Def, NuevaPosibilidades)
        )

    ).










% Predicado para aplicar la Regla 0 y actualizar el Sudoku
%FALTARIA UNIR CON REGLA 0
resolver_regla_1(Sudoku, NuevoSudoku, Posibilidades, Fin) :-
    ajuste_de_posibilidades(Posibilidades, PosibilidadesAjustadas),
    writeln("Posibilidades: "), writeln(Posibilidades),
    aplicar_regla_1(PosibilidadesAjustadas, FilaUnicos, ColumnaUnicos, CuadradoUnicos,FilaDef, ColumnaDef, Cuadrados_Def,NuevaPosibilidades),
    ajuste_de_posibilidades(NuevaPosibilidades, Fin),
    writeln(" Posibilidades Depuradas: "), writeln(Fin).
    
resolver_regla_0(Sudoku, NuevoSudoku) :-
    generar_posibilidades(Sudoku, Posibilidades),
    resolver_regla_1(Sudoku, NuevoSudoku, Posibilidades, Fin),
    aplicar_regla_0(Sudoku, Fin, NuevoSudoku).

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
    sudoku2(Tablero),
    resolver_regla_0(Tablero, NuevoTablero), 
    imprimir_sudoku(NuevoTablero),
    generar_posibilidades(NuevoTablero, PosibilidadesActualizadas),
    imprimir_posibilidades(PosibilidadesActualizadas).



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
aplicar_regla_2(Posibilidades, NuevaPosibilidades) :-
    % Aplicar la Regla 2 a filas
    aplicar_regla_2_a_todas_filas(Posibilidades, Temp1),
    % Aplicar la Regla 2 a columnas
    aplicar_regla_2_a_todas_columnas(Temp1, Temp2),
    % Aplicar la Regla 2 a cuadros
    aplicar_regla_2_a_todos_cuadros(Temp2, NuevaPosibilidades).

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

% Predicado para reemplazar un elemento en una lista en una posición dada
replace([_|T], 1, X, [X|T]).
replace([H|T], N, X, [H|R]) :- N > 1, N1 is N - 1, replace(T, N1, X, R).

% Predicado para probar la Regla 2
probar_regla_2 :-
    % Obtiene el tablero de Sudoku
    sudoku1(Tablero),
    % Genera las posibilidades para el tablero
    generar_posibilidades(Tablero, Posibilidades),
    % Aplica la Regla 2 a las posibilidades
    aplicar_regla_2(Posibilidades, NuevaPosibilidades),
    % Imprime las posibilidades después de aplicar la Regla 2
    writeln("Posibilidades despues de aplicar la Regla 2:"),
    imprimir_sudoku(NuevaPosibilidades).

% Predicado para aplicar las Reglas 0 y 2 de manera iterativa
resolver_reglas_0_y_2(Sudoku, NuevoSudoku) :-
    % Genera las posibilidades para el Sudoku
    generar_posibilidades(Sudoku, Posibilidades),
    % Aplica la Regla 2 a las posibilidades
    aplicar_regla_2(Posibilidades, PosibilidadesRegla2),
    writeln("Posibilidades despues de aplicar la Regla 2:"),
    imprimir_sudoku(PosibilidadesRegla2),
    % Aplica la Regla 0 al Sudoku con las nuevas posibilidades
    aplicar_regla_0(Sudoku, PosibilidadesRegla2, NuevoSudoku).

% Predicado para iterar las Reglas 0 y 2 hasta que no haya mas cambios
iterar_reglas_0_y_2(Sudoku, NuevoSudoku) :-
    % Aplica las Reglas 0 y 2 al Sudoku
    resolver_reglas_0_y_2(Sudoku, SudokuIntermedio),
    % Si hubo cambios en el Sudoku, continúa iterando
    (   Sudoku \= SudokuIntermedio ->
        iterar_reglas_0_y_2(SudokuIntermedio, NuevoSudoku)
    ;   NuevoSudoku = Sudoku  % Si no hubo cambios, el Sudoku está actualizado
    ).

% Predicado para probar las Reglas 0 y 2
probar_reglas_0_y_2 :-
    % Obtiene el tablero de Sudoku
    sudoku1(Tablero),
    imprimir_sudoku(Tablero),
    % Itera las Reglas 0 y 2 hasta que no haya mas cambios
    iterar_reglas_0_y_2(Tablero, NuevoTablero),
    % Imprime el Sudoku después de aplicar las Reglas 0 y 2
    writeln("Sudoku despues de aplicar las Reglas 0 y 2:"),
    imprimir_sudoku(NuevoTablero),
    generar_posibilidades(NuevoTablero, Posibilidades),
    imprimir_sudoku(Posibilidades).


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

    % Predicado para aplicar la Regla 3
    aplicar_regla_3(Posibilidades, NuevaPosibilidades) :-
        % Aplicar la Regla 3 a filas
        aplicar_regla_3_a_todas_filas(Posibilidades, Temp1),
        % Aplicar la Regla 3 a columnas
        aplicar_regla_3_a_todas_columnas(Temp1, Temp2),
        % Aplicar la Regla 3 a cuadros
        aplicar_regla_3_a_todos_cuadros(Temp2, NuevaPosibilidades).

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

    % Predicado para probar la Regla 3
    probar_regla_3 :-
        % Obtiene el tablero de Sudoku
        sudoku8(Tablero),
        % Genera las posibilidades para el tablero
        generar_posibilidades(Tablero, Posibilidades),
        imprimir_sudoku(Posibilidades),
        % Aplica la Regla 3 a las posibilidades
        aplicar_regla_3(Posibilidades, NuevaPosibilidades),
        % Imprime las posibilidades después de aplicar la Regla 3
        writeln("Posibilidades despues de aplicar la Regla 3:"),
        imprimir_sudoku(NuevaPosibilidades).

