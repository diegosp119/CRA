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

sudoku2([
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
         9, ., 4, 7, 5, ., ., 8, 2,
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


sudoku([5, 3, ., ., 7, ., ., ., .,
        6, ., ., 1, 9, 5, ., ., .,
        1, 9, 8, ., ., ., ., 6, .,
        8, ., ., ., 6, ., ., ., 3,
        4, ., ., 8, ., 3, ., ., 1,
        7, ., ., ., 2, ., ., ., 6,
        ., 6, ., ., ., ., 2, 8, .,
        ., ., ., 4, 1, 9, ., ., 5,
        ., ., ., ., 8, ., ., 7, 9]).


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

% Predicado para imprimir el tablero de lista de posibilidades
imprimir_cuadro_pos([]).
imprimir_cuadro_pos(Tablero) :-
    imprimir_fila(Tablero, Resto),
    imprimir_cuadro_pos(Resto).


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
    sudoku3(Tablero),
    generar_posibilidades(Tablero,Posibilidades),
    %writeln(Posibilidades),
    imprimir_posibilidades(Posibilidades).


%Predicado para probar la Regla 0
probar_regla_0 :-
    sudoku(Tablero),
    generar_posibilidades(Tablero, Posibilidades),
    imprimir_cuadro_pos(Posibilidades),
    resolver_regla_0(Tablero, NuevoTablero),
    imprimir_sudoku(NuevoTablero),
    generar_posibilidades(NuevoTablero, PosibilidadesActualizadas),
    imprimir_cuadro_pos(PosibilidadesActualizadas).

%----------------------REGLA 1------------------------------

indice_filas([1,2,3,10,11,12,19,20,21,4,5,6,13,14,15,22,23,24,7,8,9,16,17,18,25,26,27,28,29,30,37,38,39,46,47,48,31,32,33,40,41,42,49,50,51,34,35,36,43,44,45,52,53,54,55,56,57,64,65,66,73,74,75,58,59,60,67,68,69,76,77,78,61,62,63,70,71,72,79,80,81]).

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
    %writeln("FINNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNN").
eliminar_repetidos_de_las_listas([Lista|Rest], Indice, Valores, Def, Lista_Definitiva) :-
    % Acceder a los valores de la lista a través de los índices
    writeln("Lista: "), writeln(Lista),
    writeln("Valores: "), writeln(Valores),
    obtener_valores_por_indices(Lista, Valores, Valores_Extraidos),
    writeln("ValoresExtraidos: "), writeln(Valores_Extraidos),

    %HASTAAQUIVABIEN
    % Eliminar los repetidos en esa lista
    %Esta asignacion directa se borrará luego de momento es para ver como consigo contar los repetidos
    %Valores_Extraidos = [[1,4,9],[],[1,3,4,9],[5,9],[6],[3,5,9],[],[2,6],[]],
    eliminar_repetidos(Valores_Extraidos, ListaSinRepetidos),
    
    % Mostrar la lista sin repetidos
    writeln('Lista sin repetidos:'),
    writeln(ListaSinRepetidos),

    % Agregar los valores de ListaSinRepetidos a Def
    append(Def, ListaSinRepetidos, DefActualizado),

    % Imprimir la lista de acumulación
    writeln('Def actualizada:'),
    writeln(DefActualizado),

    %CONTROLCHECKHASTAAQUI
    Nuevo_Indice is Indice + 1,
    % Continuar con las siguientes listas
    eliminar_repetidos_de_las_listas(Rest, Nuevo_Indice, Valores, DefActualizado, Lista_Definitiva).

% Función que actualiza las posibilidades de acuerdo a los valores únicos
actualizar_posibilidades_con_unicos(Posibilidades, 82,_,_,_, NuevaPosibilidades) :-
    % Recorrer todas las casillas del Sudoku (de 1 a 81)
    writeln("Llego al fin del sudoku"),
    NuevaPosibilidades = Posibilidades,
    writeln("NuevaPosibilidades: "), writeln(NuevaPosibilidades).

% Caso recursivo: procesar una casilla y actualizarla si corresponde
actualizar_posibilidades_con_unicos(Posibilidades, Indice, FilaUnicos, ColumnaUnicos, CuadradoUnicos, NuevaPosibilidades) :-
    % Extraer el valor de las posibles posibilidades en el índice actual
    nth1(Indice, Posibilidades, ListaPosibilidades),
    
    % Comprobar si hay un valor único en FilaUnicos, ColumnaUnicos o CuadradoUnicos
    (   nth1(Indice, FilaUnicos, ValorUnicoFila),
        ValorUnicoFila = [X]  % Solo si tiene exactamente un elemento y no está vacía
        ->  Reemplazo = [X]
    ;   nth1(Indice, ColumnaUnicos, ValorUnicoColumna),
        ValorUnicoColumna = [X]  % Solo si tiene exactamente un elemento y no está vacía
        ->  Reemplazo = [X]
    ;   nth1(Indice, CuadradoUnicos, ValorUnicoCuadrado),
        ValorUnicoCuadrado = [X]  % Solo si tiene exactamente un elemento y no está vacía
        ->  Reemplazo = [X]
    ;   % Si no hay valores únicos en fila, columna o cuadrado, mantenemos las posibilidades
        Reemplazo = ListaPosibilidades
    ),

    % Reemplazar el valor en la lista de Posibilidades
    replace(Posibilidades, Indice, Reemplazo, NuevaPosibilidadesParcial),

    % Llamada recursiva para procesar el siguiente índice
    IndiceSiguiente is Indice + 1,
    actualizar_posibilidades_con_unicos(NuevaPosibilidadesParcial,IndiceSiguiente ,FilaUnicos, ColumnaUnicos, CuadradoUnicos, NuevaPosibilidades).


% Función principal para iniciar la comparación
verificar_repetidos(Filas, Columnas, Cuadrados, Valores, FilaDef, ColumnaDef, Cuadrados_Def) :-
    % Definir los índices
    Indice_Filas=[[1,2,3,10,11,12,19,20,21],[4,5,6,13,14,15,22,23,24],[7,8,9,16,17,18,25,26,27],[28,29,30,37,38,39,46,47,48],[31,32,33,40,41,42,49,50,51],[34,35,36,43,44,45,52,53,54],[55,56,57,64,65,66,73,74,75],[58,59,60,67,68,69,76,77,78],[61,62,63,70,71,72,79,80,81]],
    Indice_Columnas=[[1,4,7,28,31,34,55,58,61],[2,5,8,29,32,35,56,59,62],[3,6,9,30,33,36,57,60,63],[10,13,16,37,40,43,64,67,70],[11,14,17,38,41,44,65,68,71],[12,15,18,39,42,45,66,69,72],[19,22,25,46,49,52,73,76,79],[20,23,26,47,50,53,74,77,80],[21,24,27,48,51,54,75,78,81]],
    Indice_Cuadrado=[[1,2,3,4,5,6,7,8,9],[10,11,12,13,14,15,16,17,18],[19,20,21,22,23,24,25,26,27],[28,29,30,31,32,33,34,35,36],[37,38,39,40,41,42,43,44,45],[46,47,48,49,50,51,52,53,54],[55,56,57,58,59,60,61,62,63],[64,65,66,67,68,69,70,71,72],[73,74,75,76,77,78,79,80,81]],

    % Eliminar repetidos dentro de cada lista de filas, columnas y cuadrados
    writeln("Fila: "), writeln(Fila),
    eliminar_repetidos_de_las_listas(Indice_Filas,1, Valores, Filas, FilaDef),
    writeln("Columna: "), writeln(Columna),
    eliminar_repetidos_de_las_listas(Indice_Columnas, 1,Valores, Columnas, ColumnaDef),
    writeln("COLUMNAS: "), writeln(Columnas),
    writeln("Cuadrado: "), writeln(Cuadrado),
    eliminar_repetidos_de_las_listas(Indice_Cuadrado, 1, Valores, Cuadrados, Cuadrados_Def),
    writeln("Cuadrados Def: "), writeln(Cuadrados_Def).



%HAsta aquí es lo que queda por hacer

%ESTO NO LO USO
%actualizar_posibilidades_pareja(Posibilidades, Indice, VecinosFila, UnicoFila, NuevaPosibilidades) :-
    % Extraer los valores únicos
    %[ValorIndice, ValorVecino] = UnicoFila,

    % Verificar que los valores no sean vacíos antes de reemplazar
    %(ValorIndice \= [] -> replace(Posibilidades, Indice, ValorIndice, TempPos1) ; TempPos1 = Posibilidades),
    %(ValorVecino \= [] -> replace(TempPos1, VecinosFila, ValorVecino, NuevaPosibilidades) ; NuevaPosibilidades = TempPos1).

% Caso base: cuando hemos procesado todas las casillas
aplicar_regla_1(Posibilidades, FilaUnicos, ColumnaUnicos, CuadradoUnicos,FilaDef, ColumnaDef, Cuadrados_Def, NuevaPosibilidades) :-
    writeln("Regla 1 aplicada con exito"),
    writeln("ColumnaUnicos: "), writeln(ColumnaUnicos),
    verificar_repetidos(FilaUnicos, ColumnaUnicos, CuadradoUnicos, Posibilidades, FilaDef, ColumnaDef, Cuadrados_Def),
    writeln(FilaDef),
    writeln(ColumnaDef),
    writeln(Cuadrados_Def),

    actualizar_posibilidades_con_unicos(Posibilidades, 1,FilaDef, ColumnaDef, Cuadrados_Def, NuevaPosibilidades),
    %writeln("Posibilidades: "), writeln(Posibilidades),
    %writeln("Nuevas Posibilidades"), writeln(NuevaPosibilidades),
    writeln("Posibilidades, Sudoku: "),imprimir_sudoku(Posibilidades),
    writeln("Nuevas Posibilidades, Sudoku: "), imprimir_sudoku(NuevaPosibilidades).


% Predicado para aplicar la Regla 0 y actualizar el Sudoku
%FALTARIA UNIR CON REGLA 0
resolver_regla_1(Sudoku, NuevoSudoku, Posibilidades, Fin) :-
    %generar_posibilidades(Sudoku, Posibilidades),
    ajuste_de_posibilidades(Posibilidades, PosibilidadesAjustadas),
    writeln("Posibilidades: "), writeln(Posibilidades),
    aplicar_regla_1(PosibilidadesAjustadas, FilaUnicos, ColumnaUnicos, CuadradoUnicos,FilaDef, ColumnaDef, Cuadrados_Def, NuevaPosibilidades),
    ajuste_de_posibilidades(NuevaPosibilidades, Fin),
    writeln(" Posibilidades Depuradas: "), writeln(Fin).

iterar_reglas(Sudoku, NuevoSudoku) :-
    generar_posibilidades(Sudoku, Posibilidades),
    %LLAMADAREGLA1
    resolver_regla_1(Sudoku, NuevoSudoku, Posibilidades, Fin),
    aplicar_regla_0_1(Sudoku, Fin, NuevoSudoku).

% Predicado para aplicar la Regla 0 una vez
aplicar_regla_0_1(Sudoku, Posibilidades, NuevoSudoku) :-
    actualizar_sudoku(Sudoku, Posibilidades, SudokuActualizado),
    (   Sudoku \= SudokuActualizado -> 
        iterar_reglas(SudokuActualizado, NuevoSudoku)  % Continuar aplicando la Regla 0 si hubo cambios
    ;   NuevoSudoku = Sudoku  % Si no hubo cambios, el Sudoku está actualizado
    ).

%Predicado para probar la Regla 1
probar_regla_1 :-
    sudoku(Tablero),
    iterar_reglas(Tablero, NuevoTablero), 
    imprimir_sudoku(NuevoTablero),
    generar_posibilidades(NuevoTablero, PosibilidadesActualizadas),
    imprimir_sudoku(PosibilidadesActualizadas).

%----------------------REGLA 2------------------------------
