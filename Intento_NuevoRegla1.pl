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








%Cosas de la Regla 1


%Desde Aqui es lo que queda hacer

% Obtener los valores correspondientes a los índices dados en la lista de valores
obtener_valores_por_indices([], _, _).

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
eliminar_repetidos_de_las_listas([],_, _).
eliminar_repetidos_de_las_listas([Lista|Rest], Indice, Valores) :-
    % Acceder a los valores de la lista a través de los índices
    writeln("Lista: "), writeln(Lista),
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
    %CONTROLCHECKHASTAAQUI
    Nuevo_Indice is Indice + 1,
    % Continuar con las siguientes listas
    eliminar_repetidos_de_las_listas(Rest, Nuevo_Indice, Valores).





% Reemplazar un elemento en una lista en una posición dada
replace([_|T], 1, X, [X|T]).
replace([H|T], N, X, [H|R]) :- N > 1, N1 is N - 1, replace(T, N1, X, R).


% Función que actualiza las posibilidades de acuerdo a los valores únicos
actualizar_posibilidades_con_unicos(Posibilidades, 82,_,_,_, NuevaPosibilidades) :-
    % Recorrer todas las casillas del Sudoku (de 1 a 81)
    writeln("Llego al fin del sudoku"),
    NuevaPosibilidades = Posibilidades.

% Caso recursivo: procesar una casilla y actualizarla si corresponde
actualizar_posibilidades_con_unicos(Posibilidades, Indice, FilaUnicos, ColumnaUnicos, CuadradoUnicos, NuevaPosibilidades) :-
    % Extraer el valor de las posibles posibilidades en el índice actual
    nth1(Indice, Posibilidades, ListaPosibilidades),
    
    % Comprobar si hay un valor único en FilaUnicos, ColumnaUnicos o CuadradoUnicos
    (   nth1(Indice, FilaUnicos, ValorUnicoFila),
        ValorUnicoFila \= [] % Verifica que no esté vacío
    ->  % Si hay un valor único en la fila, lo actualizamos en las posibilidades
        Reemplazo = ValorUnicoFila
    ;   nth1(Indice, ColumnaUnicos, ValorUnicoColumna),
        ValorUnicoColumna \= []
    ->  % Si hay un valor único en la columna, lo actualizamos en las posibilidades
        Reemplazo = ValorUnicoColumna
    ;   nth1(Indice, CuadradoUnicos, ValorUnicoCuadrado),
        ValorUnicoCuadrado \= []
    ->  % Si hay un valor único en el cuadrado, lo actualizamos en las posibilidades
        Reemplazo = ValorUnicoCuadrado
    ;   % Si no hay valores únicos en fila, columna o cuadrado, mantenemos las posibilidades
        Reemplazo = ListaPosibilidades
    ),

    % Reemplazar el valor en la lista de Posibilidades
    replace(Posibilidades, Indice, Reemplazo, NuevaPosibilidadesParcial),

    % Llamada recursiva para procesar el siguiente índice
    IndiceSiguiente is Indice + 1,
    actualizar_posibilidades_con_unicos(NuevaPosibilidadesParcial,IndiceSiguiente ,FilaUnicos, ColumnaUnicos, CuadradoUnicos, NuevaPosibilidades).










% Función principal para iniciar la comparación
verificar_repetidos(Filas, Columnas, Cuadrados) :-
    % Definir los índices
    Indice_Filas=[[1,2,3,10,11,12,19,20,21],[4,5,6,13,14,15,22,23,24],[7,8,9,16,17,18,25,26,27],[28,29,30,37,38,39,46,47,48],[31,32,33,40,41,42,49,50,51],[34,35,36,43,44,45,52,53,54],[55,56,57,64,65,66,73,74,75],[58,59,60,67,68,69,76,77,78],[61,62,63,70,71,72,79,80,81]],
    Indice_Columnas=[[1,4,7,28,31,34,55,58,61],[2,5,8,29,32,35,56,59,62],[3,6,9,30,33,36,57,60,63],[10,13,16,37,40,43,64,67,70],[11,14,17,38,41,44,65,68,71],[12,15,18,39,42,45,66,69,72],[19,22,25,46,49,52,73,76,79],[20,23,26,47,50,53,74,77,80],[21,24,27,48,51,54,75,78,81]],
    Indice_Cuadrado=[[1,2,3,4,5,6,7,8,9],[10,11,12,13,14,15,16,17,18],[19,20,21,22,23,24,25,26,27],[28,29,30,31,32,33,34,35,36],[37,38,39,40,41,42,43,44,45],[46,47,48,49,50,51,52,53,54],[55,56,57,58,59,60,61,62,63],[64,65,66,67,68,69,70,71,72],[73,74,75,76,77,78,79,80,81]],

    % Eliminar repetidos dentro de cada lista de filas, columnas y cuadrados
    writeln("Fila: "), writeln(Fila),
    eliminar_repetidos_de_las_listas(Indice_Filas,1, Filas),
    writeln("Columna: "), writeln(Columna),
    eliminar_repetidos_de_las_listas(Indice_Columnas, 1,Columnas),
    writeln("Cuadrado: "), writeln(Cuadrado),
    eliminar_repetidos_de_las_listas(Indice_Cuadrado, 1, Cuadrados).



%HAsta aquí es lo que queda por hacer


actualizar_posibilidades_pareja(Posibilidades, Indice, VecinosFila, UnicoFila, NuevaPosibilidades) :-
    % Extraer los valores únicos
    [ValorIndice, ValorVecino] = UnicoFila,

    % Verificar que los valores no sean vacíos antes de reemplazar
    (ValorIndice \= [] -> replace(Posibilidades, Indice, ValorIndice, TempPos1) ; TempPos1 = Posibilidades),
    (ValorVecino \= [] -> replace(TempPos1, VecinosFila, ValorVecino, NuevaPosibilidades) ; NuevaPosibilidades = TempPos1).




% Caso base: cuando hemos procesado todas las casillas
aplicar_regla_1(_, 82, Posibilidades, FilaUnicos, ColumnaUnicos, CuadradoUnicos) :-
    writeln("Regla 1 aplicada con exito"),
    verificar_repetidos(FilaUnicos, ColumnaUnicos, CuadradoUnicos),
    actualizar_posibilidades_con_unicos(Posibilidades, 1,FilaUnicos, ColumnaUnicos, CuadradoUnicos, NuevaPosibilidades),
    %writeln("Posibilidades: "), writeln(Posibilidades),
    %writeln("Nuevas Posibilidades"), writeln(NuevaPosibilidades),
    writeln("Posibilidades, Sudoku: "),imprimir_sudoku(Posibilidades),
    writeln("Nuevas Posibilidades, Sudoku: "), imprimir_sudoku(NuevaPosibilidades).


% Caso recursivo: procesar cada casilla del Sudoku
aplicar_regla_1(Sudoku, Indice_Regla1, Posibilidades, FilaUnicos, ColumnaUnicos, CuadradoUnicos) :-
    % Extrae de la lista Posibilidades el elemento en la posición Indice_Regla1
    % ylo asigna a PosiblesValores
    
    
    %writeln("Entrando en aplicar_regla_1 con índice: "), writeln(Indice_Regla1),
    nth1(Indice_Regla1, Posibilidades, PosiblesValores),
    

    
    %Determinar vecinos de la casilla (fila, columna y cuadrado), es decir el Indice siguiente es lo que saco con esto
    %FILA
    Indice_Sig_Fila is Indice_Regla1+1,
    (Indice_Regla1 mod 3 =:= 0 ->
        %implica que se tiene que hacer un salto de 7 números
        Nuevo_Indice_Fila is Indice_Regla1 + 7
    ;
        %Si noes el caso nose hace nada
        Nuevo_Indice_Fila = Indice_Sig_Fila
    ),
    %comprobamos ahora que nose haya salido de los limites
    %primero que nosupere 81 la suma
    (Nuevo_Indice_Fila > 81 ->
        %Entonces se sale del espacio del sudokuy signfica que estamos en el ultimo elemento de la columna
        Indice_Intermedio_Fila is Indice_Regla1 -1
    ;
        %No se sale del espacio del sudoku, nohace falta hacer nada
        Indice_Intermedio_Fila = Nuevo_Indice_Fila
    ),
    %segundo que nocambie fila
    Id_Fila_Sig is ((Indice_Intermedio_Fila//27) * 3 )+ ((Indice_Intermedio_Fila mod 9)//3),
    Id_fila is ((Indice_Regla1//27) * 3 )+ ((Indice_Regla1 mod 9)//3),
    (Id_Fila_Sig =\= Id_fila ->
        % Si son distintos entonces estabamos en el limite del sudoku
        Indice_Final_Fila is Indice_Regla1 - 1
    ;
        %No se hace nada
        Indice_Final_Fila = Indice_Intermedio_Fila
    ),
    VecinosFila = Indice_Final_Fila,


    %COLUMNA
    Indice_Sig_Col is Indice_Regla1+3,
    Id_Col_Sig is  (((Indice_Sig_Col//9) mod 3) * 3 )+ (Indice_Sig_Col mod 3),
    Id_Col is (((Indice_Regla1//9) mod 3) * 3 )+ (Indice_Regla1 mod 3),
    
    % Comparar los índices de columna y ejecutar acciones
    (Id_Col_Sig =\= Id_Col ->  
        % Si son distintos, hacer una acción
        %writeln('Los índices de columna son distintos'),  
        Nuevo_Indice_Col is (9*2)+Indice_Regla1+3
    ;  
        % Si son iguales, nohace falta hacer nada
        %writeln('Los índices de columna son iguales'),
        Nuevo_Indice_Col=Indice_Sig_Col
    ),
    %Segunda comprobacion necesaria
    (Nuevo_Indice_Col>81->
            %Entonces se sale del espacio del sudokuy signfica que estamos en el ultimo elemento de la columna
            Indice_Final_Col is Indice_Regla1-3-(9*2)
        ;
            %No se sale del espacio del sudoku, nohace falta hacer nada
            Indice_Final_Col = Nuevo_Indice_Col
    ),
    VecinosColumna = Indice_Final_Col,

    
    %CUADRADO
    Indice_Sig_Cuadrado is Indice_Regla1+1,
    (Indice_Regla1 mod 9 =:= 0 ->
        %Estoy en el final del cuadrado
        Indice_Final_Cuadrado is Indice_Regla1-1
    ;
        %No pasa nada
        Indice_Final_Cuadrado = Indice_Sig_Cuadrado

    ),
    VecinosCuadrado = Indice_Final_Cuadrado,
    
    % Extraer las posibilidades de los vecinos
    nth1(VecinosFila, Posibilidades, PosibilidadesVecinosFila),
    %writeln("Posibles Vecinos Fila: "), writeln(PosibilidadesVecinosFila),
    nth1(VecinosColumna, Posibilidades, PosibilidadesVecinosColumna),
    nth1(VecinosCuadrado, Posibilidades, PosibilidadesVecinosCuadrado),
    %nth1(Indice_Regla1, Posibilidades, PosibilidadesPosActual),
    
    % Aquí debes implementar la lógica para encontrar valores únicos
    % en la fila, columna y cuadrado.
    % Por ahora, dejamos un marcador "_" indicando que no se ha calculado.
    
    %Pareja_Val_Fila = [PosibilidadesVecinosFila,PosibilidadesPosActual],
    %Pareja_Val_Col = [PosibilidadesVecinosColumna,PosibilidadesPosActual],
    %Pareja_Val_Cuad = [PosibilidadesVecinosCuadrado,PosibilidadesPosActual],
    %eliminar_repetidos(Pareja_Val_Fila, UnicoFila), %mas adelante reemplazaré en posibilidades esos dos valores en su posición correspondiente
    %eliminar_repetidos(Pareja_Val_Col, UnicoColumna),
    %eliminar_repetidos(Pareja_Val_Cuad, UnicoColumna),

    %Actualizo el valor de Posibilidades
    %actualizar_posibilidades_pareja(Posibilidades, Indice, VecinosFila, UnicoFila, NuevaPosibilidades),
    %actualizar_posibilidades_pareja(NuevaPosibilidades, Indice, VecinosColumna, UnicoColumna, NuevaPosibilidades_2),
    %actualizar_posibilidades_pareja(NuevaPosibilidades_2, Indice, VecinosCuadrado, UnicoCuadrado, NuevaPosibilidades_3),


    findall(Valor, (member(Valor, PosiblesValores), \+ member(Valor, PosibilidadesVecinosFila)), UnicoFila),
    findall(Valor, (member(Valor, PosiblesValores), \+ member(Valor, PosibilidadesVecinosColumna)), UnicoColumna),
    findall(Valor, (member(Valor, PosiblesValores), \+ member(Valor, PosibilidadesVecinosCuadrado)), UnicoCuadrado),
    writeln("Unico Fila: "), writeln(UnicoFila),

    (UnicoFila = [] -> ValorUnicoFila = 0 ; [ValorUnicoFila|_] = UnicoFila),
    (UnicoColumna = [] -> ValorUnicoColumna = 0 ; [ValorUnicoColumna|_] = UnicoColumna),
    (UnicoCuadrado = [] -> ValorUnicoCuadrado = 0 ; [ValorUnicoCuadrado|_] = UnicoCuadrado),


    % Actualizar las listas de resultados con los valores únicos encontrados
    append(FilaUnicos, [UnicoFila], NuevaFilaUnicos),
    append(ColumnaUnicos, [UnicoColumna], NuevaColumnaUnicos),
    append(CuadradoUnicos, [UnicoCuadrado], NuevaCuadradoUnicos),

    % Mostrar el contenido actualizado de las listas en cada iteración
    %writeln('Estado actual de NuevaFilaUnicos:'), writeln(NuevaFilaUnicos),
    %writeln('Estado actual de NuevaColumnaUnicos:'), writeln(NuevaColumnaUnicos),
    %writeln('Estado actual de NuevaCuadradoUnicos:'), writeln(NuevaCuadradoUnicos),
    
    % Llamada recursiva para la siguiente casilla
    NuevoIndice is Indice_Regla1 + 1,
    writeln("NuevoIndice: "), writeln(Nuevo_Indice),
    aplicar_regla_1(Sudoku, NuevoIndice, Posibilidades, NuevaFilaUnicos, NuevaColumnaUnicos, NuevaCuadradoUnicos).
    













% Predicado para aplicar la Regla 0 y actualizar el Sudoku
%FALTARIA UNIR CON REGLA 0
resolver_regla_1(Sudoku, NuevoSudoku) :-
    generar_posibilidades(Sudoku, Posibilidades),
    writeln("Posibilidades: "), writeln(Posibilidades),
    aplicar_regla_1(Sudoku, 1, Posibilidades, [], [],[]).
    
resolver_regla_0(Sudoku, NuevoSudoku) :-
    generar_posibilidades(Sudoku, Posibilidades),
    %LLAMADAREGLA1
    %aplicar_regla_1(Sudoku, 1, Posibilidades, [], [], []),
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
    sudoku(Tablero),
    generar_posibilidades(Tablero,Posibilidades),
    %writeln(Posibilidades),
    imprimir_posibilidades(Posibilidades).

%Predicado para probar la Regla 0
probar_regla_0 :-
    sudoku(Tablero),
    resolver_regla_1(Tablero, NuevoTablero). %ESTE PUNTO AQUI SOBRE RECUERDA
    resolver_regla_0(Tablero, NuevoTablero), 
    imprimir_sudoku(NuevoTablero),
    generar_posibilidades(NuevoTablero, PosibilidadesActualizadas),
    imprimir_posibilidades(PosibilidadesActualizadas).