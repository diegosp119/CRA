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


repetidos_fila(Posibilidades, Index, Num) :-
    Fila is Index // 9,
    Inicio is Fila * 9,
    Fin is Inicio + 8,
    between(Inicio, Fin, I),
    nth0(I, Sudoku, Num).

% Predicado para verificar si un número está en la misma columna
repetidos_columna(Sudoku, Index, Num) :-
    Columna is Index mod 9,
    between(0, 8, Fila),
    I is Fila * 9 + Columna,
    nth0(I, Sudoku, Num).

% Predicado para verificar si un número está en el mismo cuadro 3x3
repetidos_cuadro(Sudoku, Index, Num) :-
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


% Predicado para procesar las posibilidades
procesar_posibilidades(Index, Posibilidades) :-
    % Calcular fila, columna y Cuadrado

    Columna is  (((Index//9) mod 3) * 3 )+ (Index mod 3),
    Fila is ((Index//27) * 3 )+ ((Index mod 9)//3),
    Cuadricula is Index//9,
    

    % Inicializar listas para las posibilidades y repetidos
    inicializar_listas(9, FilaPosibilidades),
    writeln(Fila).
    %inicializar_listas(9, ColumnaPosibilidades),
    %inicializar_listas(9, CuadradoPosibilidades),
    %inicializar_listas(9, FilaRepetidos),
    %inicializar_listas(9, ColumnaRepetidos),
    %inicializar_listas(9, CuadradoRepetidos),
    

    % Procesar las posibilidades, agregar a las listas correspondientes
    %procesar_posibilidades_recursivo(Posibilidades, Fila, Columna, Cuadrado, FilaPosibilidades, ColumnaPosibilidades, CuadradoPosibilidades, FilaRepetidos, ColumnaRepetidos, CuadradoRepetidos),
    %imprimir_repetidos(FilaRepetidos[Fila]),
    %imprimir_repetidos(ColumnaRepetidos[Columna]),
    %imprimir_repetidos(CuadradoRepetidos[Cuadrado]).






    








generar_posibilidades(_, 81, []).
generar_posibilidades(Sudoku, Index, [Posibilidades|Resto]) :-
    Index < 81,
    posibilidades_casilla_regla1(Sudoku, Index, Posibilidades),
    NextIndex is Index + 1,
    generar_posibilidades(Sudoku, NextIndex, Resto),
    


% Predicado para generar las listas de posibilidades para todas las casillas
generar_posibilidades(Sudoku, Posibilidades) :-
    generar_posibilidades(Sudoku, 0, Posibilidades),

    procesar_posibilidades(Sudoku, 0, Posibilidades).



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
    sudoku(Tablero),
    generar_posibilidades(Tablero,Posibilidades),
    %writeln(Posibilidades),
    imprimir_posibilidades(Posibilidades).

%Predicado para probar la Regla 0
probar_regla_0 :-
    sudoku(Tablero),
    resolver_regla_0(Tablero, NuevoTablero),
    imprimir_sudoku(NuevoTablero),
    generar_posibilidades(NuevoTablero, PosibilidadesActualizadas),
    imprimir_posibilidades(PosibilidadesActualizadas).
























%Cosas de la Regla 1


% Caso base: cuando hemos procesado todas las casillas
aplicar_regla_1(_, 82, _, FilaUnicos, ColumnaUnicos, CuadradoUnicos) :-
    %writeln('Regla 1 aplicada con exito'),
    writeln(FilaUnicos),
    writeln(ColumnaUnicos),
    writeln(CuadradoUnicos).

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
    
    % Aquí debes implementar la lógica para encontrar valores únicos
    % en la fila, columna y cuadrado.
    % Por ahora, dejamos un marcador "_" indicando que no se ha calculado.
    findall(Valor, (member(Valor, PosiblesValores), \+ member(Valor, PosibilidadesVecinosFila)), UnicoFila),
    findall(Valor, (member(Valor, PosiblesValores), \+ member(Valor, PosibilidadesVecinosColumna)), UnicoColumna),
    findall(Valor, (member(Valor, PosiblesValores), \+ member(Valor, PosibilidadesVecinosCuadrado)), UnicoCuadrado),

    (UnicoFila = [] -> ValorUnicoFila = 0 ; [ValorUnicoFila|_] = UnicoFila),
    (UnicoColumna = [] -> ValorUnicoColumna = 0 ; [ValorUnicoColumna|_] = UnicoColumna),
    (UnicoCuadrado = [] -> ValorUnicoCuadrado = 0 ; [ValorUnicoCuadrado|_] = UnicoCuadrado),


    % Actualizar las listas de resultados con los valores únicos encontrados
    append(FilaUnicos, [UnicoFila], NuevaFilaUnicos),
    append(ColumnaUnicos, [UnicoColumna], NuevaColumnaUnicos),
    append(CuadradoUnicos, [UnicoCuadrado], NuevaCuadradoUnicos),

    % Mostrar el contenido actualizado de las listas en cada iteración
    writeln('Estado actual de NuevaFilaUnicos:'), writeln(NuevaFilaUnicos),
    writeln('Estado actual de NuevaColumnaUnicos:'), writeln(NuevaColumnaUnicos),
    writeln('Estado actual de NuevaCuadradoUnicos:'), writeln(NuevaCuadradoUnicos),
    
    % Llamada recursiva para la siguiente casilla
    NuevoIndice is Indice_Regla1 + 1,
    aplicar_regla_1(Sudoku, NuevoIndice, Posibilidades, NuevaFilaUnicos, NuevaColumnaUnicos, NuevaCuadradoUnicos).










