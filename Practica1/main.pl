% Representación del Sudoku como una lista de 81 elementos

%Sudoku 1 muy facil
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

%Sudoku 25 muy facil
sudoku1([., ., 7, ., ., 8, ., ., .,
         ., 4, 5, 7, 6, ., ., ., 2,
         6, ., ., ., 4, ., 3, ., 5,
         8, 6, ., 5, ., ., 4, ., .,
         ., ., 3, 8, ., 4, ., 6, .,
         ., 7, 2, 6, 9, ., ., 8, 3,
         ., 5, ., ., ., ., ., 4, 7,
         7, ., ., 4, ., ., ., ., 6,
         ., 3, 4, ., ., 6, 2, ., .]).

%Sudoku 985 muy facil
sudoku2([
    ., 7, 2, 4, ., ., ., ., .,
    ., ., 8, ., ., ., ., ., 4,
    ., ., ., 2, ., ., ., ., 9,
    2, ., 9, 7, ., ., ., ., .,
    ., 3, ., 8, 4, ., 9, 6, 2,
    ., 6, 4, 5, ., 2, 3, 8, .,
    5, ., 6, 3, ., 8, ., 4, .,
    3, 4, 7, ., 2, 6, 5, ., 8,
    ., 2, ., ., ., 4, ., ., .]).

%Sudoku 988 muy facil
sudoku3([
    ., 7, ., ., 5, ., 3, ., .,
    ., ., 6, 3, 2, 7, ., 1, .,
    5, ., ., ., 8, 9, 4, 6, .,
    3, ., 8, ., 6, 2, 9, 7, .,
    7, 6, ., 9, 3, ., 8, 4, 5,
    ., ., 5, 7, 4, 8, 2, ., 6,
    ., ., 4, 8, ., ., ., ., 3,
    ., 8, ., 2, 9, ., ., 5, .,
    6, ., ., 4, 7, 5, ., 8, 2]).

%Sudoku 991 muy facil
sudoku4([
    ., 4, ., 6, ., 2, 8, ., 3,
    3, 8, 2, 9, 7, ., ., 4, 5,
    ., ., 6, 8, ., 4, 2, 9, 7,
    4, 2, 5, 3, 6, ., ., ., .,
    ., ., 3, ., ., 7, ., 6, .,
    ., ., ., ., ., ., ., 3, 8,
    5, 7, ., 2, ., ., 3, 8, .,
    6, 3, ., 4, 9, 5, 7, 2, .,
    ., ., 4, 7, 8, ., ., 5, 6 ]).

%Sudoku 1007 facil
sudoku5([
    ., ., ., ., 9, 7, 4, 6, .,
    ., 4, ., 1, ., ., ., 9, .,
    ., ., ., ., ., 6, 1, 5, 8,
    ., 6, 4, 2, ., ., 9, 3, .,
    ., ., ., ., 4, ., ., ., .,
    ., 7, ., 6, ., 5, ., 8, .,
    4, 9, ., 3, 8, 1, ., ., 5,
    ., ., 6, ., ., ., ., 1, 9,
    ., ., 2, 5, ., ., 7, ., .]).

%Sudoku 1001 facil
sudoku6([ ., ., 6, ., ., 2, 3, ., 4,
         9, ., 4, 7, 5, ., ., 8, 2,
         ., ., 8, ., ., 6, ., ., 5,
         ., ., 3, ., ., ., ., 4, .,
         2, ., ., 4, ., ., 8, 3, .,
         4, ., 7, 5, ., ., ., ., .,
         ., ., ., 6, ., ., ., ., 8,
         7, ., ., ., 2, ., 4, 5, 3,
         ., ., ., 3, 7, ., ., 6, 9]).


%Sudoku 1010 facil
sudoku7([
    ., 3, ., ., ., ., 4, 7, .,
    ., ., 4, ., 7, ., ., ., 8,
    8, 7, ., 6, ., 4, ., ., 2,
    ., ., 5, ., ., 8, 2, ., 6,
    3, ., 2, 4, ., ., 7, ., 5,
    ., ., ., ., 2, 7, ., ., 3,
    5, 4, ., 9, 6, ., 3, 8, .,
    ., ., ., ., 4, 3, ., 6, .,
    ., ., ., ., ., ., ., 2, .]).

%Sudoku 1217 facil
sudoku8([
    ., ., ., ., 3, ., ., ., 4,
    ., 9, ., 4, ., 6, ., 7, .,
    ., 5, ., ., ., ., 3, 8, .,
    ., ., ., ., 7, 8, ., ., 3,
    3, ., ., ., ., ., 6, 9, .,
    5, 4, ., 6, ., ., ., 2, .,
    7, ., 5, ., 2, 4, ., ., .,
    9, 8, 4, ., 6, 5, 2, ., .,
    ., 2, 6, ., 8, ., ., ., 9]).

%Sudoku 1220 facil
sudoku9([
    ., 8, ., ., ., ., 6, 9, 3,
    2, 6, 9, ., ., ., ., ., 5,
    ., 7, ., 9, 6, ., 2, ., 8,
    ., ., ., 4, ., ., 8, ., 9,
    ., ., 8, 6, ., ., 4, 5, .,
    ., 3, 1, ., ., ., ., ., .,
    3, ., 2, 5, 8, ., ., ., 7,
    ., ., ., ., ., 6, ., 1, .,
    7, 4, 6, ., 3, ., ., ., .]).

%Sudoku 1 medio Repositorio Github
sudoku10([
    ., 2, ., 9, ., ., ., ., .,
    ., 4, 8, ., ., ., ., 3, 1,
    ., ., ., ., 6, 3, ., 2, .,
    ., ., 9, 4, ., 7, ., ., 3,
    ., ., 3, ., 8, ., 2, ., .,
    4, ., ., 1, ., 5, 6, ., .,
    ., 3, ., 5, 7, ., ., ., .,
    2, 5, ., ., ., ., 1, 8, .,
    ., ., ., ., ., 6, ., 5, .]).

%Sudoku 2 medio Repositorio Github
sudoku11([
    1, ., ., 8, ., ., 5, 7, .,
    ., ., ., ., ., 9, 2, 1, .,
    ., 9, ., ., 4, ., ., ., .,
    3, ., ., 9, ., ., ., 5, .,
    ., ., 7, ., ., ., 3, ., .,
    ., 2, ., ., ., 6, ., ., 8,
    ., ., ., ., 2, ., ., 4, .,
    ., 7, 1, 4, ., ., ., ., .,
    ., 6, 4, ., ., 7, ., ., 3]).

%Sudoku 3 medio Repositorio Github
sudoku12([
    ., ., 2, ., ., ., 8, ., .,
    ., ., 5, ., 2, ., 1, ., .,
    4, 6, ., ., ., ., ., 2, 9,
    1, 3, ., ., 6, ., ., 5, 2,
    ., ., 9, ., 8, ., 4, ., .,
    ., ., ., 3, ., 2, ., ., .,
    ., ., 6, ., 7, ., 2, ., .,
    7, ., ., ., ., ., ., ., 8,
    ., 2, ., 5, 1, 9, ., 7, .]).

%Sudoku 4 medio Repositorio Github
sudoku13([
    8, ., 2, 6, ., ., ., ., 9,
    ., ., ., ., 5, 8, ., ., .,
    ., ., 6, ., ., ., 4, ., 1,
    ., 9, ., 4, ., 6, ., ., 5,
    ., 2, ., ., ., ., ., 4, .,
    6, ., ., 2, ., 3, ., 9, .,
    2, ., 5, ., ., ., 9, ., .,
    ., ., ., 9, 7, ., ., ., .,
    1, ., ., ., ., 2, 8, ., 4]).

%Sudoku 5 medio Repositorio Github
sudoku14([
    ., 7, ., ., ., ., 1, 2, .,
    1, ., ., ., ., ., ., 6, 7,
    ., ., ., 2, ., ., ., ., 4,
    2, ., ., ., 4, ., ., 7, .,
    7, 1, ., ., 3, ., ., 4, 9,
    ., 9, ., ., 7, ., ., ., 1,
    3, ., ., ., ., 9, ., ., .,
    9, 5, ., ., ., ., ., ., 6,
    ., 6, 7, ., ., ., ., 8, .]).

%Sudoku 1 dificil Repositorio Github
sudoku15([
    ., 8, ., 2, ., ., 4, ., .,
    5, 7, ., ., ., ., 1, ., .,
    ., ., 2, 3, ., ., ., ., .,
    8, 2, ., ., 9, ., ., ., 5,
    ., ., ., 7, 1, 5, ., ., .,
    7, ., ., ., 2, ., ., 4, 1,
    ., ., ., ., ., 6, 7, ., .,
    ., ., 3, ., ., ., ., 1, 8,
    ., ., 7, ., ., 9, ., 5, .]).

%Sudoku 2 dificil Repositorio Github
sudoku16([
    6, ., ., ., 5, ., ., ., 7,
    ., 3, ., ., ., ., ., ., .,
    ., 8, ., 4, ., 9, 2, ., .,
    ., 1, 5, 3, ., ., ., ., .,
    ., ., 8, ., ., ., 3, ., .,
    ., ., ., ., ., 7, 5, 9, .,
    ., ., 9, 5, ., 1, ., 3, .,
    ., ., ., ., ., ., ., 8, .,
    2, ., ., ., 7, ., ., ., 4]).

%Sudoku 3 dificil Repositorio Github
sudoku17([
    2, 1, ., 9, 5, ., ., ., 4,
    ., 9, ., ., 6, ., ., 3, 7,
    ., ., ., 7, ., ., ., ., .,
    ., ., ., ., ., ., 3, ., 8,
    9, 2, ., ., ., ., ., 1, 5,
    8, ., 5, ., ., ., ., ., .,
    ., ., ., ., ., 2, ., ., .,
    6, 8, ., ., 1, ., ., 4, .,
    1, ., ., ., 4, 7, ., 9, 6]).

%Sudoku 4 dificil Repositorio Github
sudoku18([
    ., 2, 4, ., ., ., 6, 5, .,
    1, ., ., ., ., ., ., ., 7,
    ., ., 8, ., 1, ., 9, ., .,
    ., ., ., ., ., ., ., ., .,
    2, 6, ., ., 9, ., ., 8, 3,
    ., 8, ., 5, ., 1, ., 7, .,
    6, ., ., 9, ., 3, ., ., 8,
    ., ., 2, 8, 5, 4, 7, ., .,
    ., ., ., ., 7, ., ., ., .]).

%Sudoku 5 dificil Repositorio Github
sudoku19([
    ., ., ., ., 5, ., ., ., .,
    ., ., ., 2, ., 6, ., ., .,
    ., 6, 4, ., ., ., 3, 9, .,
    ., 4, 5, ., ., ., 8, 1, .,
    ., ., ., ., 2, ., ., ., .,
    ., ., ., 1, ., 7, ., ., .,
    ., 5, 3, ., ., ., 9, 8, .,
    ., 9, ., 8, ., 4, ., 6, .,
    1, ., ., ., 3, ., ., ., 4]).

%Sudoku 1 diabolico Github
sudoku20([
    ., 8, 3, ., 2, ., ., 9, .,
    ., ., ., 8, ., ., 1, ., .,
    ., 2, 9, 3, ., ., ., ., 8,
    ., ., ., ., 9, 8, 7, ., .,
    ., 7, ., ., ., ., ., 6, .,
    ., ., 6, 7, 4, ., ., ., .,
    3, ., ., ., ., 6, 9, 8, .,
    ., ., 2, ., ., 5, ., ., .,
    ., 1, ., ., 3, ., 5, 4, .]).

%Sudoku 2 diabolico Github
sudoku21([
    2, ., ., ., 5, ., ., ., 6,
    ., 1, ., ., ., ., ., 9, .,
    6, ., ., 8, ., 1, ., ., 3,
    ., ., 7, ., 9, ., 6, ., .,
    ., ., ., 7, ., 3, ., ., .,
    9, ., ., ., 8, ., ., ., 2,
    1, ., ., ., ., ., ., ., 5,
    ., 6, ., 9, ., 2, ., 1, .,
    ., ., 3, ., 6, ., 2, ., .]).

%Sudoku 3 diabolico Github
sudoku22([
    5, 9, ., ., ., ., ., ., 7,
    ., 4, ., ., 1, ., ., 8, 3,
    ., ., 8, ., 3, 4, 9, ., .,
    ., ., 1, 4, ., 2, ., ., .,
    ., 6, 9, ., ., ., 8, 2, .,
    ., ., ., 1, ., 9, 3, ., .,
    ., ., 4, 6, 7, ., 2, ., .,
    9, 8, ., ., 4, ., ., 3, .,
    7, ., ., ., ., ., ., 1, 6
]).

% Sudoku 4 diabolico Github
sudoku23([
    ., ., 6, ., ., ., 2, ., .,
    9, ., ., ., ., ., ., ., 4,
    2, 4, 3, ., ., ., 8, 9, 6,
    ., ., ., 5, 9, 1, ., ., .,
    ., ., 2, ., 8, ., 3, ., .,
    4, ., ., 2, ., 3, ., ., 1,
    3, ., ., ., ., ., ., ., 7,
    ., ., ., 9, ., 7, ., ., .,
    ., 1, ., 4, ., 8, ., 2, .]).

% Sudoku 5 diabolico Github
sudoku24([
    ., ., ., ., ., ., ., ., .,
    5, 6, ., ., ., ., ., 3, 2,
    2, 3, ., ., 4, ., ., 7, 9,
    ., ., ., ., 6, ., ., ., .,
    ., 7, ., 5, ., 1, ., 9, .,
    ., ., ., 7, ., 8, ., ., .,
    ., 5, 3, ., ., ., 9, 2, .,
    ., ., 9, 8, ., 6, 5, ., .,
    7, ., ., ., ., ., ., ., 4 ]).

%Sudoku prueba regla 3
sudoku25([
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

% Cargamos el archivo que contiene la lógica de las reglas
:- consult('reglas.pl').
:- consult('deteccion_errores.pl').

%interfaz principal
main :- 
    writeln('Que desea hacer: '),
    writeln('1. Ingresar un sudoku'),
    writeln('2. Realizar pruebas de reglas'),
    writeln('3. Probar sudokus por nivel de dificultad'),
    writeln('4. Prueba de deteccion de errores'),
    read(Opcion),
    (   Opcion == 1
    ->  interfaz_interactiva
    ;   Opcion == 2
    ->  interfaz_prueba
    ;   Opcion == 3
    ->  probar_contar_sudokus_por_nivel
    ;   Opcion == 4
    ->  verificar_sudoku_poss
    ).

%interfaz de pruebas

interfaz_prueba :-
    writeln('Que sudoku desea probar: '),
    writeln('1. probar_regla_0'),
    writeln('2. probar_regla_1'),
    writeln('3. probar_regla_2'),
    writeln('4. probar_regla_3'),
    writeln('5. probar_reglas_0_y_1'),
    writeln('6. probar_reglas_0_y_2'),
    writeln('7. probar_reglas_0_y_3'),
    writeln('8. probar_reglas_0_1_y_2'),
    writeln('9. probar_reglas_0_1_2_y_3'),
    read(Opcion),
    (   Opcion == 1
    -> probar_regla_0,
    writeln('Regla 0 aplicada')
    ;   Opcion == 2
    -> probar_regla_1,
    writeln('Regla 1 aplicada')
    ;   Opcion == 3
    -> probar_regla_2,
    writeln('Regla 2 aplicada')
    ;   Opcion == 4
    -> probar_regla_3,
    writeln('Regla 3 aplicada')
    ;   Opcion == 5
    -> probar_reglas_0_y_1,
    writeln('Reglas 0 y 1 aplicadas')
    ;   Opcion == 6
    -> probar_reglas_0_y_2,
    writeln('Reglas 0 y 2 aplicadas')
    ;   Opcion == 7
    -> probar_reglas_0_y_3,
    writeln('Reglas 0 y 3 aplicadas')
    ;   Opcion == 8
    -> probar_reglas_0_y_1_y_2,
    writeln('Reglas 0, 1 y 2 aplicadas')
    ;   Opcion == 9
    -> probar_reglas_0_y_1_y_2_y_3,
    writeln('Reglas 0, 1, 2 y 3 aplicadas')
    ).








% interfaz interactiva 
interfaz_interactiva :-
    writeln('Ingrese el Sudoku como una lista de 81 elementos (usar . para celdas vacías):'),
    read(Sudoku),
    (   length(Sudoku, 81)
    ->  interfaz_menu(Sudoku)
    ;   writeln('Error: La lista ingresada debe tener 81 elementos.')
    ).

%------------------------------------------
% Bucle principal de la interfaz
%------------------------------------------
interfaz_menu(Sudoku) :-
    writeln('------------------------------'),
    writeln('Estado actual del Sudoku:'),
    imprimir_sudoku(Sudoku),
    writeln('------------------------------'),
    writeln('Seleccione la regla a aplicar:'),
    writeln('   0 - Regla 0 (actualiza celdas con posibilidad única)'),
    writeln('   1 - Regla 1 (eliminación de únicos globales)'),
    writeln('   2 - Regla 2 (eliminación de pares únicos)'),
    writeln('   3 - Regla 3 (eliminación de tríos únicos)'),
    writeln('   t - Terminar la resolución'),
    read(Opcion),
    (   Opcion == t
    ->  writeln('Resolución terminada.')
    ;   aplicar_regla_interactiva(Opcion, Sudoku, NuevoSudoku),
        interfaz_menu(NuevoSudoku)
    ).