:- use_module(library(clpfd)).

% Predicado para resolver el Sudoku
resolver_sudoku(Filas) :-
    length(Filas, 9),
    maplist(same_length(Filas), Filas),
    append(Filas, Variables),
    Variables ins 1..9,

    % Restricción: cada fila debe tener valores únicos
    maplist(all_distinct, Filas),

    % Restricción: cada columna debe tener valores únicos
    transponer(Filas, Columnas),
    maplist(all_distinct, Columnas),

    % Restricción: cada subcuadrícula 3x3 debe tener valores únicos
    Filas = [A,B,C, D,E,F, G,H,I],
    bloques_3x3(A, B, C),
    bloques_3x3(D, E, F),
    bloques_3x3(G, H, I).

% Predicado para extraer los bloques 3x3 y asegurarse de que tengan valores únicos
bloques_3x3([], [], []).
bloques_3x3([A,B,C | R1], [D,E,F | R2], [G,H,I | R3]) :-
    all_distinct([A,B,C,D,E,F,G,H,I]),
    bloques_3x3(R1, R2, R3).

% Transponer una matriz
transponer([[]|_], []) :- !.
transponer(Matriz, [Columna | Resto]) :-
    extraer_primera_columna(Matriz, Columna, RestoMatriz),
    transponer(RestoMatriz, Resto).

extraer_primera_columna([], [], []).
extraer_primera_columna([[X|Xs]|Filas], [X|Columna], [Xs|Restantes]) :-
    extraer_primera_columna(Filas, Columna, Restantes).

% Predicado para imprimir el Sudoku
imprimir_sudoku([]).
imprimir_sudoku([Fila | Resto]) :-
    print_fila(Fila),
    imprimir_sudoku(Resto).

print_fila(Fila) :-
    write('| '),
    imprimir_elementos(Fila),
    writeln('|').

imprimir_elementos([]).
imprimir_elementos([X | Resto]) :-
    (var(X) -> write('_') ; write(X)),
    write(' '),
    imprimir_elementos(Resto).
% Predicado para ingresar el Sudoku manualmente de forma segura
ingresar_sudoku(Sudoku) :-
    writeln('Ingrese el Sudoku fila por fila como listas, usando _ para espacios vacíos.'),
    writeln('Ejemplo: [5,3,_,_,7,_,_,_,_].'),
    ingresar_filas(9, Sudoku).

% Predicado para ingresar cada fila con validaciones
ingresar_filas(0, []).
ingresar_filas(N, [Fila|Resto]) :-
    N > 0,
    format('Fila ~d: ', [10-N]),
    catch(
        (   read(Fila),                     % Leer la fila como lista
            validar_fila(Fila),             % Validar la fila ingresada
            length(Fila, 9),               % Verificar que tenga 9 elementos
            N1 is N - 1,
            ingresar_filas(N1, Resto)
        ),
        error(Mensaje),
        (   writeln(Mensaje), fail  % Imprimir mensaje de error y repetir
        )
    ).

% Predicado para validar una fila
validar_fila(Fila) :-
    (   is_list(Fila) -> true ; throw(error('Error: La entrada debe ser una lista.')) ),
    maplist(validar_celda, Fila).  % Validar cada celda

validar_celda(Celda) :-
    (   (integer(Celda), Celda >= 1, Celda =< 9) ; var(Celda) ->
        true
    ;   throw(error('Error: Las celdas deben ser números entre 1 y 9 o _.'))
    ).



% Predicado para resolver manualmente el Sudoku paso a paso con manejo de excepciones
resolver_manual(Sudoku) :-
    imprimir_sudoku(Sudoku),
    (   member(Fila, Sudoku), member(Celda, Fila), var(Celda) ->
        repeat,
        catch(
            (   % Solicitar fila y validar entrada
                writeln('Ingrese fila (1-9): '), 
                read(FilaNum),
                (integer(FilaNum) -> true ; throw(error('Entrada inválida: debe ser un número entero.'))),

                % Solicitar columna y validar entrada
                writeln('Ingrese columna (1-9): '), 
                read(ColNum),
                (integer(ColNum) -> true ; throw(error('Entrada inválida: debe ser un número entero.'))),

                % Solicitar valor y validar entrada
                writeln('Ingrese valor (1-9): '), 
                read(Valor),
                (integer(Valor) -> true ; throw(error('Entrada inválida: debe ser un número entero.'))),

                % Verificar límites para fila, columna y valor
                (   (FilaNum < 1 ; FilaNum > 9) ->
                    throw(error('Fila fuera de rango (debe ser 1-9)'))
                ;   (ColNum < 1 ; ColNum > 9) ->
                    throw(error('Columna fuera de rango (debe ser 1-9)'))
                ;   (Valor < 1 ; Valor > 9) ->
                    throw(error('Valor fuera de rango (debe ser 1-9)'))
                ),

                % Verificar longitud de las filas (maximo 9 columnas)
                maplist(length_(9), Sudoku),

                % Obtener la celda seleccionada
                nth1(FilaNum, Sudoku, FilaSeleccionada),
                nth1(ColNum, FilaSeleccionada, Celda),

                (   nonvar(Celda) ->
                    writeln('Error: Esta celda ya tiene un valor. Intente de nuevo.'), fail
                ;   Celda = Valor
                ),
                resolver_manual(Sudoku)
            ),
            error(Mensaje),
            (   writeln(Mensaje), fail  % Imprimir mensaje de error y repetir
            )
        )
    ;   writeln('¡Sudoku completado!'),
        (   resolver_sudoku(Sudoku) -> writeln('El Sudoku es válido.')
        ;   writeln('Error: El Sudoku contiene errores. Revise su solución.')
        ),
        imprimir_sudoku(Sudoku)
    ).

% Verificar que cada fila tenga exactamente 9 columnas
length_(N, Lista) :-
    (   length(Lista, N) ->
        true
    ;   throw(error('Error: Una fila tiene más de 9 columnas.'))
    ).



% Predicado para iniciar la resolución
iniciar :-
    writeln('¿Desea ingresar el Sudoku manualmente o usar uno predefinido?'),
    writeln('1. Ingresar manualmente'),
    writeln('2. Usar Sudoku predefinido'),
    read(Opcion),
    (   Opcion = 1 ->
        ingresar_sudoku(Sudoku),
        resolver_manual(Sudoku)
    ;   sudoku_predefinido(Sudoku),
        resolver_manual(Sudoku)
    ).

% Sudoku predefinido con espacios vacíos
sudoku_predefinido(
    [[5,3,_, _,7,_, _,_,_],
     [6,_,_, 1,9,5, _,_,_],
     [_,9,8, _,_,_, _,6,_],
     [8,_,_, _,6,_, _,_,3],
     [4,_,_, 8,_,3, _,_,1],
     [7,_,_, _,2,_, _,_,6],
     [_,6,_, _,_,_, 2,8,_],
     [_,_,_, 4,1,9, _,_,5],
     [_,_,_, _,8,_, _,7,9]]).
