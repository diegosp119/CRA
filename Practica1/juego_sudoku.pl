:- use_module(library(clpfd)).

% ------------------------------------------------------------------------------
% Predicado para resolver el Sudoku usando CLP(FD)
% ------------------------------------------------------------------------------
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

bloques_3x3([], [], []).
bloques_3x3([A,B,C | R1], [D,E,F | R2], [G,H,I | R3]) :-
    all_distinct([A,B,C,D,E,F,G,H,I]),
    bloques_3x3(R1, R2, R3).

% ------------------------------------------------------------------------------
% Transponer una matriz
% ------------------------------------------------------------------------------
transponer([[]|_], []) :- !.
transponer(Matriz, [Columna | Resto]) :-
    extraer_primera_columna(Matriz, Columna, RestoMatriz),
    transponer(RestoMatriz, Resto).

extraer_primera_columna([], [], []).
extraer_primera_columna([[X|Xs]|Filas], [X|Columna], [Xs|Restantes]) :-
    extraer_primera_columna(Filas, Columna, Restantes).

% ------------------------------------------------------------------------------
% Imprimir el Sudoku
% ------------------------------------------------------------------------------
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
    ( var(X) -> write('_') ; write(X) ),
    write(' '),
    imprimir_elementos(Resto).

% ------------------------------------------------------------------------------
% Ingresar Sudoku manualmente con validaciones
% ------------------------------------------------------------------------------
ingresar_sudoku(Sudoku) :-
    writeln('Ingrese el Sudoku fila por fila como listas, usando _ para espacios vacíos.'),
    writeln('Ejemplo: [5,3,_,_,7,_,_,_,_].'),
    ingresar_filas(9, Sudoku).

ingresar_filas(0, []).
ingresar_filas(N, [Fila|Resto]) :-
    N > 0,
    format('Fila ~d: ', [10-N]),
    catch(
        (   read(Fila),
            validar_fila(Fila),
            length(Fila, 9),
            N1 is N - 1,
            ingresar_filas(N1, Resto)
        ),
        error(Mensaje),
        (   writeln(Mensaje), fail )
    ).

validar_fila(Fila) :-
    (   is_list(Fila)
    ->  true
    ;   throw(error('Error: La entrada debe ser una lista.'))
    ),
    maplist(validar_celda, Fila).

validar_celda(Celda) :-
    (   (integer(Celda), Celda >= 1, Celda =< 9)
    ;   var(Celda)
    ->  true
    ;   throw(error('Error: Las celdas deben ser números entre 1 y 9 o _.'))
    ).

% ------------------------------------------------------------------------------
% Lectura robusta de enteros (permite escribir "1." o "2.0")
% ------------------------------------------------------------------------------
leer_entero(Numero) :-
    read(X),
    (   integer(X)
    ->  Numero = X
    ;   float(X),
        X =:= truncate(X)
    ->  Numero is truncate(X)
    ;   throw(error('Entrada inválida: debe ser un número entero.'))
    ).

% ------------------------------------------------------------------------------
% Chequeo de rangos [1..9]
% ------------------------------------------------------------------------------
check_rango(Valor, MensajeError) :-
    ( Valor >= 1, Valor =< 9
    -> true
    ;  throw(error(MensajeError))
    ).

check_rangos(FilaNum, ColNum, Val) :-
    check_rango(FilaNum, 'Fila fuera de rango (debe ser 1-9)'),
    check_rango(ColNum,  'Columna fuera de rango (debe ser 1-9)'),
    check_rango(Val,     'Valor fuera de rango (debe ser 1-9)').

% ------------------------------------------------------------------------------
% Resolución manual (paso a paso) del Sudoku
% ------------------------------------------------------------------------------
resolver_manual(Sudoku) :-
    imprimir_sudoku(Sudoku),
    (   % Caso base: si no hay celdas sin asignar, el Sudoku está completo
        \+ ( member(Fila, Sudoku), member(Celda, Fila), var(Celda) )
    ->  writeln('¡Sudoku completado!'),
        (   resolver_sudoku(Sudoku)
        ->  writeln('El Sudoku es válido.')
        ;   writeln('Error: El Sudoku contiene errores. Revise su solución.')
        ),
        imprimir_sudoku(Sudoku)
    ;   % Caso recursivo: solicitar movimiento y actualizar tablero
        solicitar_movimiento(Sudoku),
        resolver_manual(Sudoku)
    ).

% Solicita fila, columna y valor al usuario, realizando validaciones
solicitar_movimiento(Sudoku) :-
    catch(
      ( writeln('Ingrese fila (1-9): '),
        leer_entero(FilaNum),
        writeln('Ingrese columna (1-9): '),
        leer_entero(ColNum),
        writeln('Ingrese valor (1-9): '),
        leer_entero(Val),
        check_rangos(FilaNum, ColNum, Val),
        % Obtener la celda seleccionada
        nth1(FilaNum, Sudoku, FilaSeleccionada),
        nth1(ColNum, FilaSeleccionada, Celda),
        ( nonvar(Celda)
        -> writeln('Error: Esta celda ya tiene un valor. Intente de nuevo.'),
           fail
        ;  Celda = Val
        )
      ),
      error(Mensaje),
      ( writeln(Mensaje), fail )
    ).
    % NOTA: Se ha eliminado la impresión aquí para evitar duplicados,
    % ya que cada ciclo de resolver_manual/1 la imprime al iniciar.

% ------------------------------------------------------------------------------
% Verificar que cada fila tenga exactamente 9 columnas
% ------------------------------------------------------------------------------
length_(N, Lista) :-
    (   length(Lista, N)
    ->  true
    ;   throw(error('Error: Una fila tiene más de 9 columnas.'))
    ).

% ------------------------------------------------------------------------------
% Predicado principal para iniciar la ejecución
% ------------------------------------------------------------------------------
iniciar :-
    writeln('¿Desea ingresar el Sudoku manualmente o usar uno predefinido?'),
    writeln('1. Ingresar manualmente'),
    writeln('2. Usar Sudoku predefinido'),
    read(Opcion),
    (   Opcion = 1
    ->  ingresar_sudoku(Sudoku),
        resolver_manual(Sudoku)
    ;   sudoku_predefinido(Sudoku),
        resolver_manual(Sudoku)
    ).

% ------------------------------------------------------------------------------
% Sudoku predefinido con espacios vacíos
% ------------------------------------------------------------------------------
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
