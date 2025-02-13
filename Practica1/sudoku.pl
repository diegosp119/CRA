%Hechos
mujer(maria).
mujer(sara).
hombre(carlos).
hombre(pedro).
hombre(yo).
padre(juan, pedro).
padre(juan, sara).
padre(juan, yo).
madre(maria, pedro).
madre(maria, sara).

hijo(pedro, juan).
hijo(juan, felipe).
hijo(felipe, victor).
hijo(victor, alcalde).
%Reglas
hermanos(X, Y) :-
    setof((X, Y), Z^(padre(Z, X), padre(Z, Y), X \= Y), HermanosPadre),
    setof((X, Y), Z^(madre(Z, X), madre(Z, Y), X \= Y), HermanosMadre),
    append(HermanosPadre, HermanosMadre, TodosHermanos),
    list_to_set(TodosHermanos, HermanosUnicos),
    member((X, Y), HermanosUnicos).


%Reglas de recursividad
descendiente(X, Y) :- hijo(X, Y).
descendiente(X, Y) :- hijo(X, Z), descendiente(Z, Y).
% Consultas de ejemplo
% ?- mujer(maria).
% ?- mujer(X).
% ?- padre(juan, pedro).
% ?- hermanos(X, pedro).