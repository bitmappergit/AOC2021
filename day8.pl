% demoIn = ["acedgfb", "cdfbe", "gcdfa", "fbcad", "dab", "cefabd", "cdfgeb", "eafb", "cagedb", "ab"]
% demoOut = ["cdfeb", "fcadb", "cdfeb", "cdbaf"]

demoIn([[a,c,e,d,g,f,b], [c,d,f,b,e], [g,c,d,f,a], [f,b,c,a,d], [d,a,b], [c,e,f,a,b,d], [c,d,f,g,e,b], [e,a,f,b], [c,a,g,e,d,b], [a,b]]).
demoOut([[c,d,f,e,b], [f,c,a,d,b], [c,d,f,e,b], [c,d,b,a,f]]).

constraint(0, [0, 8], [1, 2, 3, 4, 5, 6, 7, 9]).
constraint(1, [0, 1, 3, 4, 7, 8, 9], [2, 5, 6]).
constraint(2, [2, 8], [0, 1, 3, 4, 5, 6, 7, 9]).
constraint(3, [3, 8, 9], [0, 1, 2, 4, 5, 6, 7]).
constraint(4, [4, 8, 9], [0, 1, 2, 3, 5, 6, 7]).
constraint(5, [5, 6, 8, 9], [0, 1, 2, 3, 4, 7]).
constraint(6, [6, 8], [0, 1, 2, 3, 4, 5, 7, 9]).
constraint(7, [0, 3, 7, 8, 9], [1, 2, 4, 5, 6]).
constraint(8, [8], [0, 1, 2, 3, 4, 5, 6, 7, 9]).
constraint(9, [8, 9], [0, 1, 2, 3, 4, 5, 6, 7]).

lookup(Key, Value, [(Key, Value) | _]).
lookup(Key, Value, [_ | Rest]) :-
  lookup(Key, Value, Rest).

solve_signals(In, Solved, Out) :-
  member((_, X), Solved),
  lookup(_, Y, In),
  intersection(X, Y, 
  constraint(In, Exc, Inc).

solve_unique([], [], []).
solve_unique([I | Is], [N | Ns], [R | Rs]) :-
  length(I, L),
  ( L = 2 -> (R = 1, N = [1])
  ; L = 4 -> (R = 4, N = [4])
  ; L = 3 -> (R = 7, N = [7])
  ; L = 7 -> (R = 8, N = [8])
  ; true
  ),
  solve_unique(Is, Ns, Rs).
