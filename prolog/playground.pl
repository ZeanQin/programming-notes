proper_list([]).
proper_list([_Head | Tail]) :-
  proper_list(Tail).

% append(List1, List2, List3).
append([], C, C).
append([A|B], C, [A|New]) :-
  append(B, C, New).

fact(0, 1).
fact(N, F) :-
  N1 = N - 1,
  fact(N1, F1), 
  F is N * F1.
