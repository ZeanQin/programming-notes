take(N, List, Front) :-
  length(Front, N),
  append(Front, _, List).
