drop(N, List, Back) :-
  append(Front, Back, List),
  length(Front, N).
