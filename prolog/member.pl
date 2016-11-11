member1(Elt, List) :- 
  append(_, [Elt|_], List).

member2(Elt, [Elt|_]).
member2(Elt, [_|Rest]) :-
  member2(Elt, Rest).
