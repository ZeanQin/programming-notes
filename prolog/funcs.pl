listof(_Elt, []).
listof(Elt, [Elt|Rest]) :-
  listof(Elt, Rest).

all_same([]).
all_same([_A]).
all_same([A, A|Rest]) :-
  all_same([A|Rest]).

adjacent(E1, E2, [E1, E2|_Back]).
adjacent(E1, E2, [_|Back]) :-
  adjacent(E1, E2, Back).

before(E1, E2, [E1|Tail]) :-
  member(E2, Tail).
before(E1, E2, [_|Tail]) :-
  before(E1, E2, Tail).

intset_member(N, tree(_L, N, _R)).
intset_member(N, tree(L, Value, R)) :-
  (N < Value ->
    intset_member(N, L)
  ; intset_member(N, R)
  ).
  
intset_insert(N, empty, node(empty, N, empty)).
intset_insert(N, node(L, V, R), node(L1, V, R1)) :- 
  (N = V -> 
    L1 = L,
    R1 = R
  ;N < V -> 
    R1 = R,
    intset_insert(N, L, L1)
  ; N > V ->
    L1 = L,
    intset_insert(N, R, R1)
  ).

sumlist([], 0).
sumlist([A|As], Sum) :- 
  sumlist([A|As], 0, Sum).

sumlist([], Acc, Acc).
sumlist([A|As], Acc, Sum) :-
  Acc1 is Acc + A,
  sumlist(As, Acc1, Sum).

tree_list(empty, []).
tree_list(node(L, V, R), List) :-
  tree_list(L, LList),
  tree_list(R, RList),
  append(LList, [V], TempList),
  append(TempList, RList, List).
 

build_balanced_tree([], empty).
build_balanced_tree([E|Rest], node(L, E, R)) :-
  length(Rest, Len),
  Len2 is Len // 2,
  append(Front, [E|Back], [E|Rest]),
  length(Front, Len2),
  build_balanced_tree(Front, L),
  build_balanced_tree(Back, R).

same_elements([E1|E1s], [E2|E2s]) :- 
  all_in([E1|E1s], [E2|E2s]),
  all_in([E2|E2s], [E1|E1s]).

all_in([E|Es], L) :-
  member(E, L), 
  all_in(Es, L).

fibs :: Int -> [Integer]
fibs 0 = []
fibs 1 = [0]
fibs n 
  | n > 1 = 0:1:fibs1 0 1 (n-2)

fibs1 fpp fp 0 = []
fibs1 fpp fp n = (fpp+fp):fibs1 fp (fpp+fp) (n-1)
