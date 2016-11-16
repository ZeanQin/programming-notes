% This file contains an implementation of a binary tree in prolog. 
% The 'binary_tree.hs' in the haskell folder contains an implementation of 
% exactly the same binary tree in haskell. 

% empty
% node empty 5.5 empty

add(Elt, empty, node(empty, Elt, empty)).
add(Elt, node(L, Value, R), node(L1, Value, R1)) :-
  (Elt = Value ->
    L1 = L,
    R1 = R
  ;Elt < Value -> 
    R1 = R,
    add(Elt, L, L1)
  ;Elt > Value ->
    L1 = L, 
    add(Elt, R, R1)  
  ).

count_nodes(empty, 0).
count_nodes(node(L, _Value, R), Count) :- 
  count_nodes(L, LCount),
  count_nodes(R, RCount), 
  Count is LCount + RCount + 1.

sum_nodes(empty, 0.0).
sum_nodes(node(L, Value, R), Sum) :- 
  sum_nodes(L, LSum),
  sum_nodes(R, RSum),
  Sum is LSum + RSum + Value.

flatten(empty, []).
flatten(node(L, Value, R), Values) :- 
  flatten(L, LValues),
  flatten(R, RValues),
  append(LValues, RValues, LeafValues),
  append(LeafValues, [Value], Values).

average_value(empty, 0.0).
average_value(node(L, Value, R), Average) :-
  get_sum_count(node(L, Value, R), (Sum, Count)),
  Average is Sum / Count.

get_sum_count(empty, (0.0, 0.0)).
get_sum_count(node(L, Value, R), (Sum, Count)) :-
  get_sum_count(L, (LSum, LCount)),
  get_sum_count(R, (RSum, RCount)),
  Sum is LSum + RSum + Value,
  Count is LCount + RCount + 1.

average_value1(empty, 0.0). 
average_value1(node(L, Value, R), Average) :- 
  flatten(node(L, Value, R), Nodes),
  sum_list(Nodes, Sum),
  length(Nodes, Length),
  Average is Sum / Length.

average_value2(empty, 0.0).
average_value2(node(L, Value, R), Average) :-
  sum_nodes(node(L, Value, R), Sum),
  count_nodes(node(L, Value, R), Count),
  Average is Sum / Count.

same_shape(empty, empty).
same_shape(node(L, _, R), node(L1, _, R1)) :- 
  same_shape(L, L1),
  same_shape(R, R1). 
