%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Author: Zean Qin
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% this holds when list L1 is the same as L2, except that in one place where L1
% has the value E1, L2 has E2.
replace(E1, [E1|Es], E2, [E2|Es]).
replace(E1, [E|As], E2, [E|Bs]) :-
  replace(E1, As, E2, Bs).

% this holds when As, Bs, and ABs are lists of the same length, and each 
% element of ABs is a term of the form A-B where A is the corresponding 
% element of As and B is the corresponding element of Bs
zip([], [], []).
zip([A|As], [B|Bs], [A-B|ABs]) :-
  zip(As, Bs, ABs).

% this holds when Xs is a list containing some of the elements of Ys, 
% in the same order they appear in the list Ys.
sublist([], []).
sublist([], [_Y|_Ys]).
sublist([E|Xs], [E|Ys]) :- 
  sublist(Xs, Ys).
sublist([X|Xs], [_Y|Ys]) :- 
  sublist([X|Xs], Ys).
