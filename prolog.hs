common([H|T], B , C) :-
      common'(H, B),
      C is C + 1.

common'(A, [H|T]):-
      A == H.

common'(A, [H|T]):-
      A \= H,
      common'(A, T).
