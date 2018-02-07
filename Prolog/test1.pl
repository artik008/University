reverse([], L).
reverse([X|L1], L2):-reverse(L1, [X|L2]).


reverse([1,2,3,4,5], Y).
reverse([1,2,3,4,5], [5,4,3,2,1]).
