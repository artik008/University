union([],X,X).
union([H|L1], L2, Res) :- member(H, L2), union(L1, L2, Res).
union([H|L1], L2, [H|Res]) :- union(L1, L2, Res).
