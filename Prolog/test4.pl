%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% №11 %%  Прототипы: (i, i, i), (i, i, o), (i, o, o), (o, o, i), (o, i, o), (o, o, o)

union([],[],[]).
union([H|L1], L2, Res) :- member(H, L2),!, union(L1, L2, Res).
union([H|L1], L2, Res) :- not(member(H, L2)), delete_one(H, Res, Res2), union(L1, L2, Res2),!.
union([], [H|L2], Res) :- member(H, Res), delete_one(H, Res, Res2), union([], L2, Res2).

delete_one(X, [X|T], T).
delete_one(X, [Y|T], [Y|TY]):- delete_one(X,T,TY).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

edge(a, c, 7).
edge(a, b, 3).
edge(c, d, 12).
edge(b, d, 0). 
edge(e, d, 9).

edge1(X, Y) :- edge(X, Y, _); edge(Y, X, _).

edge2(X, Y, T) :- edge(X, Y, T); edge(Y, X, T).

path2(X, Y, L):- pathl2(X, [Y], L).

pathl2(Y, [Y|T], [Y|T]).
pathl2(X, [Y|T], L):- edge1(Y, Z), not(member(Z, T)), pathl2(X, [Z,Y|T], L).

pathWithWeght([X, Y], N):- edge2(X, Y, N).
pathWithWeght([X, Y|T], N):- edge2(X, Y, M), pathWithWeght([Y|T], N1 ), N is M+N1.

all_path(X, Y, L):- findall(L1, path2(X, Y, L1), L).

wsort([],[]).
wsort([Head|Tail], SortedList):-wsplit(Head, Tail, Left, Right), 
             wsort(Left, SortedLeft), wsort(Right, SortedRight),
             append(SortedLeft, [Head|SortedRight], SortedList).

wsplit(_, [], [], []).
wsplit(Middle, [Head|Tail], [Head|Left], Right):- 
    pathWithWeght(Head, N1), pathWithWeght(Middle, N2), N1 =< N2, !, wsplit(Middle, Tail, Left, Right).
wsplit(Middle, [Head|Tail], Left, [Head|Right]):-wsplit(Middle, Tail, Left, Right).

get_answer(X, L1, Y):- member(Y, L1), pathWithWeght(X, N1), pathWithWeght(Y, N2), N1 =:= N2.


%% №17 %%   Прототипы: (i, i, i), (i, i, o)
min_path(X, Y, L):- all_path(X, Y, L1), wsort(L1, [H|L2]), get_answer(H, [H|L2], L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

newpath([X|T], [Y, X|T]):- edge1(X, Y),not(member(Y, [X|T])). 
 
short_path1([], Y, L, Res):- get_new_level(L, L1), short_path1(L1, Y, [], Res).
short_path1([[H|T1]|T2], Y, L, Res):- dif(H, Y), append(L, [[H|T1]], L1),!, short_path1(T2, Y, L1, Res).
short_path1([[Y|T1]|T2], Y, _, Res):- get_all_path([[Y|T1]|T2], Y, Res),!.

get_all_path([], _, []):-!.
get_all_path([[Y|T]], Y, [[Y|T]]).
get_all_path([[Y|T]|T1], Y, [[Y|T]| T2]):- get_all_path(T1, Y, T2).
get_all_path([[H|_]|T1], Y, L):- dif(H, Y), get_all_path(T1, Y, L).

get_new_level([], []).
get_new_level([H|T], L1):- findall(Z, newpath(H, Z), L), append(L, L2, L1), get_new_level(T, L2).

revers(X,Y):- revers([],X,Y).  
revers(Y,[],Y):- !.  
revers(X1,[Z|X2],Y):- revers([Z|X1],X2,Y).

%% №18 %%  Прототипы: (i, i, i), (i, i, o),

short_path(X, Y, L):- short_path1([[X]], Y, _, L1), member(L2, L1), revers(L2, L).