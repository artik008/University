%% №2 %%  Прототипы: (i, i), (o, i), (i, o), (o, o)

revers(X,Y):- revers([],X,Y).  
revers(Y,[],Y):- !.  
revers(X1,[Z|X2],Y):- revers([Z|X1],X2,Y).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% №9 %%  Прототипы: (i, i), (i, o), (o, o)

qusort([],[]):- !.
qusort([Head|Tail], SortedList):-split(Head, Tail, Left, Right), 
           qusort(Left, SortedLeft), qusort(Right, SortedRight),
           append(SortedLeft, [Head|SortedRight], SortedList).

split(_, [], [], []).
split(Middle, [Head|Tail], [Head|Left], Right):- (Head =< Middle), !, split(Middle, Tail, Left, Right).
split(Middle, [Head|Tail], Left, [Head|Right]):-split(Middle, Tail, Left, Right).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% №11 %%  Прототипы: (i, i, i), (i, i, o), (i, o, o), (o, o, i), (o, i, o), (o, o, o)

union([],X,X):- !.
union([H|L1], L2, Res) :- member(H, L2),!, union(L1, L2, Res).
union([H|L1], L2, [H|Res]) :- union(L1, L2, Res).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

edge(a, c, 7).
edge(a, b, 3).
edge(c, d, 12).
edge(b, d, 0). 
edge(e, d, 9).

edge1(X, Y) :- edge(X, Y, _); edge(Y, X, _).

%% №16 %%  Прототипы: (i, i, i), (i, i, o) н/д

path(X, X, []):- !.
path(X, Y, []):- edge1(X, Y). 
path(X, Y, L):- edge1(Z,Y), pathl(X, [Z], L), not(member(Y, L)).

pathl(X, [Y|T], [Y|T]):- edge1(X, Y), not(member(X, T)).
pathl(X, [Y|T], L):- edge1(Y, Z), not(member(Z, T)), pathl(X,[Z,Y|T], L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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


%% №17 %%   Прототипы: (i, i, i), (i, i, o)

min_path(X, Y, L):- all_path(X, Y, L1), wsort(L1, [L|_]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

newpath([X|T], [Y, X|T]):- edge1(X, Y),not(member(Y, [X|T])). 
 
short_path1([[X|T]|_], X, [X|T]):- !.
 
short_path1([X|T], Y, Z):- findall(W, newpath(X,W), L), append(T, L, NX), short_path1(NX, Y, Z). 

%% №18 %%  Прототипы: (i, i, i), (i, i, o),

short_path(X, Y, L):- short_path1([[X]], Y, L).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% №19 %%  Прототипы: -

cyclic:- not(check_all([])).

cycle(X, L):- dif(X, Y), path(X, Y, L1), path(X, Y, L2), dif(L1, L2), append(L1, L2, L).

check_all(List):-findall(L, cycle(_, L), List).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% №20 %% Прототипы: -

is_connected:- findall(X, edge1(X, _), L), no_duble(L, L1),!, check_all_ways(L1, L1), !.

no_duble([H|T],T1):-member(H,T),no_duble(T,T1).
no_duble([H|T],[H|T1]):-not(member(H,T)),no_duble(T,T1).
no_duble([],[]).

check_all_ways([], _).
check_all_ways(L, [X|T]):- connectedWithAll(X, T), append([X], L1, L), check_all_ways(L1, T).
check_all_ways(L, [X|T]):- not(connectedWithAll(X, T)),!, check_all_ways(L, T). 

connectedWithAll(_, []).
connectedWithAll(X, [Y|T]):- path(X, Y, _), connectedWithAll(X, T).
