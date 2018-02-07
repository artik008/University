qusort([],[]).
qusort([Head|Tail], SortedList):-split(Head, Tail, Left, Right), 
qusort(Left, SortedLeft), qusort(Right, SortedRight),
append(SortedLeft, [Head|SortedRight], SortedList).

split(_, [], [], []).
split(Middle, [Head|Tail], [Head|Left], Right):- (Head =< Middle), !, split(Middle, Tail, Left, Right).
split(Middle, [Head|Tail], Left, [Head|Right]):-split(Middle, Tail, Left, Right).


qusort([2,5,7,3,9,1,4,8], X).