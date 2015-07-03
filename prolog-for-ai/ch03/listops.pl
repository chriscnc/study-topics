% listops - 
member(X, [X | _]).

member(X, [_ | Tail]) :-
	member(X, Tail).


% p(EnglishWord,SpanishWord).

% concatentation
conc([], L, L). % if the first argument is the empty list then the second and third args must be the same list.

conc([X | L1], L2, [X | L3]) :-
	conc(L1,L2,L3).

% member defined in terms of concatenation
member1(X, L) :-
	conc(_, [X | _], L).

% exercise 3.1.a
% member(L1, [_,_,_], L).

% exercise 3.1.b
% member(L1, [_,_,_], L),
% member([_,_,_], L2, L1).

% exercise 3.2.a
last(Item, List) :-
	conc(_, [Item], List).

% exercise 3.2.b)
last2(Item, [Item]).

last2(Item, [_ | Tail]) :-
	last2(Item, Tail).
	
add(X,L,[X | L]).

del(X, [X | Tail], Tail).

del(X, [Y | Tail], [Y | Tail1]):-
	del(X, Tail, Tail1).


sublist(S, L) :-
	conc(_, L2, L),
	conc(S, _, L2).

insert(X, List, BiggerList) :-
	del(X, BiggerList, List).

permutation([],[]).

permutation([X | L], P) :-
	permutation(L, L1),
	insert(X, L1, P).

permutation2([],[]).

permutation2(L, [X | P]) :-
	del(X, L, L1),
	permutation2(L1, P).

% exercise 3.3
evenlength([]).
evenlength(L) :-
	conc([_,_], L1, L),
	evenlength(L1).

oddlength([_]).
oddlength(L) :-
	conc([_,_], L1, L),
	oddlength(L1).

% exercise 3.4
reverse([],[]).
reverse(List, ReversedList) :-
	conc([X], L1, List),
	conc(L2, [X], ReversedList),
	reverse(L1, L2).

% exercise 3.5
palindrome(List) :-
	reverse(List,List).

% exercise 3.6
shift([],[]).
shift(List1, List2) :-
	conc([X], L2, List1), 
	conc(L2, [X], List2).

% exercise 3.7
means(0,zero).
means(1,one).
means(2,two).
means(3,three).
means(4,four).
means(5,five).
means(6,six).
means(7,seven).
means(8,eight).
means(9,nine).
translate([],[]).
translate(List1,List2) :-
	conc([X],L1,List1),
	conc([Y],L2,List2),
	means(X,Y),
	translate(L1,L2).

% exercise 3.8 
subset([],[]).
subset([E|S1],[E|S2]) :- 
	subset(S1, S2).
subset(S1,[_|S2]) :- 
	subset(S1,S2).
	
% exercise 3.9
% dividelist(List, List1, List2)
% dividelist([a,b,c,d,e], [a,c,e], [b,d])
dividelist([],[],[]).

% exercise 3.10
% equal_length(L1,L2)
equal_length([],[]).
equal_length([_|L1], [_|L2]):-
	equal_length(L1,L2).

% exercise 3.11
% flatten(List, FlatList)
% flatten([a,b,[c,d],[],[[[e]]],f], L)
% L=[a,b,c,d,e,f]
flatten([],[]).
flatten([X|L1],[X|L2]) :-
	flatten(L1,L2).
%flatten(L1,[_|L2]) :-
%	flatten(L1,L2).
flatten([_|L1],L2) :-
	flatten(L1,L2).
