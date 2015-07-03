% family
parent(pam,bob).
parent(tom,bob).
parent(tom,liz).
parent(bob,ann).
parent(bob,pat).
parent(pat,jim).

female(pam).
female(liz).
female(pat).
female(ann).

male(tom).
male(bob).
male(jim).

mother(X,Y) :- 
	parent(X,Y),
	female(X).

grandparent(X,Z) :-
	parent(X,Y),
	parent(Y,Z).

grandchild(X,Z) :-
	parent(Y,X),
	parent(Z,Y).

sister(X,Y) :-
	parent(Z,X),
	parent(Z,Y),
	female(X),
	X \= Y.

happy(X) :-
	parent(X,Y).

hastwochildren(X) :-
	parent(X,Y),
	sister(Y,Z).

aunt(X,Y) :-
	sister(Z,X),
	parent(Z,Y),
	female(X).

ancestor(X,Z) :-
	parent(X,Z).

ancestor(X,Z) :-
	parent(X,Y),		% there exists a Y such that X is a parent of Y
	ancestor(Y,Z).  % y is an ancestor of Z




