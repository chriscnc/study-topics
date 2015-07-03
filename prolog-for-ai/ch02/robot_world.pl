% robot-world

% see(Block, X, Y): Block is observed by camera at coordinates X and Y
see(a,2,5).
see(d,5,5).
see(e,5,2).

% on(Block, Object): Block is standing on Object
on(a,b).
on(b,c).
on(c,table).
on(d,table).
on(e,table).

z(B,0) :-
	on(B,table).

z(B,Z) :-
	on(B,B0),
	z(B0,Z0),
	Z is Z0 + 1.

zz(B,0) :-
	on(B,table).

zz(B, Z0+1) :-
	on(B,B0),
	zz(B0,Z0).

xy(B,X,Y) :-
	see(B,X,Y).

xy(B,X,Y) :-
	on(B0,B),
	xy(B0,X,Y).

