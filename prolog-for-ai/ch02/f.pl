% f
f(1,one).
f(s(1),two).
f(s(s(2)),three).
f(s(s(s(X))),N) :-
	f(X,N).
