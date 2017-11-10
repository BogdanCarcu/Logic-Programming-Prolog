%women
woman(dorina).
woman(maria).
woman(ana).
woman(irina).
woman(carmen).
woman(sara).
woman(ema).
%men
man(sergiu).
man(marius).
man(mihai).
man(george).
man(andrei).
man(alex).
%relationships
parent(sergiu, mihai). %sergiu is the parent of mihai
parent(marius, maria).
parent(dorina, maria).
parent(maria, ana).
parent(maria, andrei).
parent(mihai, george).
parent(mihai, carmen).
parent(george, ana).
parent(george, andrei).
parent(irina, george).
parent(irina, carmen).
parent(carmen, sara).
parent(carmen, ema).
parent(alex, sara).
parent(alex, ema).
%............................
mother(X, Y) :- woman(X), parent(X, Y).
father(X, Y) :- man(X), parent(X, Y).
sibling(X, Y) :- parent(Z, X), parent(Z, Y), X \= Y.
sister(X, Y) :- sibling(X, Y), woman(X).
brother(X, Y) :- sibling(X, Y), man(X).
aunt(X, Y) :- sister(X, Z), parent(Z, Y).
uncle(X, Y) :- brother(X, Z), parent(Z, Y).
grandmother(X, Y) :- mother(X, Z), parent(Z, Y).
grandfather(X, Y) :- father(X, Z), parent(Z, Y).
%ancestor
ancestor(X, Y) :- parent(X, Y).
ancestor(X, Y) :- parent(Z, Y), ancestor(X, Z).