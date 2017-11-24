% incomplete lists
member_il(_, L):-var(L), !, fail.
member_il(X, [X|_]):-!.
member_il(X, [_|T]):-member_il(X, T).

insert_il(X, L):-var(L), !, L=[X|_]. %found end of list, add element
insert_il(X, [X|_]):-!. %found element, stop
insert_il(X, [_|T]):- insert_il(X, T). % traverse input list to reach end/X

delete_il(_, L, L):-var(L), !. % reached end, stop
delete_il(X, [X|T], T):-!. % found element, remove it and stop
delete_il(X, [H|T], [H|R]):-delete_il(X, T, R). % search for the element

% incomplete trees
search_it(_, T):-var(T), !, fail.
search_it(Key, t(Key, _, _)):-!.
search_it(Key, t(K, L, _)):-Key<K, !, search_it(Key, L).
search_it(Key, t(_, _, R)):-search_it(Key, R).

insert_it(Key, t(Key, _, _)):-!.
insert_it(Key, t(K, L, _)):-Key<K, !, insert_it(Key, L).
insert_it(Key, t(_, _, R)):- insert_it(Key, R).

delete_it(Key, T, T):-var(T), !, write(Key), write(' not in tree\n').
delete_it(Key, t(Key, L, R), L):-var(R), !.
delete_it(Key, t(Key, L, R), R):-var(L), !.
delete_it(Key, t(Key, L, R), t(Pred, NL, R)):-!, get_pred(L, Pred, NL).
delete_it(Key, t(K, L, R), t(K, NL, R)):-Key<K, !, delete_it(Key, L, NL).
delete_it(Key, t(K, L, R), t(K, L, NR)):- delete_it(Key, R, NR).
get_pred(t(Pred, L, R), Pred, L):-var(R), !.
get_pred(t(Key, L, R), Pred, t(Key, L, NR)):-get_pred(R, Pred, NR).


	% quiz exercises

% q.3.1

append_il(L1, L2, L1):-var(L1), !, L1 = L2.
append_il([H|T], L2, [H|R]) :- append_il(T, L2, R).

% q.3.2

reverse_il(L, R, F) :- var(L), !, append(R, _, F).
reverse_il([H|T], Acc, R):-reverse_il(T, [H|Acc], R).
			
% q.3.3

transform_il(L, []) :- var(L), !.
transform_il([H|T], [H|R]) :- transform_il(T, R).

% bst to use
tree1(t(6, t(4, t(2, _, _), t(5, _, _)), t(9, t(7, _, _), _))). 

% q.3.4

preorder(T, []) :- var(T), !.
preorder(t(K,L,R), List):- preorder(L,LL),
			 preorder(R, LR),
			append([K|LL], LR, List).

% q.3.5

max(A, B, A):- A>B, !.
max(_, B, B).

height_it(T, 0) :- var(T), !.
height_it(t(_, L, R), H):-height_it(L, H1), height_it(R, H2), max(H1, H2, H3),
		H is H3+1. 

% q.3.6

transform_it(T, nil):- var(T), !.
transform_it(t(K,L,R), t(K,TL,TR)):- transform_it(L, TL),
			 transform_it(R, TR), !.
transform_it(T, T):- !.

	
	% problems

% p.4.1

flat_il(L, L):- var(L), !.
flat_il([H|T], [H|R]) :- atomic(H),!, flat_il(T,R).
flat_il([H|T], R) :- flat_il(H,R1), flat_il(T,R2), append(R1,R2,R), !.


% p.4.2

max3(A, B, C, MAX) :- max(A, B, PR), max(PR, C, MAX).
diam_it(L, 0) :- var(L), !. 
diam_it(t(_, L, R), D) :- diam_it(L, D1), diam_it(R, D2), 
		height_it(L, LH), height_it(R, RH), SumH is LH + RH + 1,
		max3(D1, D2, SumH, D).

% p.4.3

subl(L, _, _) :- var(L), !.
subl(_, _, L) :- var(L), !, fail.
subl([H|T1], PermList, [H|T2]) :- subl(T1, PermList, T2), !.
subl(_, PermList, [_|T]) :- subl(PermList, PermList, T).

subl_il(X, L) :- subl(X, X, L). 
	

















