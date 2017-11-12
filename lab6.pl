% Lists

list1([1,2,3,[4]]).
list2([[1],[2],[3],[4,5]]).
list3([[],2,3,4,[5,[6]],[7]]).
list4([[[[1]]],1, [1]]).
list5([1,[2],[[3]],[[[4]]],[5,[6,[7,[8,[9],10],11],12],13]]).
list6([alpha, 2,[beta],[gamma,[8]]]).

% Operations

max(A, B, Max) :- A > B, !, Max = A.
max(_, B, Max) :- Max = B.  

depth([],1).
depth([H|T],R):-atomic(H),!,depth(T,R).
depth([H|T],R):- depth(H,R1), depth(T,R2), R3 is R1+1, max(R3,R2,R).

flatten1([],[]).
flatten1([H|T], [H|R]) :- atomic(H),!, flatten1(T,R).
flatten1([H|T], R) :- flatten1(H,R1), flatten1(T,R2), append(R1,R2,R).

heads3([],[],_).
heads3([H|T],[H|R],1):-atomic(H),!,heads3(T,R,0).
heads3([H|T],R,0):-atomic(H),!,heads3(T,R,0).
heads3([H|T],R,_):-heads3(H,R1,1),heads3(T,R2,0), append(R1,R2,R).
heads(L,R) :- heads3(L,R,1).

member1(H,[H|_]).
member1(X,[H|_]):-member1(X,H).
member1(X,[_|T]):-member1(X,T).

member2(X,L):- flatten1(L,L1), member(X,L1).

% Quiz 

% 7.3.1

number_atoms([H|T], PCount, Count) :- atomic(H), !, C is PCount + 1, 
				number_atoms(T, C, Count).
number_atoms([_|T], PCount, Count) :- number_atoms(T, PCount, Count).
number_atoms([], Count, Count).
number_atomic(L, R) :- number_atoms(L, 0, R).  
				

% 7.3.2

atom_sum([H|T], PS, Sum) :- atomic(H), !, S is PS + H, 
				atom_sum(T, S, Sum).
atom_sum([_|T], PS, Sum) :- atom_sum(T, PS, Sum).
atom_sum([], Sum, Sum).
atomic_sum(L, R) :- atom_sum(L, 0, R).  
				
% 7.3.3

member_det(H,[H|_]):-!.
member_det(X,[H|_]):-member_det(X,H), !.
member_det(X,[_|T]):-member_det(X,T).

% Problems

% 7.4.1

tails([], []). 
tails([H], [H]) :- atomic(H), !. 
tails([H|T], R) :- atomic(H), !, tails(T, R).
tails([H|T], R) :- tails(H, R1), tails(T, R2), append(R1, R2, R).


% 7.4.2 

rep(_, _, [], []) :- !.
rep(K, NewK, [K|T], [NewK|T]) :- !.
rep(K, NewK, [H|T], [H|R]) :- atomic(H), !, rep(K, NewK, T, R).
rep(K, NewK, [H|T], [R1|T]) :- rep(K, NewK, H, R1). 

% 7.4.3 ** Maybe too complicated :(

smaller(L1, L2) :- depth(L1, D1), 
			depth(L2, D2),
			D1 < D2, !.

eq_depth(L1, L2) :- depth(L1, D1), depth(L2, D2), D1 = D2, !.

smaller_nmb([H|T1], [H|T2]) :- smaller_nmb(T1, T2).
smaller_nmb([H1|_], [H2|_]) :- H1 < H2, !.
smaller_nmb([], _).

smaller_list(L1, L2) :- eq_depth(L1, L2), !, smaller_nmb(L1, L2), !.
smaller_list(L1, L2) :- smaller(L1, L2), !.
smaller_list([], _).

list_sort([H|T], R) :- partition(H, T, Sm, Lg), list_sort(Sm, SmS),
		      list_sort(Lg, LgS), append(SmS, [H|LgS], R).
list_sort([], []).

partition(H, [X|T], [X|Sm], Lg) :- number(X), number(H), !, X < H, !,
				  partition(H, T, Sm, Lg).
partition(H, [X|T], [X|Sm], Lg) :- smaller_list(X, H), !, partition(H, T, Sm, Lg).
partition(H, [X|T], Sm, [X|Lg]) :- partition(H, T, Sm, Lg), !.
partition(_, [], [], []).























