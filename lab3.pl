member1(X, [H|_]) :- X = H.
member1(X, [_|T]) :- member1(X, T).

append1([], L, L).
append1([H|T], L, [H|R]) :- append1(T, L, R).

delete1(X, [X|T], T).
delete1(X, [H|T], [H|R]) :- delete1(X, T, R).
delete1(_,[],[]).

delete_all(X, [X|T], R) :- delete_all(X, T, R).
delete_all(X, [H|T], [H|R]) :- delete_all(X, T, R).
delete_all(_,[],[]).

append3(L1, L2, L3, R) :- append1(L1, L2, PR1),
			append1(PR1, L3, R).

append_el(X, L, R) :- R = [X|L].

sum_l([], 0).
sum_l([H|T], SUM) :- sum_l(T, PR),
		SUM is PR + H.

separate_parity1([],[],[]).
separate_parity1([H|T], [H|E], O) :- 0 is H mod 2, !,
				separate_parity1(T, E, O).
separate_parity1([H|T], E, [H|O]) :- 
				separate_parity1(T, E, O).

%Remove duplicates:

remove_dup([], []).
remove_dup([H|T], [H|R]) :- delete_all(H, T, PR),
			remove_dup(PR, R).

%Replace all occurences of an element K with NewK
replace_all(_,_,[],[]).
replace_all(K,K,L,L).
replace_all(K, NewK, [K|T], R) :- replace_all(K, NewK, T, PR),
				R = [NewK|PR].
replace_all(K, NewK, [H|T], R) :- replace_all(K, NewK, T, PR),
				R = [H|PR].


%Deletes every Kth element
drop_k1([],_,_,[]).
drop_k1([_|T], K, Count, R) :- 0 is Count mod K,
			       NewCount is Count + 1,
			       drop_k1(T, K, NewCount, R).
drop_k1([H|T], K, Count, R) :- NewCount is Count + 1,
			       drop_k1(T, K, NewCount, PR),
			       R = [H|PR].
drop_k(L, K, R) :- drop_k1(L, K, 1, R).












