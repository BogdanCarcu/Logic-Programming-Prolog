%Difference lists

add(X,LS,LE,RS,RE):-RS=LS,LE=[X|RE].

%test cases
	% binary tree
tree1(t(6, t(4, t(2, nil, nil), t(5, nil, nil)), t(9, t(7, nil, nil), nil))).
tree2(t(8, t(5, nil, t(7, nil, nil)), t(9, nil, t(11, nil, nil)))).
tree_inc(t(6, t(4, t(2, _, _), t(5, _, _)), t(9, t(7, _, _), _))). 

inorder_dl(nil,L,L).
inorder_dl(t(K,L,R),LS,LE):-inorder_dl(L,LS,[K|LT]), 
    			inorder_dl(R,LT,LE).

preorder_dl(nil,L,L).
preorder_dl(t(K,L,R),LS,LE):- preorder_dl(L,LSL,LEL),
                    preorder_dl(R,LSR,LER),
                    LS=[K|LSL],
                    LEL=LSR,
                    LE=LER.

postorder_dl(nil,L,L).
postorder_dl(t(K,L,R),LS,LE):- postorder_dl(L,LSL,LEL),
                    postorder_dl(R,LSR,LER),
                    LEL=LSR,
    		    LSL=LS,
    		    LER=[K|LE].
	            %just like add(K,LSL,LER,LS,LE).
    				
partition(H, [X|T], [X|Sm], Lg):-X<H, !, partition(H, T, Sm, Lg).
partition(H, [X|T], Sm, [X|Lg]):- partition(H, T, Sm, Lg).
partition(_, [], [], []).

quicksort_dl([H|T],S,E):- partition(H,T,Sm,Lg),
                    quicksort_dl(Sm,S,[H|L]),
                    quicksort_dl(Lg,L,E).
quicksort_dl([],L,L).

%Side effects
:-dynamic memo_fib/2.
fib(N,F):-memo_fib(N,F),!.
fib(N,F):- N>1,
		N1 is N-1,
		N2 is N-2,
		fib(N1,F1),
		fib(N2,F2),
		F is F1+F2,
		assertz(memo_fib(N,F)).
fib(0,1).
fib(1,1).

print_all:-memo_fib(N,F),
	write(N),
	write(‘ - ‘),
	write(F),
	nl,
	fail.
print_all.

perm(L, [H|R]):-append(A, [H|T], L), append(A, T, L1), perm(L1, R).
perm([], []).

all_perm(L,_):-perm(L,L1),
			assertz(p(L1)),
			fail.
all_perm(_,R):-collect_perms(R).

collect_perms([L1|R]):-retract(p(L1)), !,
                  collect_perms(R).
collect_perms([]).
 
%Quiz exercises

%4.1

to_diff(L,LS,LS):- var(L), !.
to_diff([H|T], [H|LS], LE):- to_diff(T, LS, LE).

to_inc(LS,LE,R) :- append(LS, LE, R1),
		append(R1, _, R), !.

%4.2

complete2diff([], RS, RS).
complete2diff([H|T], [H|RS], RE) :- complete2diff(T, RS, RE).

diff2complete(LS, LE, R) :- not(var(LE)), !, append(LS, LE, R). 
diff2complete(LS, [], LS).

%4.3

decomposition(L, D1, D2) :- append(D1, D2, L).

all_decompositions(L,_) :- decomposition(L, D1, D2),
			   assertz(dec(D1,D2)),
			   fail.
all_decompositions(_,R) :- collect_decompositions(R).

collect_decompositions([[D1,D2]|R]):-retract(dec(D1,D2)), !,
				collect_decompositions(R).
collect_decompositions([]).

%Problems

%5.1

flat_il([], L, L).
flat_il([H|T], [H|RS], RE) :- atomic(H),!, flat_il(T,RS,RE).
flat_il([H|T], RS, RE) :- flat_il(H, RS1, RE1), flat_il(T,RS2,RE2), 
			RS = RS1,
			RE1 = RS2,
			RE = RE2.
flat_dl(L, R) :- flat_il(L, RS, RL), diff2complete(RS, RL, R).
			
%5.2

collect_even(nil,L,L). 
collect_even(t(K,L,R),LS,LE):- collect_even(L,LSL,LEL),
			collect_even(R,LSR,LER),
			LS=LSL,
			1 is (K mod 2), !,
			LEL=[K|LSR],
			LE=LER.
collect_even(t(_,L,R),LS,LE):- collect_even(L,LSL,LEL),
			collect_even(R,LSR,LER),
			LS=LSL,
			LEL=LSR,
			LE=LER.
%5.3
collect_keys(V,L,L,_,_) :- var(V), !.
collect_keys(t(K,L,R),LS,LE,K1,K2):-collect_keys(L,LSL,LEL,K1,K2),
			collect_keys(R,LSR,LER,K1,K2),
			LS=LSL,
			K1 < K, 
			K < K2, !,
			LEL=[K|LSR],
			LE=LER.
collect_keys(t(_,L,R),LS,LE,K1,K2):-collect_keys(L,LSL,LEL,K1,K2),
			collect_keys(R,LSR,LER,K1,K2),
			LS=LSL,
			LEL=LSR,
			LE=LER.
















