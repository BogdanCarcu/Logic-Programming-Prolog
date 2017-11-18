%test cases
	% binary tree
tree1(t(6, t(4, t(2, nil, nil), t(5, nil, nil)), t(9, t(7, nil, nil), nil))).
tree2(t(8, t(5, nil, t(7, nil, nil)), t(9, nil, t(11, nil, nil)))). 
	% ternary tree
tree3(t(6, t(4, t(2, nil, nil, nil), nil, t(7, nil, nil, nil)), t(5, nil, nil, nil), t(9, nil, nil, t(3, nil, nil, nil)))).

%inorder 
inorder(t(K,L,R), List):-inorder(L,LL), inorder(R, LR),
append(LL, [K|LR],List).
inorder(nil, []).

%preorder
preorder(t(K,L,R), List):-preorder(L,LL), preorder(R, LR),
append([K|LL], LR, List).
preorder(nil, []). 

%postorder
postorder(t(K,L,R), List):-postorder(L,LL), postorder(R, LR),
append(LL, LR,R1), append(R1, [K], List).
postorder(nil, []). 
 

% inorder traversal
pretty_print(nil, _).
pretty_print(t(K,L,R), D):-D1 is D+1, pretty_print(L, D1), print_key(K, D),
 pretty_print(R, D1).
% predicate which prints key K at D tabs from the screen left margin and then
% proceeds to a new line
print_key(K, D):-D>0, !, D1 is D-1, write('\t'), print_key(K, D1).
print_key(K, _):-write(K), nl. 

%search
search_key(Key, t(Key, _, _)):-!.
search_key(Key, t(K, L, _)):-Key<K, !, search_key(Key, L).
search_key(Key, t(_, _, R)):-search_key(Key, R). 

%insert
insert_key(Key, nil, t(Key, nil, nil)):-write('Inserted '), write(Key), nl.
insert_key(Key, t(Key, L, R), t(Key, L, R)):-!, write('Key already in tree\n').
insert_key(Key, t(K, L, R), t(K, NL, R)):-Key<K, !, insert_key(Key, L, NL).
insert_key(Key, t(K, L, R), t(K, L, NR)):- insert_key(Key, R, NR). 


%delete
delete_key(Key, nil, nil):-write(Key), write(' not in tree\n').
delete_key(Key, t(Key, L, nil), L):-!. % this clause covers also case for leaf (L=nil)
delete_key(Key, t(Key, nil, R), R):-!.
delete_key(Key, t(Key, L, R), t(Pred, NL, R)):-!, get_pred(L, Pred, NL).
delete_key(Key, t(K, L, R), t(K, NL, R)):-Key<K, !, delete_key(Key, L, NL).
delete_key(Key, t(K, L, R), t(K, L, NR)):- delete_key(Key, R, NR). 
get_pred(t(Pred, L, nil), Pred, L):-!.
get_pred(t(Key, L, R), Pred, t(Key, L, NR)):-get_pred(R, Pred, NR). 

%height
% predicate which computes the maximum between 2 numbers
max(A, B, A):-A>B, !.
	 max(_, B, B).
% predicate which computes the height of a binary tree
height(nil, 0).
height(t(_, L, R), H):-height(L, H1), height(R, H2), max(H1, H2, H3),
		H is H3+1. 

% quiz
% q8-1
inorder1(t(K,L,R)):-inorder1(L), write(K), write(' '), inorder1(R).
inorder1(nil).

% q8-2
delete_key1(Key, nil, nil):-write(Key), write(' not in tree\n').
delete_key1(Key, t(Key, L, nil), L):-!. % this clause covers also case for leaf (L=nil)
delete_key1(Key, t(Key, nil, R), R):-!.
delete_key1(Key, t(Key, L, R), NR) :- hang(L, R, NR).
delete_key1(Key, t(K, L, R), t(K, NL, R)):-Key<K, !, delete_key1(Key, L, NL).
delete_key1(Key, t(K, L, R), t(K, L, NR)):- delete_key1(Key, R, NR). 

hang(LT, t(K, nil, R), t(K, LT, R)).
hang(_, t(K, L, R), t(K, NL, R)) :- hang(_, L, NL).  

% q8-3
collect_leaves(nil, L, L).
collect_leaves(t(Key, nil, nil), PR, R) :- append(PR, [Key], R), !.
collect_leaves(t(_,L,R), PList, List):-collect_leaves(L, PList, PList1), collect_leaves(R, PList1, PList2), 
					List = PList2.

% p8-1

max3(A, B, C, MAX) :- max(A, B, PR), max(PR, C, MAX).
diam(t(_, L, R), D) :- diam(L, D1), diam(R, D2), 
		height(L, LH), height(R, RH), SumH is LH + RH + 1,
		max3(D1, D2, SumH, D).
diam(nil, 0). 


% p8-2

collect_depth(nil, 0, List, List).
collect_depth(t(K, _, _, _), 0, PL, List) :- append(PL, [K], List), !.
collect_depth(t(_, L, M, R), Depth, PL, List) :- Deeper is Depth - 1, 
					collect_depth(L, Deeper, PL, List1),
					collect_depth(M, Deeper, List1, List2),
					collect_depth(R, Deeper, List2, List).  
collect_by_depth(t(K, L, M, R), Depth, Result) :- collect_depth(t(K, L, M, R), Depth, [], Result).

% p8-3

mirror(nil, nil).
mirror(t(_, L1, R1), t(_, L2, R2)) :- mirror(L1, R2), mirror(L2, R1).

symmetric(nil).
symmetric(t(_, L, R)) :- mirror(L, R).









