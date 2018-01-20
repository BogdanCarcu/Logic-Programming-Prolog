%1
cnt([],0).
cnt([H|T], C) :- atomic(H), !, cnt(T, C).
cnt([H|T], C) :- cnt(H, NC1), cnt(T,NC2),
    			C is NC1 + NC2 + 1.

%2
numbers([], []).
numbers([H|T], [NH|R]) :- 0 is H mod 2, !,
    				NH is H * H,
    				numbers(T, R).
numbers([H|T], [NH|R]) :- NH is H * 2,
    					numbers(T,R).

%3
to_binary(N, R) :- to_bin(N, RP), reverse(RP, R).

to_bin(0, []).
to_bin(N, [X|R]) :- X is N mod 2,
    				NN is div(N,2),
    				to_bin(NN, R).

%4
rep_all(_, LE, LE, _, []) :- var(LE), !.
rep_all(_, LS, LE, _, []) :- nonvar(LE), LS =LE, !.
rep_all(X, [H|T], LE, Y, [Y,X,Y|R]) :- X = H, !,
    					rep_all(X, T, LE, Y, R).
rep_all(X, [H|T], LE, Y, [H|R]) :- rep_all(X, T, LE, Y, R).

%5
delete_pos_even(L, X, R) :- delete_pos_even(X, L, R, 1).

delete_pos_even(X, [X|T], R, I) :- 0 is I mod 2, !,
    					I1 is I + 1,
    					delete_pos_even(X, T, R, I1).
delete_pos_even(X, [H|T], [H|R], I) :- I1 is I + 1,
    					delete_pos_even(X, T, R, I1).
delete_pos_even(_,[], [], _).

%6

divisor(N,R) :- divi(N,R,1). 

divi(N, [N], ILast) :- ILast is div(N,2).
divi(N, [I|R], I) :- 0 is N mod I, !,
    			I1 is I + 1,
    			divi(N, R, I1).
divi(N, R, I) :- I1 is I + 1,
    			divi(N, R, I1).


%7
reverse_n(N,R) :- rev(N,R,_).

rev(0, 0, 1) :- !.
rev(N, R, Multiplier) :- Q is div(N, 10),
			Rest is N mod 10,
			rev(Q, PR, Mp),
			Multiplier is Mp * 10,
			BB is Mp * Rest,
			R is PR + BB.

%8 

del_k_end([], _, 1, []).
del_k_end([_|T], K, Acc1, R) :- del_k_end(T, K, Acc, R),
				0 is Acc mod K, !,
				Acc1 is Acc + 1.
del_k_end([H|T], K, Acc1, [H|R]) :- del_k_end(T, K, Acc, R),
				Acc1 is Acc + 1.

%9
separate([], [], [], _).
separate([H|T], [H|Te], Rest, IC) :- 0 is H mod 2, 
				1 is IC mod 2, !, 
				IC1 is IC + 1,
				separate(T, Te, Rest, IC1).
separate([H|T], Even, [H|Tr], IC) :- IC1 is IC + 1,
				separate(T, Even, Tr, IC1).

separate(L, Even, Rest) :- separate(L, Even, Rest, 1).
  
%10
tree(t(26,t(14,t(2,_,_),t(15,_,_)),t(50,t(35,t(29,_,_),_),t(51,_,t(58,_,_))))).

postorder(T, _) :- var(T), !.
postorder(t(K,L,R), [K|List]) :- var(L), nonvar(R), 1 is K mod 2, !,
				postorder(R, List).
postorder(t(K,L,R), [K|List]) :- var(R), nonvar(L), !, 1 is K mod 2, !,
			postorder(L, List).
postorder(t(_,L,R), List) :- postorder(L, List1),
			postorder(R, List2),
			append(List1, List2, List), !.

%12 
tree1(t(5,t(10,t(7,nil,nil),t(10,t(4,nil,nil),t(3,nil,t(2,nil,nil)))),t(16,nil,nil))).
collect_even_from_leaf(nil, LS, LS).
collect_even_from_leaf(t(K, nil, nil), [K|LE], LE) :- 0 is K mod 2, !.
collect_even_from_leaf(t(_, L, R), LS, LE) :- collect_even_from_leaf(L, LLS, LLT),
					collect_even_from_leaf(R, LLT, LRE),
					LS = LLS,
					LE = LRE.

%14
tree2(t(26,t(14,t(2,_,_),t(15,_,_)),t(50,t(35,t(29,_,_),_),t(51,_,t(58,_,_))))).

collect_all_odd_depth(V, _, []) :- var(V), !.
collect_all_odd_depth(t(K, L, R), D, [K|Res]) :- 1 is D mod 2, !,
					D1 is D + 1,
					collect_all_odd_depth(L, D1, Res1),
					collect_all_odd_depth(R, D1, Res2),
					append(Res1, Res2, Res).
collect_all_odd_depth(t(_, L, R), D, Res) :- D1 is D + 1,
					collect_all_odd_depth(L, D1, Res1),
					collect_all_odd_depth(R, D1, Res2),
					append(Res1, Res2, Res).

%15
flatten_only_depth([], _, _, []).
flatten_only_depth([H|T], D, D, [H|R]) :- atomic(H), !,
					flatten_only_depth(T, D, D, R).
flatten_only_depth([H|T], D, CD, R) :-  atomic(H), !,
					flatten_only_depth(T, D, CD, R).
flatten_only_depth([H|T], D, CD, R) :- CD1 is CD+1, 
				flatten_only_depth(H, D, CD1, R1),
				flatten_only_depth(T, D, CD, R2),
				append(R1, R2, R).
 
flatten_only_depth(L, D, R) :- flatten_only_depth(L, D, 1, R).


%16
check(_, _, 2).
check(X, [X|T], Count) :- PCount is Count + 1,
			check(X, T, PCount), !.
check(X, [_|T], Count) :- check(X, T, Count).   

remove_dup_on_odd_pos([], _, _, []).
remove_dup_on_odd_pos([H|T], L, I, R) :- 1 is I mod 2, 
					check(H, L, 0), !,
					I1 is I + 1,
					remove_dup_on_odd_pos(T, L, I1, R).
remove_dup_on_odd_pos([H|T], L, I, [H|R]) :- I1 is I + 1,
					remove_dup_on_odd_pos(T, L, I1, R).
remove_dup_on_odd_pos(L, R) :- remove_dup_on_odd_pos(L, L, 1, R). 	

%18
tree3(t(2,t(4,t(5,_,_),t(7,_,_)),t(3,t(0,t(4,_,_),_),t(8,_,t(5,_,_))))).

max(A, B, A) :- A > B, !.
max(_, B, B).

height_each(V, -1, V) :- var(V), !.
height_each(t(_, L, R), 0, t(0, L, R)) :- var(L), var(R), !.
height_each(t(_,L,R), H, Res) :- height_each(L, H1, L1),
				height_each(R, H2, R1),
				max(H1, H2, H3),
				H is H3 + 1,
				Res = t(H, L1, R1).
				 		
%19

len_con_depth([], C, [C]) :- C \= 0, !.
len_con_depth([], _, []).

len_con_depth([H|T], C, R) :- atomic(H), !, 
				C1 is C + 1,
				len_con_depth(T, C1, R).
len_con_depth([H|T], C, [C, R1 | R2]) :- C \= 0, !,
				len_con_depth(H, 0, R1),
				len_con_depth(T, 0, R2).
len_con_depth([H|T], _, [R1 | R2]) :- len_con_depth(H, 0, R1),
				len_con_depth(T, 0, R2).

%20

transform(_, 0, []) :- !.
transform(Rep, N, [Rep|R]) :- N1 is N-1,
			transform(Rep, N1, R).  

rle_decode([], []).
rle_decode([H|T], R) :- H = [H1,H2 | []],
			transform(H1, H2, R1),
			rle_decode(T, R2),
			append(R1, R2, R).

%21
encode([], _, []).
encode([H], C, [[H, FC]]) :- FC is C + 1, !.
encode([H1,H2|T], C, R) :- H1 = H2, !,
			PC is C + 1,
			encode([H2|T], PC, R).
encode([H1, H2|T], C, R) :- PC is C + 1,
			PR = [H1, PC],
			encode([H2|T], 0, R1),
			append([PR], R1, R).

rle_encode(L, R) :- encode(L, 0, R).







