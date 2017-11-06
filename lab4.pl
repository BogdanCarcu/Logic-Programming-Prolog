member1(X, [X|_]) :- !.
member1(X, [_|T]) :- member1(X, T).

delete1(_, [], []).
delete1(X, [X|T], T) :- !.
delete1(X, [H|T], [H|R]) :- delete1(X, T, R).

minimum([], M, M).
minimum([H|T], MP, M):-H<MP, !, minimum(T, H, M).
minimum([_|T], MP, M):-minimum(T, MP, M).
minimum_pretty([H|T], R):-minimum([H|T], H, R). 

max([], M, M).
max([H|T], MP, M):-H>MP, !, max(T, H, M).
max([_|T], MP, M):-max(T, MP, M).
max_pretty([H|T], R):-max([H|T], H, R). 

union([ ],L,L).
union([H|T],L2,R) :- member(H,L2),!,union(T,L2,R).
union([H|T],L,[H|R]):-union(T,L,R).

% Quizz 

% q4-1 and q4-3
del_min(L,R) :- minimum_pretty(L, M), delete1(M, L, R).
del_max(L,R) :- max_pretty(L, M), delete1(M, L, R).

reverse_fwd([], R, R).
reverse_fwd([H|T], Acc, R):-reverse_fwd(T, [H|Acc], R).
reverse_fwd_pretty(L, R):- reverse_fwd(L, [], R).

% q4-2
reverse_k(K, [H|T], [H|R], Count) :- CountNew is Count + 1,
			     reverse_k(K, T, R, CountNew).
reverse_k(K, [H|T], [H|R], K) :- reverse_fwd_pretty(T, R).
reverse_k_pretty(K, L, R) :- reverse_k(K, L, R, 0).  

% Problems

%1

rle([], _, _, []).
rle([H|[]], Count, PR, R) :- append([H], [Count], Grow),
			append(PR, [Grow], R).
rle([H,H|T], Count, PR, R) :- !, NewCount is Count + 1,
			rle([H|T], NewCount, PR, R).
rle([H1,H2|T], Count, PR, R) :- append([H1], [Count], Grow),
			append(PR, [Grow], FR), 
			rle([H2|T], 1, FR, R).
rle_encode(L, R) :- rle(L, 1, [], R).

%2

split([], _, [],[]).
split([H|L], K, [H|L1], L2):- NewK is K - 1,
	NewK >= 0,
        !,    
        split(L, NewK, L1, L2).

split([H|L], K, L1, [H|L2]):- split(L, K, L1, L2).

rotate_right(L, K, R) :- length(L, Len),
			Rest is Len - mod(K, Len),
			split(L, Rest, L1, L2),
			append(L2, L1, R). 


%3 
extract_rnd(_, R, 0, R).
extract_rnd(L, NewList, K, R) :- NewK is K-1,
			NewK >= 0,
    			!,
			length(L, Len),
			Rnd is random(Len),
			nth0(Rnd, L, Element),
			append(NewList, [Element], TempList),
			extract_rnd(L, TempList, NewK, R).
rnd_select(L, K, R) :- extract_rnd(L, [], K, R).			























 
