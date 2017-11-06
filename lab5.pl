delete2(X, [X|T], T):-!.
delete2(X, [H|T], [H|R]):-delete2(X, T, R).
delete2(_, [], []).

perm(L, [H|R]) :- append(A, [H|T], L), append(A, T, L1), perm(L1, R).
perm([], []).

is_ordered([_]).
is_ordered([H1, H2|T]):-H1 =< H2, is_ordered([H2|T]).

perm_sort(L, R):-perm(L,R), is_ordered(R), !.

minimum([], M, M).
minimum([H|T], MP, M):-H<MP, !, minimum(T, H, M).
minimum([_|T], MP, M):-minimum(T, MP, M).
minimum_pretty([H|T], R):-minimum([H|T], H, R). 

sel_sort(L, [M|R]) :- minimum_pretty(L, M), delete2(M, L, L1), sel_sort(L1, R).
sel_sort([],[]).


%----------------------------------------------

% Q1

delete1(H, [H|T], T).
delete1(X, [H|T], [H|R]) :- delete1(X, T, R).

perm1([],[]).
perm1(L, [A|R]) :- delete1(A, L, L1), perm1(L1, R).	
perm1_sort(L, R):-perm1(L,R), is_ordered(R), !.


% Q2

max([], M, M).
max([H|T], MP, M):-H>MP, !, max(T, H, M).
max([_|T], MP, M):-max(T, MP, M).
maxim_p([H|T], M) :- max([H|T], H, M).

sel_sort1(L, R):- maxim_p(L, M), delete2(M, L, L1), sel_sort1(L1, PR), append(PR, [M], R).
sel_sort1([], []). 

% Q3

ins_sort1([H|T], PR, R):- insert_ord(H, PR, PR2), ins_sort1(T, PR2, R). 
ins_sort1([], R, R).
ins_sort_pretty(L, R) :- ins_sort1(L, [], R).

insert_ord(X, [H|T], [H|R]):-X>H, !, insert_ord(X, T, R).
insert_ord(X, T, [X|T]).  


% Q4

one_pass([H1, H2|T], [H2|R]):- H1>H2, !, one_pass([H1|T], R).
one_pass([H1|T], [H1|R]):- one_pass(T, R).
one_pass([], []).

bubble_sort(L, R, Len) :- Len > 0, !, 
		one_pass(L, R1), 
		LenNew is Len - 1, 
		bubble_sort(R1, R, LenNew).
bubble_sort(L, L, _).

pretty_bubble_sort(L, R) :- length(L, Len), bubble_sort(L, R, Len).

% P1

sort_chars([H|T], R):-partition1(H, T, Sm, Lg), sort_chars(Sm, SmS),
sort_chars(Lg, LgS), append(SmS, [H|LgS], R).
sort_chars([], []).

partition1(H, [X|T], [X|Sm], Lg):-char_code(X, Xn),
				char_code(H, Hn),
				Xn < Hn, !, 
				partition1(H, T, Sm, Lg).
partition1(H, [X|T], Sm, [X|Lg]):-partition1(H, T, Sm, Lg).
partition1(_, [], [], []).

% P2

sort_len([H|T], R):-partition2(H, T, Sm, Lg), sort_len(Sm, SmS),
sort_len(Lg, LgS), append(SmS, [H|LgS], R).
sort_len([], []).

partition2(H, [X|T], [X|Sm], Lg):-length(X, Xlen),
				length(H, Hlen),
				Xlen < Hlen, !, 
				partition2(H, T, Sm, Lg).
partition2(H, [X|T], Sm, [X|Lg]):-partition2(H, T, Sm, Lg).
partition2(_, [], [], []).






























