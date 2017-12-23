% Example graph in edge-clause form
edge(a,b).
edge(a,d).
edge(b,c).
edge(b,d)..
edge(d,c).
edge(e,f).
edge(e,g).
edge(h,nil).

% Example graph in neighbour-list clause form

%neighbour(a,[b,d]).
%neighbour(b,[a,c,d]).
%neighbour(c,[b,d]).
%neighbour(d,[a,b,c]).
%neighbour(e,[f,g]).
%neighbour(f,[e,g]).
%neighbour(g,[e,f]).
%neighbour(h,[]).


% Utility

is_edge(X,Y):- edge(X,Y); edge(Y,X).

reverse_fwd([], R, R).
reverse_fwd([H|T], Acc, R):-reverse_fwd(T, [H|Acc], R).
reverse_fwd_pretty(L, R):- reverse_fwd(L, [], R).

% list-clause to edge-clause
:- dynamic neighbour/2.

neighb_to_edge:- neighbour(Node,List),
		process(Node,List),
		fail.
neighb_to_edge.

process(Node, [H|T]):- assertz(edge(Node, H)),
		process(Node, T).
process(_, []).

% Simple path: path(Source, Target, Path)

path(X,Y,Path):-path(X,Y,[X],Path).
path(X,Y,PPath, FPath):- is_edge(X,Z),
			\+(member(Z, PPath)),% = not(memember(...))
			path(Z, Y, [Z|PPath], FPath).
path(X,X,PPath, FPath) :- reverse_fwd_pretty(PPath, FPath).
%path(X,X,PPath, PPath). -> this was replaced

% Restricted path
% restricted_path(Source, Target, RestrictionsList, Path)
% check_restrictions(RestrictionsList, Path)

restricted_path(X,Y,LR,P):- path(X,Y,P),
			check_restrictions(LR, P).

check_restrictions([],_):- !.
check_restrictions([H|T], [H|R]):- !, check_restrictions(T,R).
check_restrictions(T, [_|L]):-check_restrictions(T,L).

% Optimal path

:- dynamic sol_part/2.

optimal_path(X,Y,_):-asserta(sol_part([],100)),
		path_op(X,Y,[X],1).
optimal_path(_,_,Path):-retract(sol_part(Path,_)).

path_op(X,X,Path,LPath):-retract(sol_part(_,_)),!,
		asserta(sol_part(Path,LPath)),
		fail.
path_op(X,Y,PPath,LPath):-is_edge(X,Z),
		\+(member(Z,PPath)),
		LPath1 is LPath+1,
		sol_part(_,Lopt),
		LPath1<Lopt,
		append(PPath, [Z], NewPPath),
		path_op(Z,Y,NewPPath,LPath1).

%hamilton(NbNodes, Source, Path)
hamilton(NN, X, Path):- NN1 is NN-1, 
			hamilton_path(NN1,X, X, [X],Path).

hamilton_path(N, X, Y, PWay, Path) :- is_edge(Y,Z),
				\+(member(Z, PWay)),
				N1 is N-1,
				hamilton_path(N1,X,Z, [Z|PWay], Path).
hamilton_path(0,X,Y,PWay,[X|PWay]) :- is_edge(X,Y).

% Quiz exercises

% q9-1		

:- dynamic neighb/2.

edge_to_neighb :- is_edge(X,_),
		not(neighb(X,_)), 
		findall(Y, succ(X,Y), L),
		not(X = nil),
		assertz(neighb(X,L)),
		fail.
edge_to_neighb :- !.

succ(X,Y) :- edge(X,Z),
	Y = Z.

% q9-2 

restricted_path2(X,Y,RL,Path) :- path2(X,Y,[X],RL,Path).
path2(X,Y,PPath,[H|T],FPath) :- H = X, !,
			is_edge(X,Z),
			\+(member(Z, PPath)),
			path2(Z, Y, [Z|PPath], T, FPath).
path2(X,Y,PPath,[H|T],FPath) :- is_edge(X,Z),
			\+(member(Z, PPath)),
			path2(Z, Y, [Z|PPath], [H|T], FPath).
path2(X,X, PPath, [], FPath) :- reverse_fwd_pretty(PPath, FPath), !.
path2(X,Y, _, [], FPath) :- path(X, Y, FPath).

% q9-3

edgew(a,b,2).
edgew(a,d,1).
edgew(b,c,3).
edgew(b,d,4).
edgew(d,c,1).
edgew(e,f,6).
edgew(e,g,7).
edgew(h,nil,8).

is_edgew(X,Y,W) :- edgew(X,Y,W); edgew(Y,X,W).

:- dynamic sol_part_w/2.

optimal_pathw(X,Y,_,_):-asserta(sol_part([],100000)),
		pathw_op(X,Y,[X],0).
optimal_pathw(_,_,Path,W):-retract(sol_part(Path,W)).

pathw_op(X,X,Path,LPath):-retract(sol_part(_,_)),!,
		asserta(sol_part(Path,LPath)),
		fail.
pathw_op(X,Y,PPath,LPath):-is_edgew(X,Z,W),
		\+(member(Z,PPath)),
		LPath1 is LPath+W,
		sol_part(_,Lopt),
		LPath1<Lopt,
		append(PPath, [Z], NewPPath),
		pathw_op(Z,Y,NewPPath,LPath1).

% Problems

% p9-1
% Special thanks to Szabo Balint

cycle(X, Path):-cycle(X, [X], Path).

cycle(X, [HPP|_], [HPP]):- is_edge(HPP, X).
cycle(X, [HPP|TPP], [HPP|FP]):- is_edge(HPP, Z),
      				\+member(Z, TPP),
           			cycle(X, [Z,HPP|TPP], FP).

% p9-2

% W,G,C,F
% [n,n,n,n] -> initial state
% [s,s,s,s] -> final state

%============================ WGFC |
p_edge([n,n,n,n], [n,s,n,s]).
%============================ WC | GF
p_edge([n,s,n,s], [n,s,n,n]).
%============================ WCF | G
p_edge([n,s,n,n], [s,s,n,s]).
p_edge([n,s,n,n], [n,s,s,s]).
%============================ C | WGF
p_edge([s,s,n,s], [s,n,n,n]).
%============================ GCF | W
p_edge([s,n,n,n], [s,n,s,s]).
%============================ W | GCF
p_edge([n,s,s,s], [n,n,s,n]).
%============================ WGF | C
p_edge([n,n,s,n], [n,s,s,s]).
p_edge([n,n,s,n], [s,n,s,s]).
%============================ G | WCF
p_edge([s,n,s,s], [s,n,s,n]).
%============================ GF | WC
p_edge([s,n,s,n], [s,s,s,s]).
%============================ | WGCF

is_p_edge(X,Y) :- p_edge(X,Y) ; p_edge(Y,X).

% optimal path for problem

farm_path(X,Y,_):-asserta(sol_part([],100)),
		path_f(X,Y,[X],1).
farm_path(_,_,Path):-retract(sol_part(Path,_)).

path_f(X,X,Path,LPath):-retract(sol_part(_,_)),!,
		asserta(sol_part(Path,LPath)),
		fail.
path_f(X,Y,PPath,LPath):-is_p_edge(X,Z),
		\+(member(Z,PPath)),
		LPath1 is LPath+1,
		sol_part(_,Lopt),
		LPath1<Lopt,
		append(PPath, [Z], NewPPath),
		path_f(Z,Y,NewPPath,LPath1).










