% one quiz exercise Repeat..until
until(X, X).
until(X, Y) :- repeat_print(X, Y).  
repeat_print(LOW, HIGH) :- print(LOW),
			INC is LOW + 1,
			until(INC, HIGH).

% 2.3.1 
is_triangle(A, B, C) :- V1 is A + B,
		V2 is B + C,
		V3 is A + C,
		V1 > C,
		V2 > A,
		V3 > B.


% 2.3.2
solve_eq(A, B, C, X1, X2) :- DELTA is B*B - 4*A*C, 
			      DELTA >= 0, 
			      X1 is (-1 * B + sqrt(DELTA)) / (2*A),
			      X2 is (-1 * B - sqrt(DELTA)) / (2*A).