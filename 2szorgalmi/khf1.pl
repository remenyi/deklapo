% :- type matrix == list(row).
% :- type row == list(any).
% :- type parameter ---> subRows-subCols.
% :- type subRows == int.
% :- type subCols == int.
% :- pred feldarabolasa(+matrix, +parameter, ?list(list(any))).
% feldarabolasa(Mx, P, LL): Az LL lista az Mx mátrix P paraméterű feldarabolása.

:- use_module(library(lists)).


feldarabolasa(Mx, Rc-Cc, LL) :-
	length(Mx, Rl),
	fill(Rcl, Rl, Rc),
	
	nth1(1, Mx, R),
	length(R, Cl),
	fill(Ccl, Cl, Cc),
	
	maplist(feldarabol, Mx, LLL, Ccl),
	cum(osszevon, LLL, LLO),
	
	transpose(Mx, Mxt),
	maplist(feldarabol, Mxt, LLLt, Rcl),
	cum(osszevon, LLLt, LLTO),
	
	map_product(metszet, LLO, LLTO, LL).
	

%feldarabol(R, Rc, Rs): Az Rs összetett lista az R lista Rc földarabolása
feldarabol(R, [R], Rc) :-
	length(R, Rl),
	Rl =< Rc.
feldarabol(R, [Pref|Suffs], Rc) :-
	append_length(Pref, Suff, R, Rc),
	feldarabol(Suff, Suffs, Rc).

%cum(Pred, L, A): Az L lista elemeit a Pred predikátum szerint az A akkumulátorba processzálja
cum(_, [E], E).
cum(Pred, [H1, H2 | []], Acc) :-
	call(Pred, H1, H2, Acc).
cum(Pred, [H|T], Acc) :-
	call(Pred, H, X, Acc),
	cum(Pred, T, X).

%osszevon(L1, L2, L0): L1 és L2 összetett listákat L0-ba vonja össze
osszevon(L1, L2, LO) :-
	maplist(append, L1, L2, LO).

%metszet(L1, L2, Lo): L1 és L2 metszete Lo
metszet(L1, L2, Lo) :-
	map_product(egyenlo, L1, L2, X),
	delete(X, almafa, Lo).

%egyenlo(A, B, C): Ha A és B egyenlő, akkor megegyeznek C-vel, egyébként C almafa
egyenlo(A, A, A).
egyenlo(A, B, C) :-
	A \= B,
	C = almafa.


%fill(L, N, E): Feltölti az L listát E-beli értékekkel úgy, hogy N hosszú legyen
fill([E], 1, E).
fill([E|Ll], N, E) :-
	number(N),
	N > 1,
	Nminus is N - 1,
	fill(Ll, Nminus, E).
		
    



