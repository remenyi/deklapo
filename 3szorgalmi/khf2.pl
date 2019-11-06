% :- type col  == int.
% :- type row  == int.
% :- type coords -->row-col.
% :- pred ertekek(sspec::in, coords::in, list(int)::out).
% ertekek(SSpec, R_C, Vals): 
% Egy érték pontosan akkor szerepel a Vals listában, ha:
%    (a) 1..k*k közötti egész, ahol k az SSpec feladvány cellamérete,
%    (b) teljesíti az adott mezőre vonatkozó szám- és paritási infók
%        által előírt megszorításokat, továbbá
%    (c) különbözik az adott mezőt tartalmazó sor, oszlop és cella többi
%        mezőjében szereplő száminfóktól, 
% ahol
%    SSpec az sspec típusspecifikációnak megfelelő Sudoku-feladvány,
%    R_C az adott feladvány egy mezőjének (sor-oszlop formában megadott) koordinátája,
%    Vals list(int) típusú mezőértéklista, az SSpec feladvány R_C koordinátájú
%         mezőjében megengedett értékek listája.

:- use_module(library(clpfd)).
:- use_module(library(lists)).

ertekek(s(K, M), R-C, Vals) :-
	nth1(R, M, Row),           %% Megfelelő sorok, oszlopok és cellák előállítása
	transpose(M, Mt),
	nth1(C, Mt, Col),
	nth1(C, Row, Cell),
	submtx(M, K, R, C, SM),

	N is K*K,                  %% Kezdeti számsorozat előállítása
	iota(Seq, 1, N, 1),
	
	szamfilter(Cell, Seq, Seq1),  %% Szűrés a cellában lévő megszorításokra
	evenfilter(Cell, Seq1, Seq2),
	oddfilter(Cell, Seq2, Seq3),

	szamlista(Row, RowNum, C),    %% Szűrés a sorok-, oszlopok- és cellákban lévő értékekre 
	deletelist(Seq3, RowNum, Seq4),
	szamlista(Col, ColNum, R),
	deletelist(Seq4, ColNum, Seq5),
	szamlista(SM, SMNum, 0),
	deletelist(Seq5, SMNum, Seq6),
	
	Vals = Seq6.

%% submtx(M, K, R, C, Out): Igaz, ha M mátrix K feldarabolásához,
%% R. sorához és C. oszlopához létezik olyan Out sorozat, hogy Out
%% elemei M mátrixnak azon celláit alkotja, amibe beleesik az R-C
%% mező, de az R-C mezőt nem tartalmazza
submtx(M, K, R, C, Out) :-
	N_R is floor((R-1)/K)*K+1,
	N_C is floor((C-1)/K)*K+1,
	darab(M, N_R, K, Rows),
	transpose(Rows, X),
	darab(X, N_C, K, Cols),
	transpose(Cols, Y),
	Rrel #= mod(R-1, K)+1,
	Crel #= mod(C-1, K)+1,
	nth1(Rrel, Y, Y1),
	nth1(Crel, Y1, D),
	delete(Y1, D, YD),
	YDL = [YD],
	delete(Y, Y1, YD1),
	append(YD1, YDL, YD1A),
	flatten(YD1A, Out).

%% flatten(L, FL): Igaz, ha Fl L lista kilapított változata
flatten([], []) :- !.
flatten([L|Ls], FlatL) :-
    flatten(Ls, X),
    append(L, X, FlatL).

%% darab(L, From, N, Out): Igaz, ha Out L lista azon részlistája,
%% amely From-al kezdődik és tőle jobbra N darab elemet tartalmaz
darab(_, _, 0, Out) :-
	Out = [].
darab(L, From, N, Out) :-
	nth1(From, L, S),
	append([S], X, Out),
	Fromplus is From+1,
	Nminus is N-1,
	darab(L, Fromplus, Nminus, X).

%% deletelist(L, D, Out): Igaz, ha Out lista tartalmazza L azon
%% elemeit, amelyek nincsenek benne D-ben
deletelist(L, [], Out) :-
	Out = L.
deletelist(L, [H|T], Out) :-
	(memberchk(H, L) ->
	    delete(L, H, X),
	    deletelist(X, T, Out)
	;   deletelist(L, T, Out)).

%% szamlista(Lin, Lout, N): Igaz, ha Lin-ben szereplő számok megvannak
%% Lout-ban, kivéve az Lin N. helyén lévő számot
szamlista([], Lout, _) :-
	Lout = [].
szamlista([H|T], Lout, N) :-
	Nminus is N-1,
	(Nminus =:= 0 ->
	    append([], X, Lout),
	    szamlista(T, X, Nminus)
	;   convlist(szam, H, Out),
	    append(Out, X, Lout),
	    szamlista(T, X, Nminus)).




%% szamfilter(Cell, Lin, Lout): Igaz, ha Lout Lin elemeit tartalmazza
%% abban az esetben ha Cell-ben nincs szám. Ellenkező esetben Cell-ben
%% található számot tartalmazza, ha az benne van az Lin listában is
szamfilter(Cell, Lin, Lout) :-
	(somechk(szam, Cell) ->
	    convlist(szam, Cell, Lnum),
	    length(Lnum, LnumL),
	    (LnumL > 1 ->
		Lout = []
	    ;   convlist(member(Lin), Lnum, Lout))
	;   Lout = Lin).

%% evenfilter(Cell, Lin, Lout): Igaz, ha Lout tartalmazza Lin páros
%% elemeit, ha Cell-ben van páros jel. Egyébként Lin minden elemét
%% tartalmazza
evenfilter(Cell, Lin, Lout) :-
	(somechk(paros, Cell) ->
	    convlist(paros, Lin, Lout)
	;   Lout = Lin).

%% oddfilter(Cell, Lin, Lout): Hasonló az evenfilter predikátumhoz,
%% csak páratlan számokkal
oddfilter(Cell, Lin, Lout) :-
	(somechk(paratlan, Cell) ->
	    convlist(paratlan, Lin, Lout)
	;   Lout = Lin).

%% szam(v(Sz), Sz): Igaz, ha Sz egy szám
szam(v(Sz)) :-
	number(Sz).
szam(v(Sz), Sz) :-
	number(Sz).

%% paros(X): Igaz, ha X páros
paros(X) :-
	X = e.
paros(X, O) :-
	0 #= mod(X, 2),
	X = O.

%% paratlan(X): Igaz, ha X páratlan
paratlan(X) :-
	X = o.
paratlan(X, O) :-
	1 #= mod(X, 2),
	X = O.

%% iota(L, F, T, V): Igaz, ha L lista F-el kezdődik és V értékkel
%% növekszik T-ig
iota(L, F, T, V) :-
	(F =:= T ->
	    L = [F]
	;F > T ->
	    L = []
	;   append(X, [T], L),
	    M is T-V,
	    iota(X, F, M, V)).

%% member(L, E, O): Igaz, ha E eleme L-nek, és L megegyezik O-val
member(L, E, O) :-
	memberchk(E, L),
	O is E.

