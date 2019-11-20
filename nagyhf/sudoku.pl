% :- type sspec ---> s(size, board).
% :- type size  == int.
% :- type field == list(info).
% :- type info ---> e; o; s; w; v(int).
% :- type board == list(list(field)).

% :- type ssol == list(list(int)).

% sudoku(SSpec, SSol):
% SSol az SSpec feladványt kielégítő megoldás.
% :- pred sudoku(sspec::in, ssol::out).

:- use_module(library(lists)).

sudoku(s(N, M), SSol) :-
	Size is N*N,
	length(Rows, Size),
	maplist(neo_length(Size), Rows),
	maplist(fill(Size), Rows),
	SSol = Rows,
	%%maplist(all_distinct, SSol),
	transpose(SSol, SSolT),
	maplist(all_distinct, SSolT),
	celldistinct(SSol, N),
	
	parity(M, Mp),
	numbers(M, Mn),
	wdirections(M, Mw),
	sdirections(M, Ms),
	maplist(paritycheck, Mp, SSol),
	maplist(numbercheck, Mn, SSol),
	maplist(wdirectioncheck, Mw, SSol),
	scheck(Ms, SSol).

neo_length(N, L) :-
	length(L, N).

all_distinct([]).
all_distinct([H|T]) :-
	\+member(H, T),
	all_distinct(T).

fill(N, Row) :-
	iota(X, 0, N),
	permutation(X, Row).

iota([], F, F).
iota(L, F, T) :-
	append(X, [T], L),
	M is T-1,
	iota(X, F, M), !.

celldistinct([[]|_], _).
celldistinct([], _).
celldistinct(SSol, N) :-
	prefix_length(SSol, Pref, N),
	maplist(neo_prefix_length(N), Pref, Cell),
	flatten(Cell, CellF),
	all_distinct(CellF),

	maplist(subseq, Pref, Cell, CellR),
	celldistinct(CellR, N),

	subseq(SSol, Pref, SSolR),
	celldistinct(SSolR, N).

flatten([], []) :- !.
flatten([L|Ls], FlatL) :-
    flatten(Ls, X),
    append(L, X, FlatL).

neo_prefix_length(Length, List, Prefix) :-
	prefix_length(List, Prefix, Length).

scheck(Ms, SSol) :-
	transpose(SSol, SSolT),
	transpose(Ms, MsT),
	maplist(reverse, SSolT, SSolTR),
	maplist(reverse, MsT, MsTR),
	maplist(wdirectioncheck, MsTR, SSolTR).

wdirectioncheck([_], [_]).
wdirectioncheck([_, HM2|TM], [HS1, HS2|TS]) :-
	SUM is HS1 + HS2,
	(HM2 = [w] ->
	    1 is mod(SUM, 2)
	;HM2 = [s] ->
	    1 is mod(SUM, 2)
	;true),
	wdirectioncheck([HM2|TM], [HS2|TS]).

numbercheck(MnL, SSolL) :-
	maplist(cellnumber, MnL, SSolL).

cellnumber([], _).
cellnumber(MpC, SSolC) :-
	nth1(1, MpC, Num),
	Num = SSolC.

paritycheck(MpL, SSolL) :-
	maplist(cellparity, MpL, SSolL).

cellparity([], _).
cellparity(MpC, SSolC) :-
	(memberchk(e, MpC) ->
	    0 is mod(SSolC, 2)
	;memberchk(o, MpC) ->
	    1 is mod(SSolC, 2)).

sdirections(M, Mw) :-
	convlist(sdirlist, M, Mw).

sdirlist(Lin, Lout) :-
	convlist(sdircell, Lin, Lout).

sdircell(Cin, Cout) :-
	(somechk(deli, Cin) ->
	    Cout = [s]
	;Cout = []).

deli(s).

wdirections(M, Mw) :-
	convlist(wdirlist, M, Mw).

wdirlist(Lin, Lout) :-
	convlist(wdircell, Lin, Lout).

wdircell(Cin, Cout) :-
	(somechk(nyugati, Cin) ->
	    Cout = [w]
	;Cout = []).

nyugati(w).

numbers(M, Mn) :-
	convlist(numberlist, M, Mn).

numberlist(Lin, Lout)  :-
	convlist(numbercell, Lin, Lout).

numbercell(Cin, Cout) :-
	(somechk(szam, Cin) ->
	    convlist(szam, Cin, Cout)
	;Cout = []).

szam(v(_)).
szam(v(I), O) :-
	O is I.

parity(M, Mp) :-
	convlist(paritylist, M, Mp).

paritylist(Lin, Lout) :-
	convlist(paritycell, Lin, Lout).

paritycell(Cin, Cout) :-
	(somechk(paros, Cin) ->
	    Cout = [e]
	;somechk(paratlan, Cin) ->
	    Cout = [o]
	;Cout = []).

paros(e).

paratlan(o).