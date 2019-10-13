-module(khf1).
-author('remenyig@gmail.com').
-vsn('2019-10-13').
-export([feldarabolasa/2]).
%-compile(export_all).

-type row()::[any()].
-type matrix()::[row()].
-type parameter()::{subRows(), subCols()}.
-type subRows()::integer().
-type subCols()::integer().
-spec khf1:feldarabolasa(Mss::matrix(), P::parameter()) -> Lss::[[any()]].

feldarabolasa(Mss, P) ->
    L = vertikalis_feldarabolas(element(2,P), Mss),
    LL = horizontalis_osszevonas(element(1,P), L),
    lists:merge(LL).

vertikalis_feldarabolas(N, Mss) ->
    [[lists:sublist(S, E, N) || E <- lists:seq(1, length(S), N)] || S <- Mss].

horizontalis_osszevonas(M, Mss) ->
    [lists:merge(submerge(lists:sublist(Mss, E, M))) || E <-lists:seq(1, length(Mss), M)].

submerge(L) ->
    if
	(length(L) =:= 1) -> L;
	(length(L) =:= 2) -> [subsubmerge(hd(L), hd(tl(L)))];
	(length(L) > 2) -> [subsubmerge(hd(L), hd(tl(L)))| submerge(tl(tl(L)))]
    end.

subsubmerge(L1, L2) ->
    if
	(length(L1) =:= 1) and (length(L2) == 1) -> [lists:merge(hd(L1), hd(L2))];
	(length(L1) >= 2) and (length(L2) >= 2) -> [lists:merge(hd(L1), hd(L2))| (subsubmerge(tl(L1), tl(L2)))]
    end.
