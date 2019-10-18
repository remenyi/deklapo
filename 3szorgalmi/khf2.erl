-module(khf2).
-author('remenyig@gmail.com').
-vsn('2019-10-18').
-export([ertekek/2]).
%-compile(export_all).

-type sspec() :: {size(), board()}.
-type size()  :: integer().
-type field() :: [info()].
-type info()  :: e | o | s | w | integer().
-type board() :: [[field()]].
-type col() :: integer().
-type row() :: integer().
-type coords() :: {row(),col()}.
-spec khf2:ertekek(SSpec :: sspec(), R_C :: coords()) -> Vals :: [integer()].
%% Egy érték pontosan akkor szerepel a Vals listában, ha teljesíti a
%% fenti Prolog specifikációban felsorolt (a), (b) és (c) feltételeket, ahol
%% Vals az SSpec specifikációval megadott Sudoku-feladvány R_C
%% koordinátájú mezőjében megengedett értékek listája.

ertekek(SSpec, R_C) -> 
    C = element(1, SSpec), SMat = element(2, SSpec),
    N = element(1, R_C), M = element(2, R_C),

    M_L = lists:nth(M, lists:nth(N, SMat)),                               % megszorítások kiválasztása
    S_L = lists:flatten(lists:delete(M_L, lists:nth(N, SMat))),             % sor kiválasztása
    O_L = lists:flatten(lists:delete(M_L, [lists:nth(M, S) || S <- SMat])), % oszlop kiválasztása
    C_L = lists:flatten(lists:delete(M_L, cella(N, M, SMat, C))),           % cella kiválasztása

    [X || X <- lists:seq(1, C*C),                        % lehetséges számok
	  paritas_szuro(X, M_L),                         % paritás megszorítás
	  szam_szuro(X, M_L),                            % szám megszorítás
	  not lists:any(fun(E) -> E =:= X end, S_L),     % oszlop megszorítás
	  not lists:any(fun(E) -> E =:= X end, O_L),     % oszlop megszorítás
	  not lists:any(fun(E) -> E =:= X end, C_L)      % cella megszorítás
    ].


%% Megadja, hogy az X szám megfelel-e a P paritás megszorításoknak
paritas_szuro(_, []) ->
    true;
paritas_szuro(X, P) ->
    P_H = hd(P),
    if
	P_H =:= e ->
	    X rem 2 =:= 0 andalso paritas_szuro(X, tl(P));
	P_H =:= o ->
	    X rem 2 =:= 1 andalso paritas_szuro(X, tl(P));
	true -> paritas_szuro(X, tl(P))
    end.

%% Igazzal tér vissza, ha SZ megszorításban szerepel X, vagy SZ-ben nincsen szám
szam_szuro(_, []) ->
    true;
szam_szuro(X, SZ) ->
    SZ_H = hd(SZ),
    if
	is_number(SZ_H) ->
	    X =:= SZ_H andalso szam_szuro(X, tl(SZ));
	true -> szam_szuro(X, tl(SZ))
    end.

%% Visszaadja a sudoku mátrix (SMat) azon C-méretű celláját, amelybe beleesik a N×M koordináta 
cella(N, M, SMat, C) ->
    N_C = floor((N-1)/C)*C+1,
    M_C = floor((M-1)/C)*C+1,
    L = [(lists:sublist(S, M_C, C)) || S <- lists:sublist(SMat, N_C, C)],
    lists:foldl(fun(X, Acc) -> Acc++X end, [], L).

