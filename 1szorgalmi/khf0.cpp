#include "cekla.h"

int len (const list LS) {
	if (LS == nil) return 0;
	return len(tl(LS)) + 1;
}

int pow (const int A, const int N) {
	if (N == 0) return 1;
	return pow(A, N-1) * A;
}

int elem_at (const list LS, const int N) {
	if (N == 1) return hd(LS);
	return elem_at(tl(LS), N - 1);
}

list append (const list L1, const list L2) {
	if (L1 == nil) return L2;
	return cons(hd(L1), append(tl(L1), L2));
}

list list_to (const list LS, const int N) {
	if (N == 1) return nil;
	return cons(hd(LS), list_to(tl(LS), N-1));
}

list list_from (const list LS, const int N) {
	if (N == 0) return LS;
	return list_from(tl(LS), N-1);
}

list list_betw (const list LS, const int N, const int M) {
	return list_from(list_to(LS, M), N);
}

list swap (const list LS, const int A, const int B) {
	const list from = list_from(LS, B);
	const int new_B = elem_at(LS, A);
	const list betw = list_betw(LS, A, B);
	const int new_A = elem_at(LS, B);
	const list to = list_to(LS, A);
	return append(to, cons(new_A, append(betw, cons(new_B, from))));
}

list revapp (const list L, const list L0) {
	if (L == nil) return L0;
	return revapp(tl(L), cons(hd(L), L0));
}

list reverse (const list L) {
	return revapp(L, nil);
}

list int_to_list_helper (const int S, const int A) {
	if (S < A) return cons(S, nil);
	return cons(S%A, int_to_list_helper(S/A, A));
}

list int_to_list (const int S, const int A) {
	return reverse(int_to_list_helper(S, A));
}

int list_to_int (const list LS, const int A) {
	if (tl(LS) == nil) return hd(LS);
	return pow(A, len(LS)-1) * hd(LS) + list_to_int(tl(LS), A);
}

list osszekevert_list_helper (const list LS, const int I, const int J) {
	if (I == J) return LS;
	if (I % 2 == 0) if (J % 2 == 0) return osszekevert_list_helper(swap(LS, I, J), I+1, J-1);
	if (I % 2 == 0) if (J % 2 != 0) return osszekevert_list_helper(LS, I, J-1);
	if (I % 2 != 0) if (J % 2 == 0) return osszekevert_list_helper(LS, I+1, J);
	if (I % 2 != 0) if (J % 2 != 0) return osszekevert_list_helper(LS, I+1, J-1);
}

list osszekevert_list (const list LS) {
	return osszekevert_list_helper(LS, 1, len(LS));
}

int osszekevert (const int S, const int A) {
	return list_to_int(osszekevert_list(int_to_list(S, A)), A);	
}
