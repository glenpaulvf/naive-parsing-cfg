(* Homework 2. Naive parsing of context free grammars *)

let rec produce_al g nts =
	match g with
	| [] -> []
	| (nt, rhs)::tl ->
		if nt = nts then
			rhs::produce_al tl nts
		else produce_al tl nts

let convert_grammar gram1 = 
	(fst gram1, fun nts -> produce_al (snd gram1) nts)

type ('nonterminal, 'terminal) symbol =
	| N of 'nonterminal
	| T of 'terminal

(*	The matcher will produces the alternative list for a given symbol of a
 *	grammar. If there is no such list, then return None; otherwise, evaluate
 *	the alternative list and return the first acceptable match of a prefix of
 *	the fragment. The acceptor will return the first acceptable match or return
 *	None, if there is no such match.
 *
 * The matcher is implemented with mutually recursive functions. *)
let rec matcher prod_fn sym accept derivation frag =
	let al = prod_fn sym in
	if al = [] then
		None
	else
		evaluate_al prod_fn sym al accept derivation frag

(*	Given an empty alternative list, return None. Given a non-empty alternative
 *	list, evaluate the first right hand side and concatenate the derivation
 *	with a tuple containing the symbol and the right hand side being evaluated.
 *	If the evaluation of the right hand side results in Some x, where x is some
 *	value, then return Some x; otherwise, evaluate the next right hand side. *)
and evaluate_al prod_fn sym al accept derivation frag =
	if List.length al = 0 then
		None
	else
		let value = evaluate_rhs prod_fn (List.hd al) accept (derivation@[(sym, List.hd al)]) frag in
		match value with
		| Some x -> Some x
		| None -> evaluate_al prod_fn sym (List.tl al) accept derivation frag

(*	Given an empty right hand side, pass the current derivation and fragment to
 *	the acceptor. Given a non-empty right hand side, evaluate the symbol. If
 *	the symbol is a nonterminal, then match the nonterminal with an acceptor
 *	that evaluates the right hand side. Otherwise, if the symbol if a terminal
 *	and the fragment isn't empty, then return None. However, if the symbol is a
 *	terminal and the fragment isn't empty, then evaluate the terminal. If the
 *	terminal is equal to the prefix of the fragment, then evaluate the
 *	remaining right hand sides and the suffix of the fragment. Otherwise,
*	return None. *)
and evaluate_rhs prod_fn rhs accept derivation frag =
	if List.length rhs = 0 then
		accept derivation frag
	else
		let sym = List.hd rhs in
		let rhs = List.tl rhs in
		match sym with
		| N nt -> matcher prod_fn nt (evaluate_rhs prod_fn rhs accept) derivation frag 
		| T t when frag = [] -> None
		| T t -> if t = List.hd frag then evaluate_rhs prod_fn rhs accept derivation (List.tl frag) else None

let parse_prefix gram accept frag =
	matcher (snd gram) (fst gram) accept [] frag
