(* Homework 2. Naive parsing of context free grammars *)

let rec produce_al g nts =
	match g with
	| [] -> []
	| (nt, rhs)::tl -> if nt = nts then rhs@produce_al tl nts else produce_al tl nts

let convert_grammar gram1 = 
	(fst gram1, fun nts -> produce_al (snd gram1) nts)
