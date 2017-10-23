(* Homework 2. Naive parsing of context free grammars *)

let accept_all derivation string = Some (derivation, string)

type sample_nonterminals =
	| Expr | Binop | Num

let sample_grammar_1 = (Expr, function
	| Expr ->
		[[N Num; N Binop; N Num];
		[N Num]];
	| Binop ->
		[[T"+"];
		[T"*"]]
	| Num ->
		[[T"0"]; [T"1"]; [T"2"]; [T"3"]; [T"4"]])

let sample_grammar_2 = (Expr, function
	| Expr ->
		[[N Num];
		[N Num; N Binop; N Num]];
	| Binop ->
		[[T"+"];
		[T"*"]]
	| Num ->
		[[T"0"]; [T"1"]; [T"2"]; [T"3"]; [T"4"]])

let test_1 = 
	((parse_prefix sample_grammar_1 accept_all ["1"; "+"; "2"; "*"; "3"])
	= Some
 		([(Expr, [N Num; N Binop; N Num]); (Num, [T "1"]); (Binop, [T "+"]);
   		(Num, [T "2"])],
  		["*"; "3"]))

let test_2 =
	((parse_prefix sample_grammar_2 accept_all ["1"; "+"; "2"; "*"; "3"])
	= Some ([(Expr, [N Num]); (Num, [T "1"])], ["+"; "2"; "*"; "3"]))
