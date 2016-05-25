(*
  Parser for lambda-calculus.
*)

open Utils
open Lexer


(* AST for lambda expressions - DO NOT MODIFY THIS TYPE *)
type term = | Variable of string
	    | Abstraction of string * term
	    | Application of term * term

(*
  Concrete Syntax:
  t ::= id | (\id.t) | (t1 t2) | (t) | let id=t1 in t2

  Abstract Syntax:
  term ::= id | \id.term | term1 term2
*)

exception SyntaxError of string

(*
  ADD FUNCTIONS BELOW
*)

let rec parse_term ts =
  match ts with
    | [] -> raise (SyntaxError "Term expected")
    | (Literal id) :: ts' -> (Variable id), ts'
    | LParen :: (LambdaTok :: ((Literal id) :: (DotTok :: ts')))  -> 
			let t1, ts'' = parse_term ts' in (
			  match ts'' with
			  | RParen :: ts''' -> Abstraction(id, t1), ts'''
			  | _ -> raise (SyntaxError "Unexpected token")
			)
    | LParen :: ts' ->  	    
		    let t1, ts'' =  parse_term ts' in (
			  match ts'' with
			    | RParen :: ts''' -> t1, ts'''
			    | _ -> let t2, ts''' = parse_term ts'' in (
			    match ts''' with
				  | RParen :: ts'''' -> (Application (t1, t2)), ts''''
				  | _ -> raise (SyntaxError "Unexpected token. RParen expected")
		      )
			)
    | LetTok :: (Literal id :: (EqTok :: ts')) -> let t1, ts'' = parse_term ts' in (
	  match ts'' with
	    | InTok :: ts''' -> let t2, ts'''' = parse_term ts''' in (
	      (Application ((Abstraction (id, t2)), t1)), ts'''' 
	    )
	    | _ -> raise (SyntaxError "Unexpected token. InTok expected")
	)
	| _ -> raise (SyntaxError "Parsing error. Unexpected token")


let rec format_term = function
  | Variable var -> "(" ^ var ^ ")"
  | Abstraction(s,t1) -> "(\\" ^ s ^ "." ^ (format_term t1) ^")"
  | Application(t1, t2) -> "(" ^ (format_term t1) ^ " " ^ (format_term t2) ^ ")"
  
let parse s = 
                let (t1, ts) =
				   s |> string_to_list |> tokenize |> parse_term
	      		in
	      		match ts with
	      		| [] -> t1
	      		| _ -> raise (SyntaxError "Unexpected input.")
	
		
let rec format_term_conv = function
  | Variable var ->  var 
  | Abstraction(s,t1) -> "\\" ^ s ^ "." ^ (format_term_conv t1)
  | Application(t1, t2) -> (format_term_conv t1) ^ " " ^ (format_term_conv t2) 	
  
let get_first lst =
	match lst with
	| first :: rest -> first
	| [] -> raise (SyntaxError "Empty list")

let rec my_parse_term_conv1 ts =
  match ts with
    | [] -> raise (SyntaxError "Term expected")
    | (Literal id) :: ts' -> (Variable id), ts'
    | LParen :: (LambdaTok :: ((Literal id) :: (DotTok :: ts')))  -> 
			let t1, ts'' = my_parse_term_conv1 ts' in (
			  match ts'' with
			  | RParen :: ts''' -> Abstraction(id, t1), ts'''
			  | _ -> raise (SyntaxError "Unexpected token")
			)
    | LParen :: ts' ->  	    
		    let t1, ts'' =  my_parse_term_conv1 ts' in (
			  match ts'' with
			    | RParen :: ts''' -> t1, ts'''
			    | _ -> let ref_t1, ref_ts'' = ref t1, ref ts'' in (
					while ((!ref_ts'' <> []) && ((get_first !ref_ts'') <> RParen)) do 
						let t2, ts''' = my_parse_term_conv1 !ref_ts'' in (
							ref_t1 := Application(!ref_t1, t2);
							ref_ts'' := ts''';
					)	
				   done;
				   match !ref_ts'' with
				   | RParen :: lst -> !ref_t1,lst
				   | _ -> raise (SyntaxError "Unexpected token. RParen expected")
				)
			)
    | LetTok :: (Literal id :: (EqTok :: ts')) -> let t1, ts'' = my_parse_term_conv1 ts' in (
	  match ts'' with
	    | InTok :: ts''' -> let t2, ts'''' = my_parse_term_conv1 ts''' in (
	      (Application ((Abstraction (id, t2)), t1)), ts''''
	    )
	    | _ -> raise (SyntaxError "Unexpected token. InTok expected")
	)
    | LambdaTok :: ((Literal id) :: (DotTok :: ts'))  -> 
			let t1, ts'' = my_parse_term_conv1 ts' in (
			  match ts'' with
			  | [] -> Abstraction(id, t1), []
			  | RParen :: ts''' -> Abstraction(id, t1), ts''
			  | _ -> let ref_t1, ref_ts'' = ref t1, ref ts'' in (
				while ((!ref_ts'' <> []) && ((get_first !ref_ts'') <> RParen)) do 
					let t2, ts''' = my_parse_term_conv1 !ref_ts'' in (
						ref_t1 := Application(!ref_t1, t2);
						ref_ts'' := ts''';
				)	
			   done;
			   Abstraction(id, !ref_t1), !ref_ts''
			))
    | _ ->  	    
		    let t1, ts' =  my_parse_term_conv1 ts in (
			  match ts' with
			    | [] -> t1, ts'
			    | _ -> let t2, ts'' = parse_term ts' in (
					(Application (t1, t2)), ts''
		        )
			)
			
let rec my_parse_term_conv (t, lst) = 
	let ref_t1, ref_lst = ref t, ref lst in (
				while ((!ref_lst <> []) && ((get_first !ref_lst) <> RParen)) do 
					let t2, lst'' = my_parse_term_conv1 !ref_lst in (
						ref_t1 := Application(!ref_t1, t2);
						ref_lst := lst'';
				)	
			   done;
			   !ref_t1, !ref_lst
			)

let rec parse_term_conv ts =
  match ts with
    | [] -> raise (SyntaxError "Term expected.")
    | (Literal id) :: ts' -> my_parse_term_conv((Variable id), ts')
    | LParen :: (LambdaTok :: ((Literal id) :: (DotTok :: ts')))  -> 
			let t1, ts'' = my_parse_term_conv1 ts' in (
			  match ts'' with
			  | RParen :: ts''' -> my_parse_term_conv (Abstraction(id, t1), ts''')
			  | _ -> raise (SyntaxError "Unexpected token")
			)
    | LParen :: ts' ->  	    
		    let t1, ts'' =  my_parse_term_conv1 ts' in (
			  match ts'' with
			    | RParen :: ts''' -> my_parse_term_conv(t1, ts''')
			    | _ -> let ref_t1, ref_ts'' = ref t1, ref ts'' in (
					while ((!ref_ts'' <> []) && ((get_first !ref_ts'') <> RParen)) do 
						let t2, ts''' = my_parse_term_conv1 !ref_ts'' in (
							ref_t1 := Application(!ref_t1, t2);
							ref_ts'' := ts''';
					)	
				   done;
				   match !ref_ts'' with
				   | RParen :: lst-> my_parse_term_conv (!ref_t1, lst)
				   | _ -> raise (SyntaxError "Unexpected token. RParen expected")
				)
			)
    | LetTok :: (Literal id :: (EqTok :: ts')) -> let t1, ts'' = my_parse_term_conv1 ts' in (
	  match ts'' with
	    | InTok :: ts''' -> let t2, ts'''' = my_parse_term_conv1 ts''' in (
	      my_parse_term_conv((Application ((Abstraction (id, t2)), t1)), ts'''')
	    )
	    | _ -> raise (SyntaxError "Unexpected token. InTok expected")
	)
    | LambdaTok :: ((Literal id) :: (DotTok :: ts'))  -> 
			let t1, ts'' = my_parse_term_conv1 ts' in (
			  match ts'' with
			  | [] -> Abstraction(id, t1), []
			  | RParen :: ts''' -> Abstraction(id, t1), ts''
			  | _ -> let ref_t1, ref_ts'' = ref t1, ref ts'' in (
				while ((!ref_ts'' <> []) && ((get_first !ref_ts'') <> RParen)) do 
					let t2, ts''' = my_parse_term_conv1 !ref_ts'' in (
						ref_t1 := Application(!ref_t1, t2);
						ref_ts'' := ts''';
				)	
			   done;
			   my_parse_term_conv(Abstraction(id, !ref_t1), !ref_ts'')
			))
    | _ -> raise (SyntaxError "Unexpected token. RParen expected")
		
let parse_conv s = 
                let (t1, ts) =
				   s |> string_to_list |> tokenize |> parse_term_conv
	      		in
	      		match ts with
	      		| [] -> t1
	      		| _ -> raise (SyntaxError "Unexpected input in parse_conv.")
		
		
