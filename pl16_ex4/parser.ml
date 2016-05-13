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
    | [] -> raise (SyntaxError "Term expected\n")
    | (Literal id) :: ts' -> (Variable id), ts'
    | LParen :: (LambdaTok :: ((Literal id) :: (DotTok :: ts')))  -> 
			let t1, ts'' = parse_term ts' in (
			  match ts'' with
			  | RParen :: ts''' -> Abstraction(id, t1), ts'''
			  | _ -> raise (SyntaxError "Unexpected token\n")
			)
    | LParen :: ts' ->  	    
		    let t1, ts'' =  parse_term ts' in (
			  match ts'' with
			    | RParen :: ts''' -> t1, ts'''
			    | _ -> let t2, ts''' = parse_term ts'' in (
			    match ts''' with
				  | RParen :: ts'''' -> (Application (t1, t2)), ts''''
				  | _ -> raise (SyntaxError "Unexpected token. RParen expected\n")
		      )
			)
    | LetTok :: (Literal id :: (EqTok :: ts')) -> let t1, ts'' = parse_term ts' in (
	  match ts'' with
	    | InTok :: ts''' -> let t2, ts'''' = parse_term ts''' in (
	      (Application ((Abstraction (id, t2)), t1)), ts'''' 
	    )
	    | _ -> raise (SyntaxError "Unexpected token. InTok expected\n")
	)
	| _ -> raise (SyntaxError "Parsing error. Unexpected token\n")


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
	      		| _ -> raise (SyntaxError "Unexpected input.\n")