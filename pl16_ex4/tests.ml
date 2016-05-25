(*
  Tests for the lambda calculus parser and reducers.

  EXTEND THIS FILE TO TEST YOUR SOLUTION THOROUGHLY!
*)

open Utils
open Parser
open Reducer
open Lexer

let rec evaluate ~verbose reduce t =
  if verbose then print_string (format_term t) else ();
  match reduce t with
  | None ->
    if verbose then print_string " =/=>\n\n" else ();
    t
  | Some t' ->
    if verbose then print_string " ==>\n\n" else ();
    evaluate ~verbose reduce t'

let test_and_1 = "
let tru = (\\t.(\\f.t)) in
let fls = (\\t.(\\f.f)) in
let and = (\\b.(\\c. ((b c) fls))) in
((and tru) tru)
"

let test_and_2 = "
let tru = (\\t.(\\f.t)) in
let fls = (\\t.(\\f.f)) in
let and = (\\b.(\\c. ((b c) fls))) in
((and fls) ((and tru) tru))
"

let env = "
let tru = (\\t. (\\f. t)) in
let fls = (\\t. (\\f. f)) in
let test = (\\l. (\\m. (\\n. ((l m) n)))) in
let and = (\\b. (\\c.  ((b c) fls))) in

let pair = (\\f. (\\s. (\\b.  ((b f) s)))) in
let fst = (\\p. (p tru)) in
let snd = (\\p. (p fls)) in

let c0 = (\\s. (\\z. z)) in
let c1 = (\\s. (\\z. (s z))) in
let c2 = (\\s. (\\z. (s (s z)))) in
let c3 = (\\s. (\\z. (s (s (s z))))) in
let c4 = (\\s. (\\z. (s (s (s (s z)))))) in
let c5 = (\\s. (\\z. (s (s (s (s (s z))))))) in
let c6 = (\\s. (\\z. (s (s (s (s (s (s z)))))))) in
let c7 = (\\s. (\\z. (s (s (s (s (s (s (s z))))))))) in
let c8 = (\\s. (\\z. (s (s (s (s (s (s (s (s z)))))))))) in
let c9 = (\\s. (\\z. (s (s (s (s (s (s (s (s (s z))))))))))) in
let c10 = (\\s. (\\z. (s (s (s (s (s (s (s (s (s (s z)))))))))))) in

let scc = (\\n. (\\s. (\\z. (s ((n s) z))))) in
let plus = (\\m. (\\n. (\\s. (\\z. ((m s) ((n s) z)))))) in
let times = (\\m. (\\n. (\\s. (m (n s))))) in
let power = (\\m. (\\n. (n m))) in
let iszero = (\\m. ((m (\\x. fls)) tru)) in
let prd = (let zz = ((pair c0) c0) in
           let ss = (\\p. ((pair (snd p)) ((plus c1) (snd p)))) in
           (\\m. (fst ((m ss) zz)))) in
let leq = (\\m. (\\n. (iszero ((n prd) m)))) in
let equal = (\\m. (\\n. ((and ((leq m) n)) ((leq n) m)))) in

let Y = (\\f. ((\\x. (f (x x))) (\\x. (f (x x))))) in
let Z = (\\f. ((\\x. (f (\\y. ((x x) y)))) (\\x. (f (\\y. ((x x) y)))))) in
"

let test_fact_l = env ^ "
let fact_l = (Y (\\f. (\\n. (((test (iszero n)) c1) (((times n) (f (prd n)))))))) in
((equal (fact_l c2)) c2)
"

let test_fact_s = env ^ "
let fact_s = (Z (\\f. (\\n. ((((test (iszero n)) (\\x. c1)) (\\x. (((times n) (f (prd n)))))) (\\x.x))))) in
((equal (fact_s c2)) c2)
"

let testQ1_1 = "let x = (\\x. x) in ((\\y. y) (\\z. x))"
let testQ1_2 = "
let tru = (\\t.(\\f.t)) in
let fls = (\\t.(\\f.f)) in
let and = (\\b.(\\c. ((b c) fls))) in
((and tru) tru)
"
let testQ1_3 = "y"
let testQ1_4 = "y\n(\\x. x)((\\y. y)(\\z. z))z\nlet x = (\\x. x) in ((\\y. y) (\\z. x))"

let testQ2_1 = "(\\x.t)"
let testQ2_2 = "((x y) z)"
let testQ2_3 = "(\\x.(\\y.(\\z.((((w x) y) z)u))))"
let testQ2_4 = "(\\x.(\\y.(\\z.((x y) z))))"

let testQ5_EApp1 = "let a = ((\\x. y)(\\w. z)) in (a u)"
let testQ5_EAppAbs = "((\\x. (x x))(\\y. y))"
let testQ5_EApp2 = "((\\x. x)((\\y. y)t))"
let testQ5_EAbs = "(\\x.((\\y.y)t))"

let testQ6_EApp2 = "(x ((\\y. y)t))"
let testQ6_EAbs = "(\\x.((\\y.y)t))"
let testQ6_EApp1 = "let a = ((\\x. y)(\\w. z)) in (a u)"
let testQ6_EAppAbs = "((\\x. (x x))(\\y. y))"

let testQ7_EApp2 = "(x ((\\y. (y y))t))"
let testQ7_EAbs = "(\\x.((\\y.y)((\\z.z)t)))"
let testQ7_EApp1 = "let a = ((\\x. y)(\\w. z)) in (a u)"
let testQ7_EAppAbs = "((\\x. (x x))(\\y. y))"

let testQ9_1 = "\\x. \\y. \\z. x y z"
let testQ9_2 = "x y z"
let testQ9_3 = "(let a = ((\\x. y)(\\w. z)) in (a u))"
let testQ9_4 = "x (y z)"
let testQ9_5 = "(x y z) (a b c)"

let testQ10_abs = "(\\x.(\\y.(\\z.((x y) z))))"
let testQ10_app = "(x y)"
let testQ10_var = "(x)"

let rec print_term = function
  | Variable id -> "Variable(" ^ id ^ ")"
  | Abstraction (a,b) -> "Abs(" ^ a ^ ", " ^ print_term b ^ ")"
  | Application (a,b) -> "App(" ^ print_term a ^ ", " ^ print_term b ^ ")"

let test ~verbose ~sem ~reduce s =
  printf "\nEvaluating:\n%s\nin %s semantics:\n\n" s sem;
  let result = (evaluate ~verbose reduce (parse s)) in
  printf "Result is: %s\n\n" (format_term result)

  
let test_lazy = test ~sem:"lazy" ~reduce:reduce_lazy
let test_strict = test ~sem:"strict" ~reduce:reduce_strict
let test_normal = test ~sem:"normal-order" ~reduce:reduce_normal
let test_all ~verbose s =
  test_lazy ~verbose s;
  test_strict ~verbose s;
  test_normal ~verbose s


let rec printSet set = 
	if StringSet.is_empty set = false then let x = StringSet.choose set in (
			let s' = StringSet.remove x set in(
				printf "%s" x;
				if StringSet.is_empty s' = false then printf ", "; printSet s';

			)
	
	)
	else printf ""
	
	
let () =
  test_all ~verbose:true test_and_1;
  test_all ~verbose:true test_and_2;
    
  test_lazy ~verbose:false test_fact_l;
  test_strict ~verbose:false test_fact_s;
  test_normal ~verbose:false test_fact_l;
  test_normal ~verbose:false test_fact_s;
  
  
  printf "\nTesting Question 1:\n";
  printf "Parsing string: %s:\n" testQ1_1;
  printf "%s\n" (print_term (parse testQ1_1));
  printf "Reformatting term to string format:\n";
  printf "%s\n\n" (format_term (parse testQ1_1));
  printf "Parsing string: %s:\n" testQ1_2;
  printf "%s\n" (print_term (parse testQ1_2));
  printf "Reformatting term to string format:\n";
  printf "%s\n\n" (format_term (parse testQ1_2));
    
  printf "Tokenizing string: %s:\n" testQ1_4;
  printf "Parsing terms\n";
  let l = tokenize (string_to_list testQ1_4) in 
    let rec call_parse_term = function
	  | lst -> if List.length lst > 0 then let t, l1 = parse_term lst in
          printf "Parsed term\n:%s\n" (print_term (t));
          printf "Reformatting term to string format:\n";
          printf "%s\n\n" (format_term (t));
		  call_parse_term l1 in call_parse_term l;
	

  printf "\nTesting Question 2:\n";	
  printf "FV(%s) = " (testQ2_1);
  printf "{";
  testQ2_1 |> parse |> fv |> printSet; 
  printf "}\n";
  
  printf "FV(%s) = " (testQ2_2);
  printf "{";
  testQ2_2 |> parse |> fv |> printSet; 
  printf "}\n";
  printf "FV(%s) = " (testQ2_3);
  printf "{";
  testQ2_3 |> parse |> fv |> printSet; 
  printf "}\n";
  
  printf "FV(%s) = " (testQ2_4);
  printf "{";
  testQ2_4 |> parse |> fv |> printSet; 
  printf "}\n\n";
	
  printf "\nTesting Question 4:\n";
  printf "substitute x y x\n";
  printf "%s\n" (format_term (substitute "x" (parse "y") (parse "x")));
  printf "substitute x y (\\x. x)\n";
  printf "%s\n" (format_term (substitute "x" (parse "y") (parse "(\\x. x)")));
  printf "substitute x y \\z. (z x)\n";
  printf "%s\n" (format_term (substitute "x" (parse "y") (parse "(\\z. (z x))")));
  printf "substitute x y ((\\x. (x z)) (\\z. (z x)))\n";
  printf "%s\n" (format_term (substitute "x" (parse "y") (parse "((\\x. (x z)) (\\z. (z x)))")));
  
  printf "\nTesting Question 5 - E-App1:\n";
  test_strict ~verbose:false testQ5_EApp1;
  printf "\nTesting Question 5 - E-App2:\n";
  test_strict ~verbose:false testQ5_EApp2;
  printf "\nTesting Question 5 - E-AppAbs:\n";
  test_strict ~verbose:false testQ5_EAppAbs;
  printf "\nTesting Question 5 - E-Abs:\n";
  test_strict ~verbose:false testQ5_EAbs;
  
  printf "\nTesting Question 6 - E-App1:\n";
  test_lazy ~verbose:false testQ6_EApp1;
  printf "\nTesting Question 6 - E-App2:\n";
  test_lazy ~verbose:false testQ6_EApp2;
  printf "\nTesting Question 6 - E-AppAbs:\n";
  test_lazy ~verbose:false testQ6_EAppAbs;
  printf "\nTesting Question 6 - E-Abs:\n";
  test_lazy ~verbose:false testQ6_EAbs;
  
  printf "\nTesting Question 7 - E-App1:\n";
  test_normal ~verbose:false testQ7_EApp1;
  printf "\nTesting Question 7 - E-App2:\n";
  test_normal ~verbose:false testQ7_EApp2;
  printf "\nTesting Question 7 - E-AppAbs:\n";
  test_normal ~verbose:false testQ7_EAppAbs;
  printf "\nTesting Question 7 - E-Abs:\n";
  test_normal ~verbose:false testQ7_EAbs;

  
  printf "\nTesting Question 10:\n\n";
  printf "Reformatting term %s to string format by conventions:\n" testQ10_abs;
  printf "%s\n\n" (format_term_conv (parse testQ10_abs));

  printf "Reformatting term %s to string format  by conventions:\n" testQ10_app;
  printf "%s\n\n" (format_term_conv (parse testQ10_app));
  
  printf "Reformatting term %s to string format  by conventions:\n" testQ10_var;
  printf "%s\n\n" (format_term_conv (parse testQ10_var));

  printf "\nTesting Question 9:\n";
  printf "Parsing string: %s:\n" testQ9_1;
  printf "%s\n" (print_term (parse_conv testQ9_1));
  printf "Reformatting term to string format:\n";
  printf "%s\n\n" (format_term (parse_conv testQ9_1));

  printf "Parsing string: %s:\n" testQ9_2;
  printf "%s\n" (print_term (parse_conv testQ9_2));
  printf "Reformatting term to string format:\n";
  printf "%s\n\n" (format_term (parse_conv testQ9_2));
  
  printf "Parsing string: %s:\n" testQ9_3;
  printf "%s\n" (print_term (parse_conv testQ9_3));
  printf "Reformatting term to string format:\n";
  printf "%s\n\n" (format_term (parse_conv testQ9_3));
  
  printf "Parsing string: %s:\n" testQ9_4;
  printf "%s\n" (print_term (parse_conv testQ9_4));
  printf "Reformatting term to string format:\n";
  printf "%s\n\n" (format_term (parse_conv testQ9_4));
  
  printf "Parsing string: %s:\n" testQ9_5;
  printf "%s\n" (print_term (parse_conv testQ9_5));
  printf "Reformatting term to string format:\n";
  printf "%s\n\n" (format_term (parse_conv testQ9_5));
  
