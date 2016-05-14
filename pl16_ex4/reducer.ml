(*
  Reducers (interpreters) for lambda-calculus.
*)

open Utils
open Parser


exception OutOfVariablesError


let possible_variables = List.map (fun x -> char_to_string (char_of_int x)) ((range 97 123) @ (range 65 91))



(*
  ADD FUNCTIONS BELOW
*)


let rec fv = function
  | Variable var -> StringSet.singleton var
  | Abstraction (id, term) -> StringSet.remove id (fv term)
  | Application (term1, term2) -> StringSet.union (fv term1) (fv term2)
  
let fresh_var used_vars =
  let rec fresh_var_rec used_vars lst =
      match lst with
      |[] -> raise OutOfVariablesError
      |(hd:string)::tail -> if StringSet.mem hd used_vars then fresh_var_rec used_vars tail
                   else hd 
  in fresh_var_rec used_vars possible_variables
  
let rec substitute (x:string) t1 t2 =
  match t2 with
  | Variable y when x = y -> t1
  | Variable y -> Variable y
  | Application (term1, term2) -> Application ((substitute x t1 term1), (substitute x t1 term2))
  | Abstraction (y, t) when x = y -> Abstraction (x, t)
  | Abstraction (y, t) ->
    let fv_t1 = fv t1 in(
      if StringSet.mem y fv_t1 then( 
        let fv_t = fv t in
          let vset = StringSet.add x (StringSet.union fv_t1 fv_t) in
            let z = fresh_var vset in 
			  Abstraction (z, (substitute x t1 (substitute y (Variable z) t)))    
	  )
	  else Abstraction (y, (substitute x t1 t))
    )
		
let reduce_strict term = function
	| Application(t1,t2) -> let t1' = reduce_strict t1 in (
			match t1' with
			| Some t1'' -> Some(Application(t1'',t2))
			| None -> let t2' = reduce_strict t2 in(
			match t2' with
			| Some t2'' -> Some(Application(t1,t2''))
			| None -> match t1 with
			| Abstraction(x,term_3) -> Some(substitute x t2 term_3)
			| _ -> None
			))
	| _ -> None
							
			
let reduce_lazy term = function
	| Application(t1,t2) -> let t1' = reduce_lazy t1 in (
			match t1' with
			| Some t1'' -> Some(Application(t1'',t2))
			| None -> match t1 with
					| Abstraction(x,term1) -> Some(substitute x t2 term1)
					| _ -> None
	)
	| _ -> None
	
let reduce_normal term = function	
	| Application(t1,t2) -> match t1 with
							| Abstraction(x, term1) -> Some(substitute x t2 term1)
							| _ -> let t1' = reduce_normal t1 in(
							match t1' with 
							| Some t1'' -> Some(Application(t1'',t2))
							| None -> let t2' = reduce_normal t2 in(
							match t2' with
							| Some t2'' -> Some(Application(t1,t2''))
							| None -> None
							))
	| Abstraction(x, term_1) -> let term_1' = reduce_normal term_1 in(
								match term_1' with
								| Some term_1'' -> Abstraction(x, term_1'')
								| None -> None			
								)
	| _ -> None

