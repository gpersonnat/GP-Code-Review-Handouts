(* 

AGENDA 

1) Introductions

Tips for the class: 

1) Read the textbook 

2) Do and redo the labs (and also read the lab solutions carefully!)

2) Let Expressions 

3) Functions 

4) Pattern Matching 

*)

(*  Let Expressions  *) 

(*  let keyword -> assigns (or binds) a variable name to the value of an expression  *) 

(*  let <var> = e1 in e2  *)


(* function maps an input value to an output value *)

(* High level: this function adds two integers 

Lower-Level: Takes an integer value x and returns a function that takes in another integer value y 
and adds x to y 

-> : function type constructor : used to define the type of a function 

A -> B 

*)
fun x -> fun y -> x + y ;;

(* int -> int -> int 

int -> (int -> int) = int -> int -> int 

*)

(fun x -> fun y -> x < y) 5 2  ;;

(*  (fun x -> fun y -> x < y) 5 =  fun y -> 5 < y   


((fun x -> fun y -> x < y) 5) 2 ;;
*)

let add = fun x -> fun y -> x + y ;;


let add x y = x + y ;;


let add : int -> int -> int = fun x -> fun y -> x + y ;;

(* Type annotations - How you should almost always write functions in this class *)
let add (x : int) (y : int) : int = 
  x + y ;; 

(*  

Pattern matching -> this is a way to deconstruct types 
*)

(* x! = x * (x - 1) ... 1; 5! = 5 * 4 * 3 * 2 * 1 *)

let rec factorial (n : int) : int = 
  match n with 
  | 0 -> 1 
  | _ -> n * factorial (n - 1)  ;;

let rec factorial (n : int) : int = 
  if n = 0 then 1 
  else n * factorial (n - 1) ;;


(* Length of the list  *)

let rec length (lst : int list) : int = 
  match lst with 
  | [] -> 0 
  | _ :: t -> 1 + length t ;;

let rec length (lst : int list) : int = 
  match lst with 
  | [] -> 0 
  | [h] -> 1 
  | h1 :: h2 :: t -> 2 + length t ;;


(* Anonymous function : function that doesn't have a name  *)
let add_term : float -> float = fun x -> let term = 7.3 in x +. term ;;

let add_term (x : float) : float = 
  let term = 7.3 in x +. term ;;
