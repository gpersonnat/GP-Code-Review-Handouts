
type 'a mlist = 
  'a mlist_internal ref 
  and 
    'a mlist_internal = 
      | Nil 
      | Cons of 'a * 'a mlist ;; 

(* 
Define a function mem_factorial which takes a unit as an argument and returns 1 the first
time it is called, returns 2 the second time it is called, returns 6 the third time it is called, and
so on. In general, the function should return n! on the nth time it is called. 
*)

(* unit -> int 

mem_factorial () = 1 

mem_factorial () = 2



*)

(*

Concept Check : Don't want to define the references outside because that would 
allow someone to modify the references without using the function
*)

let mem_factorial : unit -> int = 
  let n = ref 0 in 
  let fact = ref 1 in 
  fun () : int -> 
    let prev = if !n = 0 then 1 else !fact * (!n + 1) in 
    n := !n + 1; 
    fact := !n * !fact; 
    prev  ;;

let mem_factorial : unit -> int = 
   let n = ref 1 in 
   let fact = ref 1 in 
    fun () : int -> 
      n := !n + 1; 
      fact := !n * !fact; 
      !fact / (!n) ;;


(* Define a non-recursive implementation of List.filter *)
let filter_imper _ = failwith "not implemented" ;; 

(* range n m returns a list that contains all the integers from n to m-1 inclusive. Define three
versions of the range function: (1) a tail-recursive version, (2) a non-tail recursive version,
and (3) a procedural implementation (using loops) *)

let range _ = failwith "not implemented" ;; 

(* Define a function that merges two sorted (in ascending order) mutable lists. This function
should return a value of type unit and the first list should become the merging of the two lists *)
let rec merge (x : 'a mlist) (y : 'a mlist) : unit = 
  match !x, !y with 
  | _, Nil -> () 
  | Nil, _ -> x := !y 
  | Cons (hx, tx), Cons (hy, ty) -> 
    if hx < hy then merge tx y 
    else (merge x ty ; x := Cons(hy, x)) ;; 
 ;; 

(* Define a function that generates the Fibonacci numbers (i.e. fib(i) the ith time the function
is called 

fib(1) = 1 

fib(2) = 1

for n > 2: fib(n) = fib(n - 1) + fib(n - 2)

1, 1, 2, 3, 5, 8 

fib(1) = 1 

1, 2 

1 

num = 3

2, 3



*)
let fib : unit -> int = 
  let first = ref 1 in 
  let second = ref 1 in 
  fun () : int -> 
    let ret = !first in 
    let num = !first + !second in 
    first := !second; 
    second := num; 
    ret     ;;


(* Rewrite power from Part 2 (see Problem 6) to improve the worst-case time complexity. 
What is the asymptotic runtime of the function right now? *)
let rec power _ = failwith "not implemented" ;;


(* Problem 2 

1. Implement a non-recursive version of List.filter 

2. range n m returns a list that contains integers from n to m - 1 inclusive. Define this function in three ways:

1) Non tail-recursive 

2) Tail-recursive 

3) Procedural / Iterative Approaches 

*)


let filter (pred : 'a -> bool) (lst : 'a list) : 'a list = 
  let accum = ref [] in 
  let curr_list = ref lst in 
  while !curr_list <> [] 
  do 
    match !curr_list with 
    | [] -> failwith "there's a bug in filter" 
    | h :: t -> 
      if pred h then accum := !accum @ [h]; 
      curr_list := t
  done;
  !accum 

  (* List.rev -> O(n)
  
  
  1 + 2 + 3 + ..... + n = n(n + 1) / 2 = O(n^2)

  let y = 5 in 
  let y = 7 
  
  *)



let rec range (n : int) (m : int) : int list = 
  if n = m then [] 
  else n :: range (n + 1) m  ;;

let range (n : int) (m : int) : int list = 
  let rec range_inner (n' : int) (m' : int) (acc : int list) : int list = 
    if n' = m' then acc 
    else range_inner (n' + 1) m' (acc @ [n']) 
  in range_inner n m [] ;;

let range (n : int) (m : int) : int list = 
  let accum = ref [] in 
  let _ : unit = (for curr = n to m 
  do 
    accum := !accum @ [curr]
  done) in (); 
  !accum  ;;

(* 
 
e1; e2 

evaluate e1 then e2 

e1 has to be of type unit 

*)
