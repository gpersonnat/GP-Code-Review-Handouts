open List ;; 

(* 
Write a function remove_duplicates that takes in a list and returns the duplicates from the
list. Write a non-recursive solution. You may use List.sort. You may also assume that you
have the function to_run_length (from problem set 1)
*)

(* Implement this with your solution from pset1! *)
let to_run_length (lst : 'a list) : (int * 'a) list = 
  failwith "not yet implemented" ;; 

let remove_duplicates (lst : 'a list) : 'a list =
  let l = lst |> List.sort Stdlib.compare |> to_run_length |> 
              List.filter (fun (count, _) -> count > 1)
  in List.map (fun (_, elt) -> elt) l ;; 



(* 
Write a function tranpose that takes a list of lists that represents a matrix (each list corre-
sponds to a row) and returns the transpose of the matrix. 
*)

let rec transpose (lst : 'a list list) : 'a list list =
  match lst with
  | [] -> []
  | h :: t -> 
    let f = fun acc x ->
        match x with
      | [] -> acc
      | hd :: _ -> acc @ [hd] 
    in
    List.fold_left f [] lst ::
    (transpose (List.filter ((!=) []) (
  List.map List.tl lst))) ;;

(* 
Write a polymorphic function matrix_mult that takes in a function to add elements two
elements, a function to multiply elements, an initial value used to add and multiply elements
(i.e. think of this as the zero element), and two matrices. The result should be the matrix
formed by multiplying the two matrices together.
*)

let rec matrix_multiply (add : 'a -> 'a -> 'a) (mult : 'a -> 'a -> 'a) 
                        (init : 'a) (x : 'a list list) (y : 'a list list) 
                        : 'a list list = 
  let rec dot_product (xs : 'a list) (ys : 'a list) : 'a = 
    match xs, ys with 
    | [], [] -> init
    | [], _ | _, [] -> raise (Invalid_argument "invalid dot product") 
    | hx :: tx, hy :: ty -> add (mult hx hy) (dot_product tx ty) in
  let b = List.length (List.hd x) in 
  let c = List.length y in 
  let y_t = transpose y in 
  if b != c then raise (Invalid_argument "Incompatible dimensions") 
  else List.fold_left (fun acc row -> 
                            acc @  
                            [(List.fold_left (fun acc col -> 
                              acc @ [(dot_product row col)])   [] y_t )]) [] x 
;;


(* Define a function repeat f x n that applies f to x exactly
n times. *)
let rec repeat (f : 'a -> 'b) (x : 'a) (n : int) : 'b =
 if n = 1 then f x ;; 
(* 
Define a function compose : (’a -> ’a) list -> (’a -> ’a) that takes a list of 
functions and returns the composition of those functions. For example, suppose we 
had a list of functions [ f ; g; h]. compose should return the function f ◦ g ◦ h
or ℓ(x) = f (g(h(x))
*)
let compose (lst : ('a -> 'a) list) : ('a -> 'a) = 
  List.fold_right (fun f acc -> fun x -> f (acc x)) lst (fun x -> x) ;; 


(* Option and Exception Conversion *)

(*
Write a function opt_to_ext : (’a -> ’b option) -> exn -> (’a -> ’b) that
takes in a function f of type ’a -> ’b option and an exception. If the input function
returns None for an argument, the output function should raise the exception passed into
opt_to_ext. If the input function returns Some v on an argument, the output function
should return v for that argument.
*)

let opt_to_exp (f : 'a -> 'b option) (ex : exn) : 'a -> 'b =
  fun x ->
    match f x with
    | None -> raise ex
    | Some v -> v ;;


(* 
Write a function ext_to_opt : (’a -> ’b) -> (’a -> ’b option) that converts
a function that (potentially) raises an exception to a function that returns a value having an
option type. If the input function raises any exception on an input, the output function should
return None. If the input function returns a value v for an input, the output function should
return Some v.
*)

let ext_to_opt (f : 'a -> 'b) : 'a -> 'b option =
  fun x ->
  try
   Some (f x)
  with
   _ -> None ;;


