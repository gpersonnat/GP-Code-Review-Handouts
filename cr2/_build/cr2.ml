
(* 
Write a function remove_duplicates that takes in a list and returns the duplicates from the
list. Write a non-recursive solution. You may use List.sort. You may also assume that you
have the function to_run_length (from problem set 1)
*)

let to_run_length (lst : 'a list) : int * 'a list = 
  failwith "not yet implemented" ;; 

let remove_duplicates _ = failwith "not yet implemented" ;; 


(* 
Write a function tranpose that takes a list of lists that represents a matrix (each list corre-
sponds to a row) and returns the transpose of the matrix. 
*)

let transpose _ = failwith "not yet implemented" ;; 

(* 
Write a polymorphic function matrix_mult that takes in a function to add elements two
elements, a function to multiply elements, an initial value used to add and multiply elements
(i.e. think of this as the zero element), and two matrices. The result should be the matrix
formed by multiplying the two matrices together.
*)

let matrix_mult _ = failwith "not yet implemented" ;; 


(* Define a function repeat f x n that applies f to x exactly
n times. *)
let repeat _ = failwith "not yet implemented" ;; 

(* 
Define a function compose : (’a -> ’a) list -> (’a -> ’a) that takes a list of 
functions and returns the composition of those functions. For example, suppose we 
had a list of functions [ f ; g; h]. compose should return the function f ◦ g ◦ h
or ℓ(x) = f (g(h(x))
*)
let compose _ = failwith "not yet implemented" ;; 


(* Option and Exception Conversion *)

(*
Write a function opt_to_ext : (’a -> ’b option) -> exn -> (’a -> ’b) that
takes in a function f of type ’a -> ’b option and an exception. If the input function
returns None for an argument, the output function should raise the exception passed into
opt_to_ext. If the input function returns Some v on an argument, the output function
should return v for that argument.
*)

let opt_to_ext _ = failwith "not yet implemented" ;; 

(* 
Write a function ext_to_opt : (’a -> ’b) -> (’a -> ’b option) that converts
a function that (potentially) raises an exception to a function that returns a value having an
option type. If the input function raises any exception on an input, the output function should
return None. If the input function returns a value v for an input, the output function should
return Some v.
*)

let ext_to_opt = failwith "not yet implemented" ;; 


