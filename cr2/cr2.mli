

val remove_duplicates : 'a list -> 'a list ;; 


(* 
Write a function tranpose that takes a list of lists that represents a matrix (each list corre-
sponds to a row) and returns the transpose of the matrix. 
*)

val transpose : 'a list list -> 'a list list ;; 

(* 
Write a polymorphic function matrix_mult that takes in a function to add elements two
elements, a function to multiply elements, an initial value used to add and multiply elements
(i.e. think of this as the zero element), and two matrices. The result should be the matrix
formed by multiplying the two matrices together.
*)

val matrix_mult : ('a -> 'a -> 'a) -> ('a -> 'a -> 'a) -> 'a -> 'a list list -> 'a list list -> 'a list list ;; 


(* Define a function repeat f x n that applies f to x exactly
n times. *)
val repeat : ('a -> 'a) -> 'a -> int -> 'b 

(* 
Define a function compose : (’a -> ’a) list -> (’a -> ’a) that takes a list of 
functions and returns the composition of those functions. For example, suppose we 
had a list of functions [ f ; g; h]. compose should return the function f ◦ g ◦ h
or ℓ(x) = f (g(h(x))
*)
val compose : ('a -> 'a) list -> ('a -> 'a) ;; 



(* Option and Exception Conversion *)

(*
Write a function opt_to_ext : (’a -> ’b option) -> exn -> (’a -> ’b) that
takes in a function f of type ’a -> ’b option and an exception. If the input function
returns None for an argument, the output function should raise the exception passed into
opt_to_ext. If the input function returns Some v on an argument, the output function
should return v for that argument.
*)

val opt_to_ext : ('a -> 'b option) -> exn -> ('a -> 'b)  ;; 

(* 
Write a function ext_to_opt : (’a -> ’b) -> (’a -> ’b option) that converts
a function that (potentially) raises an exception to a function that returns a value having an
option type. If the input function raises any exception on an input, the output function should
return None. If the input function returns a value v for an input, the output function should
return Some v.
*)

val ext_to_opt : ('a -> 'b) -> ('a -> 'b option) ;; 


