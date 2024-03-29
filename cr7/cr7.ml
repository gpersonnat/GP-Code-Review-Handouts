(*  Define a function (&&&) : bool Lazy.t -> bool Lazy.t -> bool.
It should behave like a short circuit Boolean AND. That is, lb1 &&& lb2 should first force lb1.
If the value is false, the function should return false. Otherwise, it should force lb2 and return its
value. *)

let (&&&) _ = failwith "" ;; 

(*  Define a value pow2 : int stream whose elements are the powers of two *)

(* Define a stream whose elements are the lowercase letters of the alphabet on repeat: a, b, c .....,
z, a, b, c ..., z ... Hint : The chr and code function in OCaml’s Char module might be helpful *)

let pow2 _ = failwith "" ;; 

(* Suppose we have the following type:
type flip = Heads | Tails
Define a stream of pseudorandom coin flips. *)

type flip = Heads | Tails

let flips _ = failwith "" ;; 

(*  Define a function expo_term : float -> float stream, that returns the stream of
the Taylor expansion of ex *)

let expo_term _ = failwith "" ;;

(*  Define a function total : int stream -> int stream, that takes in a stream <a;
b; c; ...> and outputs a stream of the running total of the input elements, i.e., <a; a +.
b; a +. b +. c; ...> *)

let total = failwith "" ;; 

(* Define a function infinite_delay : (’a -> ’a) list -> ’a list -> ’a Lazy.t
stream that takes a list of functions and arguments to those functions (of the same size),
and returns a repeating stream of delayed computations (functions applied to corresponding
argument). For instance, consider the function list to be [f; g; h] and the argument list to
be [x;y;z]. The output stream should represent the delayed computations of f x, g y,
h z, f x, g y, .... Though if we force computations in the stream, each computation
should only be computed once *)

let infinite_delay _ = failwith "" ;; 


(* 

Define a function \texttt{compute : ('a -> 'a list) -> 'a list -> unit -> 'a} 
that takes in lists of computations and returns a function that 
repeatedly returns the next computation each time the function is called. 
For example, if we have a function 
list \texttt{[f; g; h]} and \texttt{[x; y; z]}, 
then the function returned from compute should 
return \texttt{f x}, then \texttt{g y}, then \texttt{h z}, \texttt{f x}, and so on. 
*)

let compute _ = failwith "" ;; 



(* Define an ’a tree type for infinite trees. Each node in the tree can have any number of
children *)



(* Write a function numNodes that takes an ’a tree and a positive integer d and returns the
number of nodes in the ’a tree above depth d. We say that the root node of the tree is at
depth 0, its children are at depth 1, its grandchildren are at depth 2, and so on *)

let numNodes _ = failwith "" ;; 