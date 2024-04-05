open NativeLazyStreams ;;

(*  Define a function (&&&) : bool Lazy.t -> bool Lazy.t -> bool.
It should behave like a short circuit Boolean AND. That is, lb1 &&& lb2 should first force lb1.
If the value is false, the function should return false. Otherwise, it should force lb2 and return its
value. *)

let (&&&) (lb1 : bool Lazy.t) (lb2 : bool Lazy.t) : bool = 
  let b1 = Lazy.force lb1 in 
  if b1 = false then false 
  else Lazy.force lb2 ;; 

(* Lazy.t - thunk 

let 'a thunk_internal = 
  | Evaluated of 'a 
  | Unevaluated of (unit -> 'a) 
and 'a Lazy.t = 'a thunk_internal ref ;; 


*)


  let (&&&) (lb1 : bool Lazy.t) (lb2 : bool Lazy.t) : bool = 
    let b1 = Lazy.force lb1 in 
    if b1 then Lazy.force lb2  
    else false ;;

(* 1)  Define a value pow2 : int stream whose elements are the powers of two *)

let rec pow2 : int stream = 
  lazy ( Cons (1,  smap (( * ) 2) pow2  ))
  ;; 

let rec nats : int stream = 
  lazy (Cons (0, smap succ nats)) ;;

(* let pow2 : int stream =
  let generate (n : int) : int stream = 
    lazy (Cons (int_of_float (2. **. (float_of_int n)), generate (n + 1))) in 
  generate 0 ;;  *)


(* 1; 2; 4; 8; 16, ... *)

(* Define a stream whose elements are the lowercase letters of the alphabet on repeat: a, b, c .....,
z, a, b, c ..., z ... Hint : The chr and code function in OCaml’s Char module might be helpful *)

let rec alphabet : char stream = 
  let next_char (chr : char) : char = 
    let alpha_size = 26 in 
    let cde = Char.code chr in 
    let a_code = Char.code 'a' in 
    Char.chr (((cde + 1) - a_code) mod alpha_size + a_code)
  in lazy (Cons ('a', smap next_char  alphabet))
  

(* a, b, c, ...., z, a, b , c *)

(* Suppose we have the following type:
type flip = Heads | Tails
Define a stream of pseudorandom coin flips. 
Hint : Random.int bound -> returns an integer in [0, bound)
*)
type flip = Heads | Tails

let rec random_stream : flip stream = 
  lazy (Cons ((if Random.int 2 = 0 then Heads else Tails), random_stream)) ;;

let rec random_stream : flip stream = 
  if Random.int 2 = 0 then 
  lazy (Cons (Heads, random_stream))
  else lazy (Cons (Tails, random_stream)) ;;


let flips : flip stream = 
  let rec generate () : flip stream = 
    if Random.int 2 = 0 then lazy (Cons (Heads, generate ()))
    else lazy (Cons (Tails, generate ())) in 
  generate () ;; 


(* 

Heads, Tails, Heads, Heads, Tails, Tails, ....

*)

(*  Define a function expo_term : float -> float stream, that returns the stream of
the Taylor expansion of ex *)

(* e^x = 1 + x x^2 / 2! + x^3 / 3! 

generate n -> x^n / n! -> add to the stream

*)

let rec factorial (n : int) : int = 
  if n = 0 then 1 
  else n * factorial (n - 1) ;;

let rec power (x : float) (n : int) : float = 
  if n = 0 then 1. 
  else x *. power x (n - 1) ;;

let expo_term1 (x : float) : float stream = 
  let rec generate (n : int) : float stream = 
    lazy ( Cons ((power x n) /. ( float_of_int (factorial n)) , generate (n + 1) )) in 
  generate 0  
  ;;

(* expo1_term - computing elements in the stream is more costly the first time bc - O(n) 

expo_term - less costly the first time bc it takes O(1) time to compute elements 

Second time expo1_term and expo_term2 take the same time to generate elements bc values are memoized

*)
  let expo_term2 (x : float) : float stream = 
    let rec generate (x_n_1 : float) (n : int) : float stream =
      lazy  (let x_n = if n = 0 then 1. else (x /. (float_of_int n)) * x_n_1 in 
      (Cons (x_n, generate x_n (n + 1)))) in 
    generate 1. 0. 
  

(* 
n > 1 

x_n = x_{n - 1} * (x / n) 

x_{n - 1} = x^{n - 1} / (n - 1)! 


x_n = x^n / n! 

x_n = x^n / n!

*)



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

(* [f; g; h]  [x; y; z] 

f x -> v 

-> f x, g y, h z, f x, .... 


[ lazy_1 (f x), lazy_2 (g y), lazy_3 (h z)] -> (lazy_1 (f x)), lazy_2 (g y), lazy_3 (h z), (lazy_1 (f x)), lazy_2 (g y), ...
*)

let rec infinite_delay (funcs : (('a -> 'a) list)) (args : 'a list) : 'a Lazy.t stream = 
  let rec aux (f : ('a -> 'a) list) (a : 'a list) : 'a Lazy.t stream = 
    match f, args with 
    | [], [] -> infinite_delay funcs args 
    | [], _ 
    | _, [] -> raise (Invalid_argument "not same length")
    | fh :: ft, ah :: at -> lazy (Cons (lazy (fh ah), aux ft at)) 
  in aux funcs args ;; 


let rec infinite_delay (funcs : ('a -> 'a) list) (args : 'a list) : 'a Lazy.t stream = 
  if Length.length funcs <> List.length args then raise (Invalid_argument "not same length")
  else 
  ( 
    let delayed_comps : 'a Lazy.t list = List.map2 (fun f x -> lazy (f x) ) funcs args in 
    let generate_seq (lst : 'a Lazy.t list) : 'a Lazy.t stream = 
      let aux (l : 'a Lazy.t list) : 'a Lazy.t stream =
        match l with 
        | [] -> generate_seq lst 
        | h ::  t -> lazy (Cons (h, aux t)) in 
    aux lst in 
    generate_seq delayed_comps 
  ) ;;

(* 

Define a function compute : ('a -> 'a list) -> 'a list -> unit -> 'a
that takes in lists of computations and returns a function that 
repeatedly returns the next computation each time the function is called. 
For example, if we have a function 
list [f; g; h] and [x; y; z], 
then the function returned from compute should 
return f x, then g y, then h z, f x, and so on. 
*)

(* 
infinite_delay funcs args -> f x, g y, h z, ... 



*)
let compute (funcs : ('a -> 'a) list) (args : 'a list) : unit -> 'a =
  let comp_stream = ref (infinite_delay funcs args) in 
  fun () -> 
    let Cons (c, t) = Lazy.force !comp_stream in 
    comp_stream := t; Lazy.force c 
;; 




(* Define a function compute : (’a -> ’a list) -> ’a list -> ’a that takes in a
list of computations and repeatedly returns the next computation each time the function is
called. For example, if we have a function list [f; g; h] and [x; y; z], then this function
should return f x, then g y, then h z, f x, and so on. *)
let compute _ = failwith "" ;; 

(* Define an ’a tree type for infinite trees. Each node in the tree can have any number of
children *)

type 'a tree = Node of 'a * 'a tree list ;;

type 'a stream_internal = Cons of 'a * 'a stream
and 'a stream = 'a stream_internal Lazy.t ;;

type 'a tree_internal = Node of 'a * 'a tree list 
and 'a tree = 'a tree_internal Lazy.t ;;

(* 

1
2     3
4 5  6 7  
*)



(* Write a function numNodes that takes an ’a tree and a positive integer d and returns the
number of nodes in the ’a tree above depth d. We say that the root node of the tree is at
depth 0, its children are at depth 1, its grandchildren are at depth 2, and so on *)

let rec numNodes (tr : 'a tree) (d : int) : int = 
  if d = 0 then 1 
  else 
  let Node (_, children) = Lazy.force tr in 
  1 + List.fold_left (fun acc x -> numNodes x (d - 1) + acc) 0 children ;; 