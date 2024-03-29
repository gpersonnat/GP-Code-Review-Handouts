type 'a stream_internal = Cons of 'a * 'a stream
 and 'a stream = 'a stream_internal Lazy.t ;;

let head (s : 'a stream) : 'a =
  let Cons (hd, _tl) = Lazy.force s in hd ;;

let tail (s : 'a stream) : 'a stream =
  let Cons (_hd, tl) = Lazy.force s in tl ;;
  
let rec first (n : int) (s : 'a stream) : 'a list =
  if n = 0 then []
  else head s :: first (n - 1) (tail s) ;;

let rec smap (f : 'a -> 'b)
             (s : 'a stream)
           : 'b stream =
  lazy (Cons (f (head s),
              smap f (tail s)));;

let rec smap2 (f : 'a -> 'b -> 'c)
              (s1 : 'a stream)
              (s2 : 'b stream)
            : 'c stream = 
  lazy (Cons (f (head s1) (head s2),
              smap2 f (tail s1) (tail s2))) ;;


let rec sfilter (pred : 'a -> bool) (s : 'a stream) : 'a stream =

  lazy (if pred (head s)
        then Cons ((head s), sfilter pred (tail s))
        else Lazy.force (sfilter pred (tail s))) ;;


let (&&&) (lb1 : bool Lazy.t) (lb2 : bool Lazy.t) : bool = 
  let lb1 = Lazy.force lb1 in 
  if lb1 = false then false 
  else Lazy.force lb2 ;;

(* Define a value pow2 :int stream whose elements are the powers of two.  *)


let rec pow2 : int stream = 
  lazy (Cons (1, smap (( * ) 2) pow2)) ;;
    
(* Define a stream whose elements are the lowercase letters of the alphabet on repeat: a, b, c ....., z, a, b, c ..., z ... Hint: 
 The chr and code function in OCaml's Char module might be helpful  *)

 let rec alpha_stream : char stream = 
  let alpha_size = 26 in
  let next_char (chr : char) : char = 
    let cde = Char.code chr in 
    let a_code = Char.code 'a' in 
    Char.chr (((cde + 1) - a_code) mod alpha_size + a_code) in 
  lazy (Cons ('a', smap next_char alpha_stream)) ;;

type flip = Heads | Tails
let rec random_stream : flip stream = 
  lazy (Cons ((if Random.int 2 = 0 then Heads else Tails), random_stream)) ;;


  (* Define a function expo_term : float -> float stream, that returns the stream of
  the Taylor expansion of ex. The Taylor expansion of ex is defined as: *)



let expo_term (x : float) : float stream = 
  let rec generate (prev : float) (n : int) : float stream = 
    lazy (
    let new_el = if n = 0 then 1. else  (x /.  (float_of_int n)) *. prev in 
    Cons ( new_el , generate new_el (n + 1))) in 
  generate 1. 0 ;;


let total (s : int stream) : int stream = 
  let rec generate (prev_sum : int) (s' : int stream) : int stream = 
    lazy (
      let curr_sum = head s' + prev_sum in 
      Cons (curr_sum , generate curr_sum (tail s'))) in 
  generate 0 s ;;

let rec infinite_delay (funcs : ('a -> 'a) list) (args : 'a list) : 'a Lazy.t stream =
  let rec aux (f : ('a -> 'a) list) (a : 'a list) : 'a Lazy.t stream =  
    match funcs, args with 
    | [], [] -> infinite_delay funcs args
    | [], _  
    | _, [] -> raise (Invalid_argument "function and arg lists not of eq length")
    | fh :: ft, ah :: at -> lazy (Cons (lazy (fh ah), aux ft at) ) in  
  aux funcs args;;

let rec compute (funcs : ('a -> 'a) list) (args : 'a list) : unit -> 'a = 
  let comp_stream = ref (infinite_delay funcs args) in 
  fun () -> 
    let Cons (h, t) = Lazy.force !comp_stream in 
    comp_stream := t; Lazy.force h ;;


type 'a tree_internal = 
  Node of 'a * 'a tree list and 
'a tree = 'a tree_internal Lazy.t ;;

(* Write a function numNodes that takes an ’a tree and a positive integer d and returns the
number of nodes in the ’a tree above depth d. We say that the root node of the tree is at
depth 0, its children are at depth 1, its grandchildren are at depth 2, and so on *)

let rec ones : int tree = lazy (Node (1, [ones; ones])) ;;

let rec numNodes (tr : 'a tree) (d : int) : int = 
  if d = 0 then 1 
  else 
  let Node (h, children) = Lazy.force tr in 
  1 + List.fold_left (fun acc x -> numNodes x (d - 1) + acc) 0 children ;;
