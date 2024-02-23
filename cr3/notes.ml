

(* 

Annoucnments 

1) Ps1 Feedback -> take a look! 

2) PS3 is due Wednesday 

Agenda 

1) Algebraic Data Types 


*)


(* 

Alternation and Conjuction 

1) Alternation - choosing between different alternatives from a set of alternatives (Variants) 

2) Conjuction - the combining of different values into one composite values 

*)

type color_label = 
| Red 
| Blue 
| Green 
| Black  ;; 

(* Alternation!! *)

(* 

Value Constructor - shows us how to construct values of some type
*)


type color = 
| Simple of color_label 
| RGB of int * int * int ;;

(* Conjunction: conjoing three integer values *)

let c : color = Simple Red  ;;

let d : color = RGB (5, 2, 3) ;;


(* 

Invariants - properties of types or code that we want to enforce 

*)

(* 

Dictionary -> map from keys to values 

Invariant : the # of keys = # of values 

*)

type ('key, 'value) dictionary = {keys : 'key list; values : 'value list} ;;

(* Example  *)

let d : (int, int) dictionary = {keys = [4; 5; 2]; values = [1; 8; 9]} ;;

let bad_dict : (int, int) dictionary = {keys = [4; 2]; values = [1; 8; 9]} ;; 

let string_dict : (int, string) dictionary = {keys = [5; 2]; values = ["cs50"; "cs51"]} ;;



type 'a option = 
  | None 
  | Some of 'a ;;

type ('key, 'value) dictionary = ('key * 'value) list ;; 


type ('key, 'value) dict_entry = {key : 'key; value : 'value} 
 and ('key, 'value) dictionary = ('key, 'value) dict_entry list ;;

 (* type typ1 = type_expr and typ2 = type_expr *)

(* Recursive Algebraic Data Types *)

type 'a list = 
  | Nil  (* [] *)
  | Cons of 'a * 'a list  (* 'a ::  'a list *)

let c : int list = Cons (5, Cons (2, Cons (3, Nil))) ;;

(* Binary Trees *)

type 'a bintree = 
| Empty 
| Node of 'a * 'a bintree * 'a bintree ;; 

let tree1 = Empty ;; 

let tree2 : int bintree = Node (2, Empty, Empty) ;; 

let tree3 : int bintree = Node (2, Empty, (Node (8, Empty, Empty))) ;;


let rec node_count (btree : 'a bintree) : int = 
  match btree with 
  | Empty -> 0 
  | Node (_, l, r) -> 1 + node_count l + node_count r ;;









