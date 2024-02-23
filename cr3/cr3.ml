
(* Define a type for a multiway tree, where each node can have any number of children *)

type 'a bintree = 
  | Empty 
  | Node of 'a * 'a bintree * 'a bintree ;;

type 'a tree =  Tree of 'a * 'a tree list ;;

let t1 : int tree = Tree (5, [ Tree (2, []); Tree (3, []); Tree (10, []) ]) ;;

(* >= 1 node *)

(* Define a function mapbt that given a function f and a mulitway tree tr, applies f to each
node in the tree and returns a transformed multiway tree. *)

let rec mapbt (f : 'a -> 'b) (tr : 'a tree) : 'b tree  = 
  let Tree (root, children) = tr in 
  Tree (f root,  List.map (mapbt f) children) ;; 

(* 

type 'a list = Nil | Cons of 'a * 'a list 

*)
  
(* 

Note : Pattern much on Alegbraic data types that have >1 value constructor 

Here, we don't need to pattern much bc there's only one value constructor

*)

(* Define a function foldbt that walks the entire tree and performs an operation on each node
and the accumulator, returning the final value of the accumulator *)

let rec foldbt (f : 'acc -> 'a -> 'acc) (init : 'acc) (tr: 'a tree) : 'acc =
  let Tree(root, children) = tr in
  let apply_root = f init root in
  List.fold_left (fun acc child -> foldbt f acc child) apply_root children ;;

(* A leaf is a node with no children. Define a function leaves that collects the 
leaves of a tree, returning them as a list *)
let rec leaves (tr : 'a tree) : 'a list = 
  let Tree(root, children) = tr in 
  if children = [] then [root] 
  else List.fold_left (fun acc child -> acc @ (leaves child)) [] children ;; 

(* Define a function zip_trees that takes two multiway trees and merges them into a new tree.
The trees should be merged in a way that corresponding nodes are combined into pairs. If
two trees are not the same shape, raise an Exception. *)

(* List.map2  *)
let rec zip_trees (t1 : 'a tree) (t2 : 'b tree) : ('a * 'b) tree = 
  let Tree(r1, c1), Tree(r2, c2) = t1, t2 in 
  try 
    Tree((r1, r2), List.map2 zip_trees c1 c2) 
  with 
  _ -> raise (Invalid_argument "trees are not of the same shape");;

(* Suppose we want to represent a file system using algebraic data types
as follows: *)

type fileObj = File of string | Folder of string * fileObj list

(* A file system contains objects, which can be folders or files. Folders can contain files or other folders,
which we’ll refer to as subfolders. The folder in which a subfolder is contained is called the parent
folder *)

(* Define a function list_all that prints the name of all the files (each on a new line) contained
in a given fileObj (including those in all subfolders) *)
let list_all _ = failwith "not yet implemented" ;; 


