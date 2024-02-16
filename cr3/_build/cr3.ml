
(* Define a type for a multiway tree, where each node can have any number of children *)


(* Define a function mapbt that given a function f and a mulitway tree tr, applies f to each
node in the tree and returns a transformed binary tree. *)

let mapbt _ = failwith "not yet implemented" ;; 
  

(* Define a function foldbt that walks the entire tree and performs an operation on each node
and the accumulator, returning the final value of the accumulator *)
let foldbt _ = failwith "not yet implemented" ;; 

(* A leaf is a node with no children. Define a function leaves that collects the 
leaves of a tree, returning them as a list *)
let leaves _ = failwith "not yet implemented" ;; 

(* Define a function zip_trees that takes two multiway trees and merges them into a new tree.
The trees should be merged in a way that corresponding nodes are combined into pairs. If
two trees are not the same shape, raise an Exception. *)
let zip_trees _ = failwith "not yet implemented" ; 

(* Suppose we want to represent a file system using algebraic data types
as follows: *)

type fileObj = File of string | Folder of string * fileObj list

(* A file system contains objects, which can be folders or files. Folders can contain files or other folders,
which we’ll refer to as subfolders. The folder in which a subfolder is contained is called the parent
folder *)

(* Define a function list_all that prints the name of all the files (each on a new line) contained
in a given fileObj (including those in all subfolders) *)
let list_all _ = failwith "not yet implemented" ;; 


