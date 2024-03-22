
(* Define a type for a multiway tree, where each node can have any number of children *)

type 'a multi_tree = Tree of 'a * 'a multi_tree list ;; 


(* Define a function mapbt that given a function f and a mulitway tree tr, applies f to each
node in the tree and returns a transformed binary tree. *)

let rec mapbt (f: 'a -> 'b) (tr: 'a multi_tree) : 'b multi_tree = 
  let Tree(root, children) = tr in 
  let new_children = children |> List.map (mapbt f) 
  in Tree(f root, new_children) ;; 
  

(* Define a function foldbt that walks the entire tree and performs an operation on each node
and the accumulator, returning the final value of the accumulator *)
let rec foldbt (f : 'acc -> 'a -> 'acc) (init : 'acc) 
               (tr: 'a multi_tree) : 'acc = 
  let Tree(root, children) = tr in 
  let apply_root = f init root in 
  List.fold_left (foldbt f) apply_root children ;; 

(* A leaf is a node with no children. Define a function leaves that collects the 
leaves of a tree, returning them as a list *)
let rec leaves (tr : 'a multi_tree) : 'a list = 
  let Tree(root, children) = tr in 
  match children with 
   | [] -> [root]
   | _children -> List.fold_left (fun acc child -> acc @ leaves child) [] children ;; 

(* Define a function zip_trees that takes two multiway trees and merges them into a new tree.
The trees should be merged in a way that corresponding nodes are combined into pairs. If
two trees are not the same shape, raise an Exception. *)
let rec zip_trees (tr1: 'a multi_tree) (tr2: 'b multi_tree) : ('a * 'b) multi_tree = 
  let Tree(root1, children1) = tr1 in 
  let Tree(root2, children2) = tr2 in
  try 
   Tree((root1, root2), 
            List.map2 zip_trees children1 children2) 
  with 
   | Invalid_argument _ -> raise 
                            (Invalid_argument 
                              "Error: Input trees are of different shape") ;; 

(* Suppose we want to represent a file system using algebraic data types
as follows: *)

type fileObj = File of string | Folder of string * fileObj list

(* A file system contains objects, which can be folders or files. Folders can contain files or other folders,
which we’ll refer to as subfolders. The folder in which a subfolder is contained is called the parent
folder *)

(* Define a function list_all that prints the name of all the files (each on a new line) contained
in a given fileObj (including those in all subfolders) *)
let rec list_all (f : fileObj) : unit = 
  match f with 
  | File filename -> Printf.printf "%s\n" filename
  | Folder(folder_name, objects) -> List.iter list_all objects ;; 


