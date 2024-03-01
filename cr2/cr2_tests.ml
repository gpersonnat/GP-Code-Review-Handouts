
open Cr2 ;; 

open CS51Utils.Absbook ;;

let rec max_list_opt (lst : int list) : int option =
  match lst with
  | [] -> None
  | head :: tail ->
    match (max_list_opt tail) with
    | None -> Some head
     | Some max_tail -> Some (max head max_tail) ;;
   let rec max_list (lst : int list) : int =
    match lst with
    | [] -> raise (Invalid_argument "max_list: empty list")
    | [elt] -> elt
    | head :: tail -> max head (max_list tail) ;;

(* Write unit tests here! *)


let opt_to_ext_test () = 
  unit_test (try 
              let _ = opt_to_ext max_list_opt (Invalid_argument "empty list") [] in 
              false 
            with 
            | Invalid_argument _ -> true 
            | _ -> false ) "opt_to_ext max_list_opt []";
           
  unit_test ((opt_to_ext max_list_opt (Invalid_argument "empty list") [5;2]) = 5) "opt_to_ext non-empty list" 
;;

let ext_to_otp_test () = 
  unit_test(ext_to_opt max_list [] = None) "ext_to_opt max empty list"; 
  unit_test(ext_to_opt max_list [5; 2; 3; -4] = Some 5) "ext_to_opt max non-empty" 
;;

opt_to_ext_test () ;;

ext_to_otp_test ;;





  
