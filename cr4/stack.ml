
exception EmptyStack


type 'a stack = 'a list 
let empty : 'a stack = [] 

let push (elt : 'a) (stk : 'a stack) : 'a stack = 
  elt :: stk 

let pop_helper (stk : 'a stack) : 'a * 'a stack = 
  match stk with 
  | [] -> raise EmptyStack
  | hd :: tl -> hd, tl 

let top (stk : 'a stack) : 'a = 
  stk |> pop_helper |> fst 

let pop (stk : 'a stack) : 'a stack = 
  stk |> pop_helper |> snd 