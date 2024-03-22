exception EmptyStack

type 'a stack 

val empty : 'a stack 
val push: 'a -> 'a stack -> 'a stack 
val top: 'a stack -> 'a 
val pop: 'a stack -> 'a stack
