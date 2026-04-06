
type
  thing =
    Cons of thing * thing |
    Nil |
    Number of int |
    Symbol of string ;;

let rec every predicate elements = 
  match elements with
  | Cons(head, tail) -> if predicate head then every predicate tail else false
  | _ -> true;;


let equalThings a b =
  match (a, b) with
  | (Nil, Nil) -> true
  | (Number x, Number y) -> x = y
  | (Symbol x, Symbol y) -> x = y
  | _ -> false;;


let rec substitute elements old newThing = 
  match elements with
  | Cons(head, tail) -> 
    let newHead = if equalThings head old then newThing else head 
    in Cons (newHead, substitute tail old newThing)
  | _ -> Nil;;


let rec questyEqual left right = 
  match left with
  | Symbol "?" -> true
  | Cons(lhead, ltail) -> 
    (
      match right with
      | Cons (rhead, rtail) -> questyEqual lhead rhead && questyEqual ltail rtail
      | _ -> false
    )
  | _ -> left = right;;


