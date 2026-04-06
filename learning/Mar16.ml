(* Transfer List to Lazy List *)

type 'a eagerList =
  | Empty
  | Node of 'a * 'a eagerList

(* 
  Node (1, Node (2, Node (3, Empty))) 
*)

type 'a lazyList = 
  | Empty
  | Node of 'a * ('a lazyList Lazy.t)

let lazyHd l = 
  match l with
  | Empty -> failwith "empty"
  | Node (h, _) -> h

let lazyTl l = 
  match l with
  | Empty -> failwith "empty"
  | Node (_, t) -> Lazy.force t


open Lazy

let mylist =
  Node (1,
    lazy (
      Node (2,
        lazy (
          Node (3, lazy Empty)
        )
      )
    )
  )

(* An infinite sequence *)
let rec from n =
  Node (n, lazy (from (n + 1)))

