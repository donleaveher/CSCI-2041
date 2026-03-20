type 's lazyList = 
  | LazyEmpty
  | LazyNode of 's Lazy.t * 's lazyList Lazy.t

exception LazyListError

let lazyCons h t = LazyNode (h, t)

let lazyHead l = 
  match l with
  | LazyEmpty -> raise LazyListError
  | LazyNode (h, t) -> Lazy.force h;;

let lazyTail l = 
  match l with 
  | LazyEmpty -> raise LazyListError
  | LazyNode (h, t) -> Lazy.force t;;

let rec lazyTake l n = 
  if n = 0 then []
  else (lazyHead l) :: lazyTake (lazyTail l) (n-1);; 

(*
# * * * * *     #   type 's lazyList = LazyEmpty | LazyNode of 's Lazy.t * 's lazyList Lazy.t
exception LazyListError
val lazyCons : 'a Lazy.t -> 'a lazyList Lazy.t -> 'a lazyList = <fun>
val lazyHead : 'a lazyList -> 'a = <fun>
val lazyTail : 'a lazyList -> 'a lazyList = <fun>
val lazyTake : 'a lazyList -> int -> 'a list = <fun>
#   * * *     *                   val lazyInts : int -> int -> int lazyList = <fun>
#   * * *               val lazyFibs : unit -> int lazyList = <fun>
#                         val strings : string lazyList = LazyNode (lazy "I'm", <lazy>)
#       - : string = "I'm"
#   *     - : string = "so"
#   *     - : string = "lazy"
#   *               Oops.
- : string = ""
#   * *     - : string list = ["I'm"; "so"; "lazy"]
#   *     val oneThruNine : int lazyList = LazyNode (<lazy>, <lazy>)
#       Computed integer 3
Computed integer 2
Computed integer 1
- : int list = [1; 2; 3]
#   * * * *     Computed integer 9
Computed integer 8
Computed integer 7
Computed integer 6
Computed integer 5
Computed integer 4
- : int list = [1; 2; 3; 4; 5; 6; 7; 8; 9]
#   * * * * * * *     - : int list = [1; 2; 3; 4; 5; 6; 7; 8; 9]
#   *     val allTheFibs : int lazyList = LazyNode (<lazy>, <lazy>)
#       Computed Fibonacci 34
Computed Fibonacci 21
Computed Fibonacci 13
Computed Fibonacci 8
Computed Fibonacci 5
Computed Fibonacci 3
Computed Fibonacci 2
Computed Fibonacci 1
Computed Fibonacci 1
Computed Fibonacci 0
- : int list = [0; 1; 1; 2; 3; 5; 8; 13; 21; 34]
#   * * * * * * * * * * *     - : int list = [0; 1; 1; 2; 3; 5; 8; 13; 21; 34]
*)