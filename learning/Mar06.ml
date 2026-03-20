let f a b =
  if a = 0 then 0
  else a * (b ());;
(* 
  If call b multiple times 
  Use let to bind the temp variable
*)
let f a b =
  if a = 0 then 0
  else a * (b ()) * (b ());;

let f a b =
  if a = 0 then 0
  else let b' = b () in a * b' * b';;


(*
  Data structure: Future
*)

type 'base functionOrvalue = 
  | FutureFunction of (unit -> 'base)
  | FutureValue of 'base ;;

type 'base future = 
  | Future of ('base functionOrvalue) ref ;;

let makeFuture func = 
  Future (ref (FutureFunction func));;

let realize future = 
  match future with
  | Future variable ->
    (
      match !variable with
      | FutureFunction func ->
        let value = func () in
        variable := FutureValue value;
        value
      | FutureValue value -> value
    );;

let g a b =       (* b 是一个 future *)
  if a = 0
  then 0
  else (realize b) * (realize b) * a ;;

g 2 (makeFuture (fun () -> 1)) ;;

let side_effect () =
  Printf.printf "I was called!\n" ;
  10 ;;

let b = lazy (side_effect ()) ;;

Lazy.force b ;;
(* 打印 "I was called!", 返回 10 *)

Lazy.force b ;;
(* 不打印任何东西, 直接返回 10 *)

Lazy.force b ;;
(* 不打印任何东西, 直接返回 10 *)