(* Association List *)
let memoize f = 
  let t = ref[] in
  let rec memoizing p x = 
    match p with 
    | [] -> 
      let y = f x in 
      t := (x, y) :: !t;
      y
    | (x', y)::p' ->
      if x' = x then y
      else memoizing p' x
    in (fun x-> memoizing (!t) x);;

(* Improve *)
let memoize z f = 
  let table = Array.make z (-1) in
  let rec memoized n = 
    if table.(n) > 0 then table.(n)
    else (table.(n) <- f n memoized;
      table.(n))
  in memoized ;;

let memyFib = 
  memoize 50 (
    fun n recurse ->
      match n with
      | 0 -> 0
      | 1 -> 1
      | _ -> recurse (n - 2) + recurse (n - 1)
  );;
