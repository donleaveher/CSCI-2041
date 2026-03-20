(* The basic of Array in Ocaml *)
Array.make 5 0 (* => [| 0; 0; 0; 0; 0 |]，int array *)

let memfib = 
  let t = Array.make 50 (-1) in
  let rec fib n = 
    if t.(n) > -1
      then t.(n)
    else (t.(n) <- (
      match n with
      | 0 -> 0
      | 1 -> 1
      | _ -> fib (n-2) + fib (n-1));
      t.(n)
    )
  in fib;;
    

let time s f x = 
  let t0 = Sys.time () in
  let y = f x in
  let t1 = Sys.time() in
  Printf.printf "%s %f" s (t1 -. t0);
  y ;;

time "fib test" (fun () -> memfib 46) ();;
