
let rec c n k =
  if k = 0 then 1
  else if n = 0 then 0
  else c (n-1) k + c (n-1) (k-1);;

let memyC n k =
  let table = Hashtbl.create ~random:false 1000 in
  let rec helper n k =
    if k = 0 then 1
    else if n = 0 then 0
    else
      match Hashtbl.find_opt table (n, k) with
      | Some v -> v
      | None ->
        let v = helper (n-1) k + helper (n-1) (k-1) in
        Hashtbl.add table (n, k) v;
        v
      in helper n k;;

(*
# * * * * *   val c : int -> int -> int = <fun>
val memyC : int -> int -> int = <fun>
# * *               val time : string -> (unit -> 'a) -> 'a = <fun>
#   * * * *     * *     c test1 0.000002 seconds
- : int = 1
#   * *     c test2 0.000002 seconds
- : int = 0
#   * *     c test3 0.000001 seconds
- : int = 1
#   * *     c test4 0.000001 seconds
- : int = 1
#   * *     c test5 0.000005 seconds
- : int = 70
#   * *     c test6 0.000001 seconds
- : int = 10
#   * *     *     c test7 26.561136 seconds
- : int = 847660528
#   * *     * * *     memyC test1 0.000003 seconds
- : int = 1
#   * *     memyC test2 0.000004 seconds
- : int = 0
#   * *     memyC test3 0.000001 seconds
- : int = 1
#   * *     memyC test4 0.000011 seconds
- : int = 1
#   * *     memyC test5 0.000010 seconds
- : int = 70
#   * *     memyC test6 0.000003 seconds
- : int = 10
#   * *         memyC test7 0.000056 seconds
- : int = 847660528
#   * * 
*)