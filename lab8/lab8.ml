module type Associaty =
  sig
    type ('k, 'v) t = Empty | Pair of 'k * 'v * ('k, 'v) t
    exception NoSuchKey
    val delete : ('k, 'v) t -> 'k -> ('k, 'v) t
    val get : ('k, 'v) t -> 'k -> 'v
    val make : unit -> ('k, 'v) t
    val put : 'k -> 'v -> ('k, 'v) t -> ('k, 'v) t
  end ;;

module Association : Associaty =
  struct
    type ('k, 'v) t = Empty | Pair of 'k * 'v * ('k, 'v) t
    exception NoSuchKey
    let error () = raise NoSuchKey

    let rec delete pairs key =
      match pairs with
      | Empty -> error ()
      | Pair (k, v, rest) ->
        if k = key then rest
        else Pair (k, v, delete rest key)

    let rec get pairs key =
      match pairs with
      | Empty -> error ()
      | Pair (k, v, rest) ->
        if k = key then v
        else get rest key

    let make () = Empty

    let put key value pairs = Pair (key, value, pairs)
  end ;;

(*
# * *     * *     *   module type Associaty =
  sig
    type ('k, 'v) t = Empty | Pair of 'k * 'v * ('k, 'v) t
    exception NoSuchKey
    val delete : ('k, 'v) t -> 'k -> ('k, 'v) t
    val get : ('k, 'v) t -> 'k -> 'v
    val make : unit -> ('k, 'v) t
    val put : 'k -> 'v -> ('k, 'v) t -> ('k, 'v) t
  end
module Association : Associaty
#   val a : ('a, 'b) Association.t = Association.Empty
#               val a0 : (int, string) Association.t =
  Association.Pair (1, "I",
   Association.Pair (2, "II",
    Association.Pair (3, "III",
     Association.Pair (4, "IIII",
      Association.Pair (5, "V", Association.Empty)))))
# * * * * *           - : string = ""
#     - : string = "I"
#     - : string = "II"
#     - : string = "III"
#     - : string = "IIII"
#     val a1 : (int, string) Association.t =
  Association.Pair (4, "IV",
   Association.Pair (1, "I",
    Association.Pair (2, "II",
     Association.Pair (3, "III",
      Association.Pair (4, "IIII",
       Association.Pair (5, "V", Association.Empty))))))
# * * * * * *     - : string = "I"
#     - : string = "IV"
#     val a2 : (int, string) Association.t =
  Association.Pair (1, "I",
   Association.Pair (2, "II",
    Association.Pair (3, "III",
     Association.Pair (4, "IIII",
      Association.Pair (5, "V", Association.Empty)))))
# * * * * *     - : string = "I"
#     - : string = "IIII"
#     val a3 : (int, string) Association.t =
  Association.Pair (4, "IV",
   Association.Pair (1, "I",
    Association.Pair (2, "II",
     Association.Pair (3, "III",
      Association.Pair (4, "IIII", Association.Empty)))))
# * * * * *         - : string = ""
#   
*)