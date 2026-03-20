
let makeStream this state next =
  ((this, state), next) ;;


let first ((this, state), next) =
  this ;;

(* REST. Return a stream with its first element removed. *)

let rest ((this, state), next) =
  (next this state, next) ;;

(* TAKE. Return a list of the first COUNT elements TAKEn from STREAM. *)

let rec take count stream =
  match count
  with 0 -> [] |
      _ -> (first stream) :: take (count - 1) (rest stream) ;;

let odds = makeStream 1 1 (fun this state -> (this + 2, state));;

let rec trim count stream = 
  match count with
  | 0 -> stream
  | _ -> trim (count - 1) (rest stream);;

let scale factor stream = 
  makeStream (factor* (first stream))
            (rest stream)
            (fun this state -> (factor*(first state), rest state));;

let sum left right = 
  makeStream (first left + first right)
              (rest left, rest right)
              (fun this (l_stream, r_stream) -> (first l_stream + first r_stream, (rest l_stream, rest r_stream)));;


(*
The output in terminal
# * * * * * * *     * * *   val makeStream : 'a -> 'b -> 'c -> ('a * 'b) * 'c = <fun>
val first : ('a * 'b) * 'c -> 'a = <fun>
val rest : ('a * 'b) * ('a -> 'b -> 'c) -> 'c * ('a -> 'b -> 'c) = <fun>
val take : int -> ('a * 'b) * ('a -> 'b -> 'a * 'b) -> 'a list = <fun>
val odds : (int * int) * (int -> '_weak1 -> int * '_weak1) = ((1, 1), <fun>)
val trim :
  int ->
  ('a * 'b) * ('a -> 'b -> 'a * 'b) -> ('a * 'b) * ('a -> 'b -> 'a * 'b) =
  <fun>
val scale :
  int ->
  (int * 'a) * (int -> 'a -> 'b) ->
  (int * ('b * (int -> 'a -> 'b))) *
  ('c -> (int * 'd) * (int -> 'd -> 'e) -> int * ('e * (int -> 'd -> 'e))) =
  <fun>
val sum :
  (int * 'a) * (int -> 'a -> 'b) ->
  (int * 'c) * (int -> 'c -> 'd) ->
  (int * (('b * (int -> 'a -> 'b)) * ('d * (int -> 'c -> 'd)))) *
  ('e ->
   ((int * 'f) * (int -> 'f -> 'g)) * ((int * 'h) * (int -> 'h -> 'i)) ->
   int * (('g * (int -> 'f -> 'g)) * ('i * (int -> 'h -> 'i)))) =
  <fun>
#     val makeStream : 'a -> 'b -> 'c -> ('a * 'b) * 'c = <fun>
#         val first : ('a * 'b) * 'c -> 'a = <fun>
#         val rest : ('a * 'b) * ('a -> 'b -> 'c) -> 'c * ('a -> 'b -> 'c) = <fun>
#             val take : int -> ('a * 'b) * ('a -> 'b -> 'a * 'b) -> 'a list = <fun>
#   *       val naturals : (int * unit) * (int -> '_weak2 -> int * unit) =
  ((0, ()), <fun>)
#   * * * *     - : int = 1
#   - : int = 3
#   - : int = 5
#   - : int list = [1; 3; 5; 7; 9; 11; 13]
#   val but1st5 : (int * unit) * (int -> unit -> int * unit) = ((5, ()), <fun>)
#   - : int = 5
#   - : int = 6
#   - : int = 7
#   - : int list = [5; 6; 7; 8; 9; 10; 11]
#   val byFives :
  (int * ((int * unit) * (int -> unit -> int * unit))) *
  ('_weak3 ->
   (int * '_weak4) * (int -> '_weak4 -> '_weak5) ->
   int * ('_weak5 * (int -> '_weak4 -> '_weak5))) =
  ((0, ((1, ()), <fun>)), <fun>)
#   - : int = 0
#   - : int = 5
#   - : int = 10
#   - : int list = [0; 5; 10; 15; 20; 25; 30]
#   val natsPlusByFives :
  (int *
   (((int * unit) * (int -> unit -> int * unit)) *
    ((int * ((int * unit) * (int -> unit -> int * unit))) *
     (int ->
      (int * unit) * (int -> unit -> int * unit) ->
      int * ((int * unit) * (int -> unit -> int * unit)))))) *
  ('_weak6 ->
   ((int * '_weak7) * (int -> '_weak7 -> '_weak8)) *
   ((int * '_weak9) * (int -> '_weak9 -> '_weak10)) ->
   int *
   (('_weak8 * (int -> '_weak7 -> '_weak8)) *
    ('_weak10 * (int -> '_weak9 -> '_weak10)))) =
  ((0, (((1, ()), <fun>), ((5, ((2, ()), <fun>)), <fun>))), <fun>)
#   - : int = 0
#   - : int = 6
#   - : int list = [0; 6; 12; 18; 24; 30; 36]

*)

(* 1. makeStream: 构造一个流 (Stream)
   参数 this: 流的当前值 (当前吐出的数据)
   参数 state: 当前的内部状态 (用于辅助计算下一个值)
   参数 next: 一个函数，接收 (this, state) 并返回下一个 (新this, 新state) 的元组
   返回: 一个嵌套元组 ((当前值, 状态), next函数)
*)
let makeStream this state next =
  ((this, state), next) ;;

(* 2. first: 获取流的当前（第一个）元素
   只需要从嵌套元组中提取出 `this` 即可。
*)
let first ((this, state), next) =
  this ;;

(* 3. rest: 获取流的“剩余部分”（即让流往前走一步）
   通过调用 `next` 函数算出新的 (this, state)，然后和原本的 `next` 函数重新打包成一个新流。
*)
let rest ((this, state), next) =
  (next this state, next) ;;

(* 4. take: 从流中“提取”前 count 个元素，并转换成普通的 OCaml 列表 (List)
   因为流是无限的，我们必须用 take 来限制获取的数量，否则程序会永远运行下去。
*)
let rec take count stream =
  match count with 
  | 0 -> []  (* 提取 0 个，返回空列表 *)
  | _ -> 
      (* 把当前流的第一个元素拿出来，加到列表中，
         然后递归地对 rest stream (剩下的流) 继续 take (count - 1) 个元素 *)
      (first stream) :: take (count - 1) (rest stream) ;;

(* 5. odds: 一个具体的无限流例子（生成所有奇数: 1, 3, 5, 7...）
   - 初始 this = 1
   - 初始 state = 1 (这里其实没用到 state 的特殊性质，只是作为占位符)
   - next 函数 = (fun this state -> (this + 2, state))，每次把当前值 + 2。
*)
let odds = makeStream 1 1 (fun this state -> (this + 2, state));;

(* 6. trim: “丢弃”流的前 count 个元素，返回从第 count+1 个元素开始的新流
   （相当于让流空转 count 次）
*)
let rec trim count stream = 
  match count with
  | 0 -> stream  (* 丢弃 0 个，直接返回当前流 *)
  | _ -> 
      (* 递归地对流调用 rest，并且让需要丢弃的数量 - 1 *)
      trim (count - 1) (rest stream);;

(* 7. scale: 将流中的所有元素放大 factor 倍
   【高阶玩法】：注意看，这里的 state 传入的是 `rest stream`（底层流的剩余部分）。
   - 当前值: factor * (底层流的当前值)
   - 状态: 底层流前进后的样子 (rest stream)
   - next 函数: 每次取出状态(即底层流)的 first 进行放大，并把底层流的 rest 作为新的状态。
*)
let scale factor stream = 
  makeStream (factor * (first stream))
             (rest stream)
             (fun this state -> (factor * (first state), rest state));;

(* 8. sum: 将两个流按位置对应相加（比如流A的第1项 + 流B的第1项 ...）
   【高阶玩法】：这里的 state 传入的是一个元组 `(rest left, rest right)`，即两个底层流的组合。
   - 当前值: 左流的 first + 右流的 first
   - 状态: (左流的 rest, 右流的 rest)
   - next 函数: 每次取出状态里两个流的 first 相加，然后将两个流同时 rest 作为新的状态。
*)
let sum left right = 
  makeStream (first left + first right)
             (rest left, rest right)
             (fun this (l_stream, r_stream) -> 
                (first l_stream + first r_stream, (rest l_stream, rest r_stream)));;