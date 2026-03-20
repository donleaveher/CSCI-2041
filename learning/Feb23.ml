(* ===== Stream 基础定义 ===== *)

(* 构造流: this=当前元素, state=状态, next=生成下一个元素的函数 *)
let makeStream this state next =
  ((this, state), next) ;;

(* 取流的第一个元素 *)
let first ((this, _), _) = this ;;

(* 去掉第一个元素，返回剩余的流 *)
let rest ((this, state), next) =
  (next this state, next) ;;

(* 从流中取前 count 个元素，返回列表 *)
let rec take count stream =
  match count with
  | 0 -> []
  | _ -> first stream :: take (count - 1) (rest stream) ;;

(* ===== 流的实例 ===== *)

(* 自然数流: 0, 1, 2, 3, ... (不需要 state) *)
let naturals =
  makeStream 0 () (fun this _ -> (this + 1, ())) ;;

(* 阶乘流: 1, 1, 2, 6, 24, ... (state 记录下一个要乘的数) *)
let factorials =
  makeStream 1 1 (fun this state -> (this * state, state + 1)) ;;

(* 全1流: 1, 1, 1, 1, ... *)
let ones =
  makeStream 1 () (fun _ _ -> (1, ())) ;;

(* ===== 流的组合 ===== *)

(* 逐元素求和: sum left right 的第 i 个元素 = left[i] + right[i] *)
let sum left right =
  let this = first left + first right in
  let state = (rest left, rest right) in
  let next _ state =
    let (l, r) = state in
    (first l + first r, (rest l, rest r))
  in
  makeStream this state next ;;

(* ===== 流上的搜索 (注意: 可能不终止!) ===== *)

(* 测试流中是否存在满足 pred 的元素 *)
(* 警告: 若 pred 永远为 false, 此函数永远不会返回 *)
let isIn pred stream =
  let rec isInning stream =
    if pred (first stream)
    then true
    else isInning (rest stream)
  in isInning stream ;;

(* ===== 测试 ===== *)
(* take 5 naturals           => [0; 1; 2; 3; 4]        *)
(* take 5 factorials         => [1; 1; 2; 6; 24]       *)
(* take 5 (sum ones naturals) => [1; 2; 3; 4; 5]       *)
(* isIn (fun x -> x mod 2 = 1) naturals => true        *)