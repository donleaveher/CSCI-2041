let rec choose etc things = 
  match things with
  | []->()
  | head::tail->etc head; choose etc tail;;

let rec allbut things thing = 
  match things with
  | [] -> []
  | head::tail ->
    if head == thing then tail
    else head :: allbut tail thing;;

let permute etc unpermutedThings = 
  let rec permuting etc permutedThings unpermutedThings = 
    match unpermutedThings with
    | []->etc permutedThings
    | _ -> choose (fun thing -> 
      permuting etc (thing::permutedThings) (allbut unpermutedThings thing)) 
      unpermutedThings
  in permuting etc [] unpermutedThings

(*
# * * * * * * *   
val choose : ('a -> 'b) -> 'a list -> unit = <fun>
val allbut : 'a list -> 'a -> 'a list = <fun>
val permute : ('a list -> unit) -> 'a list -> unit = <fun>
# *  val printThings : ('a -> 'b, out_channel, unit) format -> 'a list -> unit = <fun>
#   * *     []
- : unit = ()
#   [1 ; 2]
- : unit = ()
#   [0 ; 2]
- : unit = ()
#   [0 ; 1]
- : unit = ()
#   [0 ; 1 ; 2]
- : unit = ()
#   *     - : unit = ()
#       1 - : unit = ()
#         0 1 2 
- : unit = ()
#       *         []
- : unit = ()
#           [0]
- : unit = ()
#           
[2 ; 1 ; 0]
[1 ; 2 ; 0]
[2 ; 0 ; 1]
[0 ; 2 ; 1]
[1 ; 0 ; 2]
[0 ; 1 ; 2]
- : unit = ()
#   * * * * * * * * * *   

*)

(* 1. choose 函数：遍历列表，对每个元素执行一次回调函数 etc
   相当于标准库中的 List.iter
   参数 etc: 接收一个元素并执行某种操作的函数
   参数 things: 要遍历的列表
*)
let rec choose etc things = 
  match things with
  | [] -> ()                  (* 基础情况：列表为空，什么也不做，返回单元类型 () *)
  | head::tail -> 
      etc head;               (* 核心：把当前的头元素 head 喂给回调函数 etc 执行 *)
      choose etc tail;;       (* 递归：继续对剩下的尾部列表 tail 执行相同的操作 *)


(* 2. allbut 函数：从列表中剔除指定的元素（只剔除第一次遇到的那个）
   参数 things: 原始列表
   参数 thing: 需要被剔除的目标元素
*)
let rec allbut things thing = 
  match things with
  | [] -> []                  (* 基础情况：列表为空，返回空列表 *)
  | head::tail ->
      (* 注意这里用的是 == (物理相等)，在 OCaml 中通常比较内存地址；
         如果想比较数值或结构，通常建议用 = (结构相等)。 *)
      if head == thing then 
        tail                  (* 如果找到了目标元素，直接返回剩下的 tail，相当于把它剔除了 *)
      else 
        (* 如果当前元素不是目标，保留它，并递归去 tail 里继续找 *)
        head :: allbut tail thing;;


(* 3. permute 函数：生成全排列的核心入口
   参数 etc: 当生成一个完整的排列时，用来处理这个排列的回调函数
   参数 unpermutedThings: 尚未被排列的初始列表（比如 [1; 2; 3]）
*)
let permute etc unpermutedThings = 
  
  (* 定义内部辅助递归函数 permuting
     参数 etc: 接收完整排列的回调函数
     参数 permutedThings: 已经排好序的元素收集器（累加器）
     参数 unpermutedThings: 还没被排进去的剩余元素
  *)
  let rec permuting etc permutedThings unpermutedThings = 
    match unpermutedThings with
    (* 递归终止条件：如果没有剩余元素了，说明我们已经构建好了一个完整的排列！
       此时，把收集好的 permutedThings 喂给回调函数 etc 执行。*)
    | [] -> etc permutedThings
    
    (* 递归推进：如果还有剩余元素 *)
    | _ -> 
        (* 使用上面的 choose 函数，从 unpermutedThings 中挨个“挑选”一个元素 thing *)
        choose (fun thing -> 
          (* 这是一个匿名回调函数，对于选出的每一个 thing，执行以下逻辑：*)
          permuting 
            etc 
            (thing :: permutedThings)       (* 1. 把选出的 thing 加入到已排列的列表中 *)
            (allbut unpermutedThings thing) (* 2. 从未排列的列表中把这个 thing 剔除 *)
        ) 
        unpermutedThings (* choose 函数遍历的正是尚未排列的元素列表 *)
        
  in 
  (* 启动递归：初始状态下，已排列列表为空 []，未排列列表为全部输入 *)
  permuting etc [] unpermutedThings


  