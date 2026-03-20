let rec filter_cps pred lst on_result =
  match lst with
  | [] -> on_result []
  | head :: tail ->
      filter_cps pred tail (fun filtered_tail ->
        if head = pred then on_result(head :: filtered_tail)
        else on_result filtered_tail
        (* 思考：在这里判断 pred head，
           如果为 true，把 head 塞进 filtered_tail 然后喂给 on_result；
           如果为 false，直接把 filtered_tail 喂给 on_result *)
      )

type tree = 
  | Empty
  | Node of int * tree * tree ;;

let rec sum_tree_cps t on_result =
  match t with
  | Empty -> on_result 0
      
  | Node(v, left, right) ->
    sum_tree_cps left (
      fun left_res -> 
        sum_tree_cps right(
          fun right_res -> 
            on_result (left_res + right_res + v)
          )
      )
      (* 思考：
         1. 先对左子树调用 sum_tree_cps
         2. 在它的回调函数 (fun left_sum -> ...) 内部
         3. 对右子树调用 sum_tree_cps
         4. 在右子树的回调函数 (fun right_sum -> ...) 内部
         5. 把 v + left_sum + right_sum 拼在一起，喂给最外层的 on_result
      *)

let rec append_cps l1 l2 on_result =
  match l1 with
  | [] -> on_result l2
  | head :: tail -> 
    append_cps tail l2 (
      fun tail_list -> on_result(head :: tail_list)
    )

let map func objects = 
  let rec mapping objects map_list =
    match objects with
    | []->map_list
    | head::tail -> mapping tail ((func head)::(map_list)) in mapping objects []
 
let map func objects = 
  let rec mapping objects on_result = 
    match objects with
    | [] -> on_result []
    | head::tail -> mapping tail (fun map_tail -> on_result(func head::map_tail))
  in mapping objects 



let update_env_cps key f env k = 
  let rec updating env k =
    match env with
    | [] -> k []
    | (k_head, v_head)::tail-> 
      if k_head = key then updating tail (fun tail_env -> k( f(k_head, v_head) :: tail_env ))
      else updating tail (fun tail_env -> k ((k_head, v_head)::tail_env))
    in updating env k

let f1 n = 
  if n < 0 then []
  else
    let rec helper n list =
      match n with
      | 0 -> list
      | _ -> let list' = n::list in helper (n-1) list'
    in helper n [];;
  
let rec f2 etc tree = 
  match tree with
  | Empty -> ()
  | Node(k, left, right) ->
    f2 etc left;
    etc k;
    f2 etc right;;