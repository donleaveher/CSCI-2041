type 'base stackOperation = 
  | IsEmpty
  | Peek
  | Pop
  | Push of 'base ;;

type 'base stackResult = 
  | NoResult
  | BaseResult of 'base
  | BoolResult of bool ;;

exception StackError of string;;

let makeStack() = 
  let top = ref [] in (* 私有变量：用 list 存储栈内容 *)

  let isEmpty () =
    BoolResult (!top = [])
  in

  let peek () = 
    match !top with
    | [] -> raise(StackError "Cannot Peek at an empty stack")
    | element :: _ -> BaseResult element
  in

  let pop () =
    match !top with 
    | [] -> raise (StackError "Cannot Pop an empty stack")
    | _ :: elements -> 
      top := elements; (* 去掉栈顶，注意 pop 不返回被弹出的值 *)
      NoResult
  in

  let push element =
    top := element :: !top ; (* 新元素放到列表头部 *)
    NoResult
  in
  
  let dispatch operation = (* dispatch 通过闭包记住上面所有函数 *)
    match operation with
    | IsEmpty -> isEmpty()
    | Peek -> peek()
    | Pop -> pop()
    | Push element -> push element
  in 
  dispatch ;; (* 返回 dispatch 函数本身，不是调用它！ *)
