(*
  Module:
  modules look like classes but are NOT classes
  You can’t make instances of modules
  Modules are not 1st class

  module 和 class 看起来有点像，因为它们都能：

    把一组相关东西放在一起
    用点号访问：Stack.make
    不同之处

  但 module 不是 class：

  1）不能实例化

    你不能像 OOP 那样写很多个 module 对象：

    不能“new Stack”
    不能创建多个模块实例
    2）module 主要是命名组织工具

    它不是拿来表示“对象”的，而是拿来组织定义的。

  3）不是 first-class

    所谓不是 first-class，大致就是：

    不能像普通值那样随便传来传去
    不能直接像 int、list、function 那样自然参与一切表达式操作

    在这门课的层面，你先记成：

    module 更像“编译期/语言层面的组织工具”，不是运行时对象。
*)

module Stack =
struct
  (* 栈的元素类型是 'a，整个栈本质上是一个可变的 list *)
  type 'a t = 'a list ref

  (* 自定义异常 *)
  exception StackError of string

  (* 判断栈是否为空 *)
  let isEmpty stack =
    !stack = []

  (* 创建一个空栈 *)
  let make () =
    ref []
  (*  
    stack 是一个引用, !stack 表示把里面的 list 取出来
  *)

  (* 抛出栈相关错误 *)
  let oops message =
    raise (StackError message)

  (* 查看栈顶元素，但不移除 *)
  let peek stack =
    match !stack with
    | [] -> oops "can't peek at an empty stack"
    | element :: _ -> element

  (* 弹出栈顶元素 *)
  let pop stack =
    match !stack with
    | [] -> oops "can't pop from an empty stack"
    | _ :: elements ->
        stack := elements

  (* 压入一个新元素到栈顶 *)
  let push stack element =
    stack := element :: !stack
end

let s = Stack.make ();;

Stack.isEmpty s;;
(* true *)

Stack.push s 10;;
(* () *)

Stack.peek s;;
(* 10 *)

Stack.push s 20;;
(* () *)

Stack.peek s;;
(* 20 *)

Stack.pop s;;
(* () *)

Stack.peek s;;
(* 10 *)

(*
module Stack :
sig
  type 'a t = 'a list ref
  exception StackError of string
  val isEmpty : 'a list ref -> bool
  val make : unit -> 'a list ref
  val oops : string -> 'b
  val peek : 'a list ref -> 'a
  val pop : 'a list ref -> unit
  val push : 'a list ref -> 'a -> unit
end
*)