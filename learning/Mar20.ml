(* 定义一个模块类型：描述“栈模块”应该提供什么 *)
module type STACKY =
sig
  type 'a t
  exception StackError of string

  val isEmpty : 'a t -> bool
  val make : unit -> 'a t
  val peek : 'a t -> 'a
  val pop : 'a t -> unit
  val push : 'a t -> 'a -> unit
end

(* 用这个接口来实现一个真正的模块 *)
module Stack : STACKY =
struct
  type 'a t = 'a list ref
  exception StackError of string

  let make () = ref []

  let isEmpty s =
    !s = []

  let peek s =
    match !s with
    | [] -> raise (StackError "peek from empty stack")
    | x :: _ -> x

  let pop s =
    match !s with
    | [] -> raise (StackError "pop from empty stack")
    | _ :: xs -> s := xs

  let push s x =
    s := x :: !s

  (* 这个函数没有写进 module type 里，所以外部看不到 *)
  let debug_length s =
    List.length !s
end

(* 测试 *)
let () =
  let s = Stack.make () in
  Stack.push s 10;
  Stack.push s 20;
  Printf.printf "Top = %d\n" (Stack.peek s);
  Stack.pop s;
  Printf.printf "Top after pop = %d\n" (Stack.peek s)


module type PERSON_SIG =
sig
  type t
  val make : string -> int -> t
  val get_name : t -> string
end

module Person : PERSON_SIG =
struct
  type t = { name : string; age : int }

  let make name age = { name; age }

  let get_name p = p.name

  let get_age p = p.age   (* 没写进接口，外面不能访问 *)
end

let () =
  let p = Person.make "Alice" 20 in
  Printf.printf "Name = %s\n" (Person.get_name p)



(*
  local module 复习代码
*)
let describe_number x =
  let module M =
  struct
    type kind = Positive | Zero | Negative
    exception BadNumber

    let classify n =
      if n > 0 then Positive
      else if n = 0 then Zero
      else Negative
  end
  in
  match M.classify x with
  | M.Positive -> "positive"
  | M.Zero -> "zero"
  | M.Negative -> "negative"

let () =
  Printf.printf "%s\n" (describe_number 7);
  Printf.printf "%s\n" (describe_number 0);
  Printf.printf "%s\n" (describe_number (-3))



let number_to_string x =
  let module M =
  struct
    type sign = Pos | Neg | Zero
  end
  in
  let s =
    if x > 0 then M.Pos
    else if x < 0 then M.Neg
    else M.Zero
  in
  match s with
  | M.Pos -> "positive"
  | M.Neg -> "negative"
  | M.Zero -> "zero"
(*这里 sign 这个类型是局部类型，只在函数内部用。*)



(*两个不同模块实现同一个接口*)
module type COUNTER =
sig
  type t
  val make : unit -> t
  val inc : t -> unit
  val get : t -> int
end

module CounterA : COUNTER =
struct
  type t = int ref
  let make () = ref 0
  let inc c = c := !c + 1
  let get c = !c
end

module CounterB : COUNTER =
struct
  type t = { mutable value : int }
  let make () = { value = 0 }
  let inc c = c.value <- c.value + 1
  let get c = c.value
end

let () =
  let c1 = CounterA.make () in
  CounterA.inc c1;
  CounterA.inc c1;
  Printf.printf "CounterA = %d\n" (CounterA.get c1);

  let c2 = CounterB.make () in
  CounterB.inc c2;
  Printf.printf "CounterB = %d\n" (CounterB.get c2)