(* Raised when the solver encounters an error. *)
exception SolvingError of string

type expression =
  Var of string |                    (* a, b, c ... *)
  Neg of expression |                (*   ¬ R *)
  Add of expression * expression |   (* L + R *)
  Div of expression * expression |   (* L / R *)
  Equ of expression * expression |   (* L = R *)
  Mul of expression * expression |   (* L × R *)
  Sub of expression * expression ;;  (* L − R *)

(* Return true if Var name appears in expr. *)
let rec isInside name expr =
  match expr with
  | Var s -> s = name
  | Neg r -> isInside name r
  | Add (l, r) | Sub (l, r) | Div (l, r) | Equ (l, r) | Mul (l, r) ->
    isInside name l || isInside name r

(* Apply algebraic rules to isolate Var name on the left side. *)
let rec solver name left right =
  match left with
  | Var v when v = name -> Equ (left, right)  (* solved *)
  | Neg b -> solver name b (Neg right)
  | Add (a, b) ->
    if isInside name a then solver name a (Sub (right, b))
    else solver name b (Sub (right, a))
  | Sub (a, b) ->
    if isInside name a then solver name a (Add (right, b))
    else solver name b (Sub (a, right))
  | Div (a, b) -> 
    if isInside name a then solver name a (Mul (right, b))
    else solver name b (Div (a, right))
  | Mul (a, b) ->
    if isInside name a then solver name a (Div (right, b))
    else solver name b (Div (right, a))
  | _ -> raise (SolvingError "cannot solve")

(* Solve equation for Var name. Ensure name is on the left, then call solver. *)
let solve name equation =
  match equation with
  | Equ (left, right) -> 
    if isInside name right && isInside name left then raise (SolvingError "variable appears both sides")
    else if isInside name left then solver name left right
    else if isInside name right then solver name right left
    else raise (SolvingError "variable not found")
  | _ -> raise (SolvingError "not an equation")