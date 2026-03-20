(*
  Define the basci type of Binary search tree
*)
type 'key bst = 
  |BstEmpty
  |BstNode of 'key * 'key bst * 'key bst;;

let rec find_max subtree =
  match subtree with 
    | BstEmpty -> raise(Failure "Empty tree has no left_max")
    | BstNode(v, _, BstEmpty) -> v
    | BstNode(v, _, right) ->  find_max right
;;

let rec bstDelete tree key = 
    match tree with
    | BstEmpty -> BstEmpty

    | BstNode(v, left, right) ->
      if key < v then
        BstNode(v, bstDelete left key, right)
      else if key > v then
        BstNode(v, left, bstDelete right key)
      else 
        match (left, right) with 
        
        | (BstEmpty, BstEmpty) -> BstEmpty

        | (BstEmpty, right_child) -> right_child

        | (left_child, BstEmpty) -> left_child

        | (left_child, right_child) -> 
          let left_max = find_max left_child in
          BstNode(left_max, bstDelete left_child left_max, right_child)
;;
(*
  The output from terminal
  # * * * * * * *     * * * * *   type 'key bst = BstEmpty | BstNode of 'key * 'key bst * 'key bst
val find_max : 'a bst -> 'a = <fun>
val bstDelete : 'a bst -> 'a -> 'a bst = <fun>
# *                         val bstInsert : 'a bst -> 'a -> 'a bst = <fun>
#                           val bstIsIn : 'a -> 'a bst -> bool = <fun>
#   *     val t : 'a bst = BstEmpty
# val t : int bst = BstNode (100, BstEmpty, BstEmpty)
# val t : int bst = BstNode (100, BstNode (70, BstEmpty, BstEmpty), BstEmpty)
# val t : int bst =
  BstNode (100, BstNode (70, BstEmpty, BstEmpty),
   BstNode (137, BstEmpty, BstEmpty))
# val t : int bst =
  BstNode (100, BstNode (70, BstNode (53, BstEmpty, BstEmpty), BstEmpty),
   BstNode (137, BstEmpty, BstEmpty))
# val t : int bst =
  BstNode (100,
   BstNode (70, BstNode (53, BstEmpty, BstEmpty),
    BstNode (86, BstEmpty, BstEmpty)),
   BstNode (137, BstEmpty, BstEmpty))
# val t : int bst =
  BstNode (100,
   BstNode (70, BstNode (53, BstEmpty, BstEmpty),
    BstNode (86, BstNode (74, BstEmpty, BstEmpty), BstEmpty)),
   BstNode (137, BstEmpty, BstEmpty))
# val t : int bst =
  BstNode (100,
   BstNode (70, BstNode (53, BstEmpty, BstEmpty),
    BstNode (86, BstNode (74, BstEmpty, BstEmpty), BstEmpty)),
   BstNode (137, BstEmpty, BstNode (212, BstEmpty, BstEmpty)))
# val t : int bst =
  BstNode (100,
   BstNode (70, BstNode (53, BstEmpty, BstEmpty),
    BstNode (86, BstNode (74, BstEmpty, BstEmpty), BstEmpty)),
   BstNode (137, BstEmpty,
    BstNode (212, BstNode (149, BstEmpty, BstEmpty), BstEmpty)))
# val t : int bst =
  BstNode (100,
   BstNode (70, BstNode (53, BstEmpty, BstEmpty),
    BstNode (86, BstNode (74, BstEmpty, BstEmpty), BstEmpty)),
   BstNode (137, BstEmpty,
    BstNode (212, BstNode (149, BstEmpty, BstEmpty),
     BstNode (997, BstEmpty, BstEmpty))))
#   * * * * * * * * * * * * * * *     val t : int bst =
  BstNode (100,
   BstNode (70, BstNode (53, BstEmpty, BstEmpty),
    BstNode (86, BstNode (74, BstEmpty, BstEmpty), BstEmpty)),
   BstNode (137, BstEmpty,
    BstNode (212, BstEmpty, BstNode (997, BstEmpty, BstEmpty))))
#   * * * * * * * * * * * *         val t : int bst =
  BstNode (100,
   BstNode (70, BstNode (53, BstEmpty, BstEmpty),
    BstNode (86, BstNode (74, BstEmpty, BstEmpty), BstEmpty)),
   BstNode (137, BstEmpty, BstNode (212, BstEmpty, BstEmpty)))
#   * * * * * * * * * *         val t : int bst =
  BstNode (100,
   BstNode (70, BstNode (53, BstEmpty, BstEmpty),
    BstNode (74, BstEmpty, BstEmpty)),
   BstNode (137, BstEmpty, BstNode (212, BstEmpty, BstEmpty)))
#   * * * * * * * *         val t : int bst =
  BstNode (74, BstNode (70, BstNode (53, BstEmpty, BstEmpty), BstEmpty),
   BstNode (137, BstEmpty, BstNode (212, BstEmpty, BstEmpty)))
#   * * * * * * * * *         val t : int bst =
  BstNode (74, BstNode (53, BstEmpty, BstEmpty),
   BstNode (137, BstEmpty, BstNode (212, BstEmpty, BstEmpty)))
#   * * * * * *         val t : int bst =
  BstNode (74, BstNode (53, BstEmpty, BstEmpty),
   BstNode (212, BstEmpty, BstEmpty))
#   * * * *         val t : int bst = BstNode (74, BstEmpty, BstNode (212, BstEmpty, BstEmpty))
# val t : int bst = BstNode (74, BstEmpty, BstEmpty)
# val t : int bst = BstEmpty
*)

(* 定义一个多态的二叉搜索树类型
   'key 表示树中存储的元素类型。
   树要么是空的 (BstEmpty)，要么是一个节点 (BstNode)，包含：当前值、左子树、右子树。
*)
type 'key bst = 
  | BstEmpty
  | BstNode of 'key * 'key bst * 'key bst;;

(* 辅助函数：查找并返回当前树中的最大值
   由于是二叉搜索树，最大值一定在最右侧的叶子节点上。
*)
let rec find_max subtree =
  match subtree with 
    (* 错误处理：如果是空树，则抛出异常 *)
    | BstEmpty -> raise(Failure "Empty tree has no left_max")
    (* 如果当前节点没有右子树了，那它自身的值就是最大值 *)
    | BstNode(v, _, BstEmpty) -> v
    (* 否则，继续递归查找右子树 *)
    | BstNode(v, _, right) -> find_max right
;;

(* 查找函数：判断某个 key 是否存在于二叉搜索树中
   返回类型为 bool (true 表示存在，false 表示不存在)
*)
let rec bstSearch tree key = 
    match tree with
    (* 如果遍历到空树，说明没找到，返回 false *)
    | BstEmpty -> false
    
    | BstNode(v, left, right) ->
        if key = v then 
            true                   (* 找到了，返回 true *)
        else if key < v then 
            bstSearch left key     (* key 更小，去左子树找 *)
        else 
            bstSearch right key    (* key 更大，去右子树找 *)
;;

(* 插入函数：向二叉搜索树中插入一个新的 key
   注意：通常 BST 不允许重复元素，如果元素已存在，则原样返回。
*)
let rec bstInsert tree key =
    match tree with
    (* 如果遇到空位置，就用新元素在这里创建一个新的叶子节点 *)
    | BstEmpty -> BstNode(key, BstEmpty, BstEmpty)
    
    | BstNode(v, left, right) ->
        if key < v then
            (* 插入值小于当前值，往左子树插，并重建节点连接新左子树 *)
            BstNode(v, bstInsert left key, right)
        else if key > v then
            (* 插入值大于当前值，往右子树插，并重建节点连接新右子树 *)
            BstNode(v, left, bstInsert right key)
        else
            (* 插入值等于当前值，说明已经存在。直接返回原树不作修改 *)
            tree
;;

(* 核心函数：从二叉搜索树中删除指定的 key，并返回一棵新树 *)
let rec bstDelete tree key = 
    match tree with
    (* 基本情况：如果树为空，直接返回空树（说明没找到要删除的元素） *)
    | BstEmpty -> BstEmpty

    (* 匹配到节点时，对比 key 和当前节点值 v 的大小 *)
    | BstNode(v, left, right) ->
      if key < v then
        (* 要删除的 key 小于当前值，去左子树递归删除，并重建当前节点 *)
        BstNode(v, bstDelete left key, right)
      else if key > v then
        (* 要删除的 key 大于当前值，去右子树递归删除，并重建当前节点 *)
        BstNode(v, left, bstDelete right key)
      else 
        (* 找到了要删除的节点 (key = v)！接下来处理四种节点子节点的情况： *)
        match (left, right) with 
        
        (* 情况 A：是叶子节点（左右皆空），直接删除，返回空 *)
        | (BstEmpty, BstEmpty) -> BstEmpty

        (* 情况 B：只有右子树，用右子树直接顶替当前节点的位置 *)
        | (BstEmpty, right_child) -> right_child

        (* 情况 C：只有左子树，用左子树直接顶替当前节点的位置 *)
        | (left_child, BstEmpty) -> left_child

        (* 情况 D：左右子树都有（最复杂的情况） *)
        | (left_child, right_child) -> 
          (* 1. 找到左子树中的最大值 (也就是前驱节点) *)
          let left_max = find_max left_child in
          (* 2. 用这个最大值顶替当前要被删除的节点的值 (v -> left_max)
             3. 并在左子树中把那个最大值对应的节点删掉 *)
          BstNode(left_max, bstDelete left_child left_max, right_child)
;;

(* 中序遍历：将二叉树转换成一个列表 (List) *)
let rec in_order tree =
  match tree with
  | BstEmpty -> []  (* 如果是空树，返回空列表 *)
  
  | BstNode(v, left, right) ->
      (* 1. 先递归遍历左子树，得到左边所有元素的列表 *)
      (* 2. 将当前节点的值 v 包装成单元素列表 [v] *)
      (* 3. 再递归遍历右子树，得到右边所有元素的列表 *)
      (* 4. 用 @ 符号将这三个部分拼接起来 *)
      (in_order left) @ [v] @ (in_order right)
;;

let rec pre_order tree =
  match tree with
  | BstEmpty -> []
  
  | BstNode(v, left, right) ->
      (* 先把当前节点放在最前面，然后拼接左子树，最后拼接右子树 *)
      [v] @ (pre_order left) @ (pre_order right)
;;

let rec post_order tree =
  match tree with
  | BstEmpty -> []
  
  | BstNode(v, left, right) ->
      (* 先处理左子树，再处理右子树，最后才把当前节点追加到末尾 *)
      (post_order left) @ (post_order right) @ [v]
;;