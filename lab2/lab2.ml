let rec gcd i j = 
    if i <> 0 
    then if j > i 
            then gcd i (j - i) 
            else gcd (i - j) j 
    else j ;;

let rat n d = 
    let g = gcd n d in
    (n / g, d / g);;

let ratAdd (n1, d1) (n2, d2) = 
    rat (n1 * d2 + n2 * d1) (d1 * d2);;

let ratMul (n1, d1) (n2, d2) = 
    rat (n1 * n2) (d1 * d2);;

let ratDiv (n1, d1) (n2, d2) = 
    rat (n1 * d2) (d1 * n2);;

let ratGt (n1, d1) (n2, d2) =
    n1 * d2 > n2 * d1;;



let euler () =
    let ep = rat 1 100000 in
        let rec helper n fac = 
        let term = rat 1 fac in
        if ratGt ep term then rat 0 1
        else ratAdd term (helper (n+1) (fac*(n+1))) in
    helper 0 1;;
(*

terminal output 
ocaml tests2.ml
1 / 2
1 / 2
1 / 1
5 / 6
5 / 1
8 / 15
1 / 10
3 / 2
true
false
109601 / 40320

*)
(* 计算两个整数的最大公约数 (Greatest Common Divisor)
   这里使用的是基于减法的更相减损术（非取模版本的欧几里得算法）
*)
let rec gcd i j = 
    if i <> 0 
    then if j > i 
            then gcd i (j - i)  (* 如果 j > i，则用 j 减去 i 继续求 *)
            else gcd (i - j) j  (* 否则用 i 减去 j 继续求 *)
    else j ;;                   (* 当 i 为 0 时，j 就是最大公约数 *)

(* 构造并化简一个有理数（分数）
   输入为分子 n 和分母 d，返回化简后的元组 (分子, 分母)
*)
let rat n d = 
    let g = gcd n d in          (* 找出分子和分母的最大公约数 *)
    (n / g, d / g);;            (* 分子分母同除以最大公约数，实现分数化简 *)

(* 有理数加法： n1/d1 + n2/d2 = (n1*d2 + n2*d1) / (d1*d2) 
*)
let ratAdd (n1, d1) (n2, d2) = 
    rat (n1 * d2 + n2 * d1) (d1 * d2);;

(* 有理数乘法： (n1/d1) * (n2/d2) = (n1*n2) / (d1*d2)
*)
let ratMul (n1, d1) (n2, d2) = 
    rat (n1 * n2) (d1 * d2);;

(* 有理数除法： (n1/d1) / (n2/d2) = (n1*d2) / (d1*n2)
*)
let ratDiv (n1, d1) (n2, d2) = 
    rat (n1 * d2) (d1 * n2);;

(* 有理数大于比较： 判断 n1/d1 > n2/d2 是否成立
   通过交叉相乘避免了浮点数精度丢失： n1*d2 > n2*d1
*)
let ratGt (n1, d1) (n2, d2) =
    n1 * d2 > n2 * d1;;

(* 计算欧拉数 e 的近似值
*)
let euler () =
    (* 定义计算精度 epsilon (ep)，即 1/100000 (10的负5次方) *)
    let ep = rat 1 100000 in
        
        (* 递归辅助函数：
           n 表示当前的项数（0, 1, 2...）
           fac 表示当前项的阶乘 n!
        *)
        let rec helper n fac = 
        let term = rat 1 fac in (* 当前的计算项： 1 / n! *)
        
        (* 终止条件：如果精度 ep 大于当前项，说明后续的项已经微乎其微，停止计算，返回 0/1 (即 0) *)
        if ratGt ep term then rat 0 1
        
        (* 否则，将“当前项”加上“后续所有项的递归和” 
           下一次递归时，n 变为 n+1，阶乘 fac 变为 fac * (n+1) 
        *)
        else ratAdd term (helper (n+1) (fac*(n+1))) in
        
    (* 从 n=0, 0!=1 开始启动计算 (公式第一项为 1/0!) *)
    helper 0 1;;