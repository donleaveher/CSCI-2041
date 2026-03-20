let binomial n k = 
  if k > n then 0.0
  else
    let rec helper q n k = 
      if k = 0 then q
      else
        let q = q *. (float_of_int n) /. (float_of_int k) in
        helper q (n-1) (k-1)
      in helper 1.0 n k