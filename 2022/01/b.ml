type prev = Int of int | Empty

let a = Array.make 1000 0

let cmp a b = 
  if a > b then -1 
  else if b > a then 1
  else 0

let rec aoc p i = 
  let input = read_int_opt () in
    match input, p with 
    | Some r, Empty -> Array.set a i r; aoc (Int r) i
    | Some r, Int x -> Array.set a i (a.(i) + r); aoc (Int r) i
    | None, Empty -> a |> Array.fast_sort cmp; print_int @@ a.(0) + a.(1) + a.(2)
    | None, Int x -> aoc Empty @@ i+1
;;
aoc Empty 0 