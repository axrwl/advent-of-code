type prev = Int of int | Empty

let a = ref 0
let b = ref 0

let rec ab () = 
  if !a > !b then b := !a else ()

let rec aoc p = 
  let input = read_int_opt () in
    match input, p with 
    | Some r, Empty -> a := r; aoc (Int r)
    | Some r, Int x -> a := !a + r; aoc (Int r)
    | None, Empty -> print_int !b
    | None, Int x -> ab(); aoc Empty
;;
aoc Empty