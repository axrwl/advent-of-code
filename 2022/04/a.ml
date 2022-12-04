open String
open List

let check a b c d = 
  if b > d then c >= a
  else if d > b then a >= c
  else d = b

let parser s = s |> split_on_char ',' 
                 |> map @@ split_on_char '-'
                 |> flatten
                 |> map int_of_string

let rec aoc s = 
  let input = read_line() in
  if input <> "" then let lst = parser input in
    if check (hd lst) (nth lst 1) (nth lst 2) (nth lst 3)
      then aoc @@ s+1 else aoc s
  else print_int s
;; aoc 0