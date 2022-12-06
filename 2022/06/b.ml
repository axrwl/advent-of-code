open String
module C = Set.Make(Char)

let to_list s = List.init (length s) @@ get s

let input = open_in "aoc.in" |> input_line |> to_list

let rec aoc n = 
  if input |> List.filteri (fun i _ -> i < n+1 && i > n-14)
           |> C.of_list |> C.elements 
           |> List.length = 14 then print_int @@ n+1
  else aoc @@ n+1
;; aoc 13