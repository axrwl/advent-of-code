open String

let score c = 
  match Char.code c with
  | c when c > 96 -> c - 96
  | c -> c - 38

let to_list s = List.init (length s) @@ get s

let rec aoc s = 
  let l1 = read_line() |> to_list in
  if List.length l1 != 0 then begin
    let l2 = read_line() |> to_list in
    let l3 = read_line() |> to_list in
    let items = Array.make 52 0 in
    let f c = Array.set items (score c - 1) 1 in List.iter f l1; 
    let f c = if items.(score c - 1) = 1 
      then Array.set items (score c - 1) 2 in List.iter f l2;
    let rec f c i = 
      if items.(score c - 1) = 2 then score c
      else f (List.nth l3 @@ i + 1) @@ i + 1 in
    aoc @@ s + f (List.hd l3) 0;
  end else print_int s
;; aoc 0