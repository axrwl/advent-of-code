open String

let score c = 
  match Char.code c with
  | c when c > 96 -> c - 96
  | c -> c - 38

let to_list s = List.init (length s) @@ get s

let rec aoc s = 
  let input = read_line() in
  let len = length input in
  if len != 0 then begin
    let items = Array.make 52 0 in
    let x = len/2 in
    let a = sub input 0 x |> to_list in
    let b = sub input x x |> to_list in
    let f c = Array.set items (score c - 1) 1 in
    List.iter f a;
    let rec f c i = 
      if items.(score c - 1) = 1 then score c
      else f (List.nth b @@ i + 1) @@ i + 1 in
    aoc @@ s + f (List.hd b) 0;
  end else print_int s
;; aoc 0