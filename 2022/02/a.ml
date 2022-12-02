let rec aoc score = 
  let input = read_line() in
  let parser = function
  | "A X" -> 4
  | "A Y" -> 8
  | "A Z" -> 3
  | "B X" -> 1
  | "B Y" -> 5
  | "B Z" -> 9
  | "C X" -> 7
  | "C Y" -> 2
  | "C Z" -> 6
  | _     -> 0 in
  if input |> parser != 0 then
    input |> parser |> Int.add score |> aoc
  else print_int score
;;

aoc 0