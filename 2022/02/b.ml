let rec aoc score = 
  let input = read_line() in
  let parser = function
  | "A X" -> 3
  | "A Y" -> 4
  | "A Z" -> 8
  | "B X" -> 1
  | "B Y" -> 5
  | "B Z" -> 9
  | "C X" -> 2
  | "C Y" -> 6
  | "C Z" -> 7
  | _     -> 0 in
  if input |> parser != 0 then
    input |> parser |> Int.add score |> aoc
  else print_int score
;;

aoc 0