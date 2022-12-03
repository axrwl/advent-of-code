echo "Day :"
read day
cookie=$(<cookie.txt)
copy(){
  case "$(uname -s)" in
        Linux*)  xclip -selection clipboard;;
        Darwin*) pbcopy;;
  esac
}
curl "https://adventofcode.com/2022/day/$day/input" \
-H "cookie: session=$cookie" | copy