open Stack
let stacks = Array.init 9 (fun x -> create());;

let push_string s i = 
  let len = String.length s in
  let lst = List.init len @@ String.get s in
  let rec loop = function
    | 0 -> ()
    | n -> push (List.nth lst @@ n-1) stacks.(i); loop @@ n-1 in
  loop len
;;

push_string "RWFHTS"   0;
push_string "WGDGS"    1;
push_string "WTB"      2;
push_string "JZQNTWRD" 3;
push_string "ZTVLGHBF" 4;
push_string "GSBVCTPL" 5;
push_string "PGWTRBZ"  6;
push_string "RJCTMGN"  7;
push_string "WBGL"     8;;

let move f t n = 
  let s = create() in
  let rec loop f t = function 
    | 0 -> ()
    | n -> push (pop f) t; loop f t @@ n-1 in
  loop f s n; loop s t n

let rec aoc () = 
  let input = read_line() in
  if input <> "" then
    let m, f, t = Scanf.sscanf input "move %d from %d to %d" (fun x y z -> x, y-1, z-1) in
    move stacks.(f) stacks.(t) m; aoc()
  else Array.iter (fun x -> print_char @@ pop x) stacks
;; aoc()