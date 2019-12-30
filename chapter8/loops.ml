open Core

(* OCaml forloops are inclusive of lower and upperbound *)
let () =
  printf "1..10\n[";
  for i = 1 to 10 do printf " %d" i done;
  printf " ]\n10..1\n[";
  for i = 10 downto 1 do printf " %d" i done;
  printf " ]\n"

let arr = [|10;9;8;7;6;5;4;3;2;1|]

let rev_inplace arr =
  let i = ref 0 in
  let j = ref (Array.length arr - 1) in
  while !i < !j do
    let tmp = arr.(!i) in
    arr.(!i) <- arr.(!j);
    arr.(!j) <- tmp;
    Int.incr i;
    Int.decr j
  done

(* array utility functions *)
let print_arr arr =
  printf "[";
  for i = 0 to (Array.length arr)-1 do printf " %d" arr.(i) done;
  printf " ]\n"
        
let () =
  rev_inplace arr;
  print_arr arr
