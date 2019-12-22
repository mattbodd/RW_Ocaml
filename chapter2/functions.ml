open Core

(* global variables *)
let numList = [0;5;10;15;20];;
let stutteredList = ["OCaml"; "Rust"; "Rust"; "C++"; "C"];;
let path = "/usr/bin:/usr/local/bin:/bin:/sbin";;
let aNum = 9;;
let aNum2 = 5;;

(* helper functions *)
(* standard method for printing list with List.iter *)
let printList aList =
  printf "{ ";
  List.iter ~f:(fun x -> printf "%d " x) aList;
  printf "}\n";
;;

let rec myPrintList aList =
  match aList with
  | [] -> ()
  | hd :: tl -> printf "%d " hd; myPrintList tl
;;

(* wrap printed list in curly brackets *)
let prettyPrintList aList =
  printf "{ ";
  myPrintList aList;
  printf "}\n"
;;
(* end helper functions *)

(* anonymous functions *)
(* defined using the `fun` keyword *)
printf "%d\n" ((fun x -> x + 1) 7);;

(* passing anonymous function to another function *)
List.map ~f:(fun x -> x + 1) numList;;
printList numList;;

(* storing anonymous functions in a list *)
let increments = [(fun x -> x + 1); (fun x -> x + 2)];;
(* in this example, iterate over all elements (functions)
   in incremenst and invoke them with an argument of 5 *)
List.map ~f:(fun g -> printf "%d " (g 5)) increments;;
printf "\n";;

(* as functions are treated as first order objects
   they can be bound in the same way a literal can *)
let mult_by_ten = (fun num -> num * 10);;
printf "10 * %d = %d\n" aNum (mult_by_ten aNum);;

(* declare a multiargument function 
   <name> <arg1> <arg2> ... *)
let abs_diff x y = abs(x - y);;
printf "abs(%d - %d) = %d\n" aNum2 aNum (abs_diff aNum2 aNum);;

(* recursive functions are explicitly marked with rec *)
let rec find_first_stutter aList =
  match aList with
  (* using a conjutive match arm *)
  | [] | [_] -> None
  | e1 :: e2 :: tl -> if e1 = e2 then Some e1
                      (* concatenate e2 and tl as we have only verified e1*)
                      else find_first_stutter (e2 :: tl)
;;

match find_first_stutter stutteredList with
| None -> printf "No stutters found!\n"
| Some stutter -> printf "Found a stutter: %s!\n" stutter
;;

(* infix operators can be used as prefix *)
printf "(+) %d %d = %d\n" aNum aNum2 ((+) aNum aNum2);;
(* it may be helpful to use an infix operator in a prefix way
   for the sake of partial evaluation *)
let numListPlusThree = List.map ~f:((+) 3) numList;;
printList numListPlusThree;;

(* UNIX pipe-like operator *)
String.split ~on:':' path
(* result of string split is passed to dedup_and_sort  *)
|> List.dedup_and_sort ~compare:String.compare
(* result of dedup_and_sort is passed to List.iter *)
(* notice that the function passed to List.iter is partially applied *)
|> List.iter ~f:(printf "%s\n")
;;

let add_two = function
  (* an alternative way to define a function *)
  (* function requires pattern matching to parse multiple arguments *)
  | Some x, Some y-> printf "%d + %d = %d\n" x y (x + y)
  | _ -> printf "Not enough values passed in\n"
;;
add_two ((Some 1),(Some 2));;

(* labeled arguments are preceded with a ~ *)
let ratio ~num ~denom = float num /. float denom;;
printf "%d /. %d = %f\n" 3 4 (ratio ~num:3 ~denom:4);;
(* label punning can be used with labeled arguments *)
let num = 5 in
    let denom = 7 in
    printf "%d /. %d = %f\n" num denom (ratio ~num ~denom)
;;

(* while labeled arguments allow for arbitrary ordering of labeled arguments 
   arbitrary order cannot be used when passing a function to a function
   with labeled arguments *)

(* optional arguments are prefixed with ? *)
let concat ?sep str1 str2 =
  let sep = match sep with
    | None -> ""
    | Some x -> x
  in
  str1 ^ sep ^ str2
;;
printf "%s\n" (concat "Boddewyn" "Matthew" ~sep:", ");;
(* example of presserving optional argument in wrapper *)
let upper_concat ?sep str1 str2 = concat ?sep (String.uppercase str1) str2;;
printf "%s\n" (upper_concat "Boddewyn" "Matthew" ~sep:", ");;

(* when partially applying optional arguments, they are erased as soon as the
   first non-labeled/optional argument defined after the optional argument
   is passed in *)
(* for this reason, OCaml warns when an optional argument is defined last 
   in a function signature as it can never be erased *)


(* DEBUG *)
printf "EOF!\n"
