open Core;;

(* global variables *)
let numList = [1;2;3;4;5;6;7;8;9;10];;
let stutteredList = ["OCaml";"Rust";"Rust";"C";"C++";"C++"];;

(* helper functions *)
(* standard method for printing list with List.iter *)
let printIntList ?(sep=" ") aList =
  printf "{";
  List.iter ~f:(fun x -> printf "%s%d" sep x) aList;
  printf "%s}\n" sep;
;;

let printStrList ?(sep=" ") aList =
  printf "{";
  List.iter ~f:(fun x -> printf "%s%s" sep x) aList;
  printf "%s}\n" sep;
;;
(* end helper functions *)

(* match statements are typically compiled with insightful conditional
   jumps to avoid checking each arm of a match *)
let match_plus_one num =
  match num with
  | 0 -> 1
  | 1 -> 2
  | 2 -> 3
  | _ -> num + 1
;;

(* this implementation will require checking each conditional branch 9*)
let plus_one_if num =
  if      num = 1 then 2
  else if num = 2 then 3
  else if num = 3 then 4
  else    num + 1
;;

(* functionality to create a pretty printed table *)
(* first calculate the maximum column width *)
let max_widths header rows =
  (* store lengths into list *)
  let lengths aList = List.map ~f:String.length aList in
  (* iterate over each row *)
  List.fold rows
    (* sort each header by length *)
    ~init:(lengths header)
    ~f:(fun acc row ->
      List.map2_exn ~f:Int.max acc (lengths row))
;;

let render_seperator widths =
  (* generate appropriate amount of dashes for a given column width
     ie: width + 2 *)
  let pieces = List.map widths
                 ~f:(fun width -> String.make (width + 2) '-')
  in
  (* each line starts with a |, followed by each group of dashes seperated
     by a +, finally followed by a | *)
  "|"  ^ String.concat ~sep:"+" pieces ^ "|"
;;

(* function to create padding *)
let pad str length =
  " " ^ str ^ String.make (length - String.length str + 1) ' '
;;

let render_row row widths =
  (* pad each cell and encase with |'s *)
  let padded = List.map2_exn row widths ~f:pad in
  "|" ^ String.concat ~sep:"|" padded ^ "|"
;;

let render_table header rows =
  let widths = max_widths header rows in
  String.concat ~sep:"\n"
    (render_row header widths
     :: render_seperator widths
     :: List.map rows ~f:(fun row -> render_row row widths)
    )
;;

printf "%s\n"
  (render_table
     ["language";"architect";"first release"]
     [ ["Lisp";"John McCarthy";"1958"];
       ["C";"Dennis Ritchie";"1969"];
       ["ML";"Robin Milner";"1973"];
       ["OCaml";"Xavier Leroy";"1996"];
  ])
;;

(* more useful list functions *)
let evens = List.filter ~f:(fun num -> num mod 2 = 0) numList;;
printIntList evens;;

(* List.filter_map will drop all elements that return None after mapping *)
List.filter_map (Sys.ls_dir ".") ~f:(fun fileName ->
    match String.rsplit2 ~on:'.' fileName with
    | None | Some("",_) -> None
    | Some (_,ext) -> Some ext)
|> List.dedup_and_sort ~compare:String.compare
|> printStrList
;;

(* List.partition_tf will seperate values into two columns
   T for which the condition is True and F for False *)
let (evens, odds) = List.partition_tf numList ~f:(fun num -> num mod 2 = 0);;
printIntList evens;;
printIntList odds;;

(* concatenation of lists can be done in a number of ways *)
(* using List.append *)
let listAppend_evensAndOdds = List.append evens odds;;
printIntList listAppend_evensAndOdds;;
(* Using @ operator*)
let at_evensAndOdds = evens @ odds;;
printIntList at_evensAndOdds;;
(* lists of lists can be combined as well *)
let flatList = List.concat [listAppend_evensAndOdds;at_evensAndOdds];;
printIntList flatList;;

(* recursively print directory tree using List.concat *)
let rec ls_rec s =
  if Sys.is_file_exn ~follow_symlinks:true s
  then [s]
  else
    Sys.ls_dir s
    |> List.map ~f:(fun sub -> ls_rec (s ^/ sub))
    |> List.concat
;;
let dirTree = ls_rec ".";;
printStrList dirTree ~sep:"\n";;

(* the as keyword in pattern matching can be used to creating
   duplicate values *)
let rec destutter = function
  | [] | [_] as l -> l
  (* here, tl represents the concactenated value hd' :: _ *)
  (* the when clause can be used to condition a match arm 
   * when clauses prevent the compiler from determining 
   * if a match is exhaustive or not *)
  | hd :: (hd' :: _ as tl) when hd = hd' -> destutter tl
  | hd :: tl -> hd :: destutter tl
;;
let destutteredList = destutter stutteredList;;
printStrList destutteredList;;
