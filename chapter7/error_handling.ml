open Core
open Stdio

(* options can be used to represent success or errors often in programs
 * however they are not always sufficient in conveying pertinent information *)
(* Result.t is meant to address this deficiency and acts as an augmented option
 * with the ability to store additional information in the event of an error *)
let some_results = [Ok 3; Error "abject failure"; Ok 4]
                   
(* Error.t provides the utility of lazily generating error values as it is 
 * unnecessary to craft an error message if an error never occurs *)
(* errors can contain primitive values such as strings *)
let str_error = Error.of_string "an error occurred"
    
(* errors can also take more complex handling through thunks which expect a 
 * single unit type *)
let thunk_error = Error.of_thunk (fun () ->
    let (pi : float) = Float.acos(-1.0) in
    Printf.sprintf "an error occur-ed: %f" pi)

(* errors are often serialized using s-exprs *)
let custom_to_sexp = [%sexp_of: float * string list * int]
let my_sexp = custom_to_sexp (3.5, ["a";"b";"c"], 6034)

(* it can be helpful to provide additional context with reported errors *)
(* Error.tag allows for tagging of errors and Error.of_list allows for grouping
 * together of errors *)
(*
 
 Error.tag
    (Error.of_list [ Error.of_string "Your tires were slashed]";
                     Error.of_string "Your windshield was smashed" ])
    "Over the weekend"

*)

(* common functionality for working with error-prone values is natively 
 * provided *)
(* in this case, Option.bind will only apply the function ~f:... 
 * when (List.hd/tail) is not None *)
let compute_bounds ~compare list =
  let sorted = List.sort ~compare list in
  Option.bind (List.hd sorted) ~f:(fun first ->
      Option.bind (List.last sorted) ~f:(fun last ->
          Some (first, last)))

(* Option.bind can be expressed using infix notation as well *)
let infix_compute_bounds ~compare list =
  let open Option.Monad_infix in
  let sorted = List.sort ~compare list in
  List.hd sorted   >>= fun first ->
  List.last sorted >>= fun last ->
  Some (first, last)

(* Option.let_syntax can be used to make binding a little more familiar *)
(* let%bind can be rewritten from

   let%bind x = some_expr in some_other_expr
   * to
   some_expr >>= fun x -> some_other_expr

 *)
let let_compute_bounds ~compare list =
  let open Option.Let_syntax in
  let sorted = List.sort ~compare list in
  let%bind first = List.hd sorted in
  let%bind last  = List.last sorted in
  Some (first, last)

(* an additional Option idiom is Option.both which constructs an optional pair
 * iff both inputs are not None *)
let both_compute_bounds ~compare list =
  let sorted = List.sort ~compare list in
  Option.both (List.hd sorted) (List.last sorted)

(* exceptions declare runtime inconsistencies and can be crafted *)
exception Key_not_found of string
let exceptions = [Division_by_zero; Key_not_found "b"]
(* exceptions are all of type exn and functions which routinely *)
let rec find_exn alist key = match alist with
  | [] -> raise (Key_not_found key)
  | (key', data) :: tl -> if String.(=) key key' then data else find_exn tl key

let alist = [("a",1); ("b",2)]

(* handling exceptions is done using try/with statements *)
let risky_divide (a : int) =
  try printf "a/0 = %d" (a/0) with
  | _ -> printf "cannot divide by zero!\n"

(* to avoid unreaped resources, try/finally style statements can be expressed
 * using Exn.protect *)
let parse_line line =
  String.split_on_chars ~on:[','] line
  |> List.map ~f:Float.of_string

let load filename =
  let inc = In_channel.create filename in
  Exn.protect
    ~f:(fun () -> In_channel.input_lines inc |> List.map ~f:parse_line)
    ~finally:(fun () -> In_channel.close inc)

let print_weight weight =
  printf "Weight: %d\n" weight

let lookup_weight ~print_weight alist key =
  try
    let data = find_exn alist key in
    print_weight data
  with
    Key_not_found _ -> printf "Weight not found\n"

(* knowing the name of exceptions in advance can be difficult which is why
 * pattern matching on more general exceptions can be helpful *)
let general_lookup_weight ~print_weight alist key =
  match find_exn alist key with
  | exception _ -> 0
  | data -> print_weight data

(* exceptions are mostly useful when they provide backtraces to identify the 
 * root of problems *)
exception Empty_list

let list_max = function
  | [] -> raise Empty_list
  | hd :: tl -> List.fold tl ~init:hd ~f:(Int.max)

let () =
  printf "Found %d\n" (find_exn alist "a");
  risky_divide 2;
  lookup_weight ~print_weight alist "me";
  printf "%d\n" (list_max [1;2;3]);
  printf "%d\n" (list_max []);
