open Base

(* this module is to be used by freq_modular.ml *)

(* define type t to be list of tuple *)
type t = (string * int) list

(* define the median t value *)
type median = | Median of string
              | Before_and_after of string * string

(* define empty to be an empty list *)
let empty = []

(* to_list, as defined in interface, converts a type t -> (string * int) list *)
let to_list x = x

let touch counts line =
  let count =
    match List.Assoc.find ~equal:String.equal counts line with
    | None -> 0
    | Some x -> x
  in
  List.Assoc.add ~equal:String.equal counts line (count + 1)

let median t =
  (* convert input, t, to type (string * int) list *)
  let aList = to_list t in
  (* sort in descending order based on value in (string * int) (key, value) *)
  (* TODO: consolidate duplicate logic for sorting *)
  let sorted_strings =
    List.sort aList ~compare:(fun (_, count1) (_, count2) ->
        Int.descending count1 count2)
  in
  let len = List.length sorted_strings in
  if len = 0 then failwith "median: empty frequency count";
  (* function to grab the nth element in sorted_strings list *)
  let nth n = fst (List.nth_exn sorted_strings n) in
  if len % 2 = 1
  then Median (nth (len / 2))
  else Before_and_after (nth (len/2 - 1), nth (len/2))
