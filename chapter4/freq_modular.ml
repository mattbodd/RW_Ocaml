open Base
open Stdio

let build_counts () =
  In_channel.fold_lines In_channel.stdin ~init:Counter.empty ~f:Counter.touch

(* store lines from In_channel.stdin *)
(* lines will be of type t *)
let lines : Counter.t = build_counts ()
    
(* print median *)
let median () =
  (* match the result of Counter.median *)
  match Counter.median lines with
  (* if result is of type Median *)
  | Counter.Median string -> printf "True median: %s\n" string
  (* if result is of type Before_and_after*)
  | Counter.Before_and_after (before, after) ->
    printf "Before and after median: %s, %s\n" before after

let top_ten () =
  (* convert to type (string * int) list  *)
  Counter.to_list lines
  (* sort list by value in (string * int) (key, value) in descending order *)
  |> List.sort ~compare:(fun (_, count1) (_, count2) ->
      Int.descending count1 count2)
  (* take top 10 elements of sorted list *)
  |> (fun l -> List.take l 10)
  (* iterate over top 10 items *)
  |> List.iter ~f:(fun (line, count) -> printf "%3d : %s\n" count line)


(* somewhat anlygous to main function *)
let () =
  median ();
  top_ten ()

       
