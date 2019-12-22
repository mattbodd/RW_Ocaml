(*
 Matthew Boddewyn
 12-22-19
 Real World OCaml
 *)

(* utilize Core library (Jane Street) *)
open Core

(* define a recursive function - read_and_accmulate *)
let rec read_and_accumulate acc =
  let line = In_channel.input_line In_channel.stdin in
  match line with
  | None -> acc
  | Some x -> read_and_accumulate (acc +. Float.of_string x)

let () =
  printf "Total: %F\n" (read_and_accumulate 0.)
