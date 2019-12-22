(*
 Matthew Boddewyn
 12-22-19
 Real World OCaml
 *)

(* utilize Core library (Jane Street) *)
open Core

(* define a recursive function - read_and_accmulate *)
let rec read_and_accumulate acc =
  (* grab line from standard input *)
  let line = In_channel.input_line In_channel.stdin in
  (* pattern match EOF -> return acc OR accumulate and recursively call *)
  match line with
  | None -> acc
  | Some x -> read_and_accumulate (acc +. Float.of_string x)

(* unconditionally print formatted string *)
let () =
  printf "Total: %F\n" (read_and_accumulate 0.)
