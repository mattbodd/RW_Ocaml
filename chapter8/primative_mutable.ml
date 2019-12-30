open Core

(* work with arrays *)
let aArray = [| 1; 2; 3 |]

(* array utility functions *)
let print_index arr index =
  printf "arr.(%d) = %d\n" index arr.(index)

let () =
  print_index aArray 1;
  aArray.(1) <- 0;
  print_index aArray 1

(* work with strings *)
(* strings are more memory efficient than char arrays in that an entry in a
 * character array on a 64-bit machine will take up 8 bytes whereas a character
 * in a string takes up a single byte *)

(* work with refs *)
(* creating a single mutable variable is achieved using the ref keyword and
 * effectively creates a single, mutable field record *)
let aMut = ref 10

let () =
  printf "aMut = %d\n" !aMut;
  (* increment aMut *)
  aMut := !aMut + 1;
  printf "aMut = %d\n" !aMut
