open Core
open Base

(* helper functions *)
let print_int_list l =
  printf "{ ";
  List.iter ~f:(fun i -> printf "%d " i) l;
  printf "}\n"

let print_float_list l =
  printf "{";
  List.iter ~f:(fun f -> printf "%0.2f " f) l;
  printf "}\n"
(* end helper functions *)

(* ocaml is conceptually broken into two sublanguages:
   * one concerned with types and value_tuple
   * another concerned with modules and their signatures
 * this means you cannot define variables with a value of a module or functions
 * which take modules as arguments *)
(* first class modules are a way to bridge this gap *)

module type X_int = sig
  val x : int
end

module Three : X_int = struct
  let x = 3
end

(* a first class module is created by packaging a module with a compatible
 * signature*)
let three = (module Three : X_int)

(* the module type can be omitted if it can be inferred *)
module Four = struct
  let x = 4
end

let numbers = [three; (module Four)]
(* anonymous modules are no more complex *)
let nums_with_anon = [three; (module struct let x = 5 end)]

(* to access the contents of a first class module, it needs to be unpacked into 
 * an ordinary module *)
module New_three = (val three : X_int)
let () =
  printf "%d\n" New_three.x

(* creating a function designed to take in a first class module *)
(* originally:
   let to_int m =
     let module M = (val m : X_int) in
     M.x
*)
(* more concisely *)
(* here, the parameter has been annotated *)
let to_int (module M : X_int) = M.x

let plus m1 m2 =
  (* create a new anonymous module with a field, x, equal to the sum of 
   * m.x, m2.x*)
  (module struct
    let x = to_int m1 + to_int m2
  end : X_int)

(* passing a first class module to a function *)
let six = plus three three
let () =
  printf "%d\n" (to_int six)

(* first class modules can contain more than simple values *)
module type Bumpable = sig
  type t
  val bump : t -> t
end
(* create a concrete implementation of the Bumpable signature *)
module Int_bumper = struct
  type t = int
  let bump n = n + 1
end

module Float_bumper = struct
  type t = float
  let bump n = n +. 1.
end

(* create first class modules *)
(* expose the polymorphic type t within Bumpable interface *)
let int_bumper = (module Int_bumper : Bumpable with type t = int)
let float_bumper = (module Float_bumper : Bumpable with type t = float)

(* make use of int_bumper leveraging concrete type t *)
(* this example does not work as presented in the book:
    * let (module Bumpable) = int_bumper in Bumpable.bump 3
*)

(* define a function to use first-class modules polymorphically *)
(* a locally abstract type, t, is used here to make use of the polymorphic type
 * passed at invocation *)
let bump_list
    (type a)
    (module B : Bumpable with type t = a)
    (l: a list)
  =
  List.map ~f:B.bump l

let bumped_int_list = bump_list int_bumper [1;2;3]
let bumped_float_list = bump_list float_bumper [1.2;2.3;3.4]
let () =
  print_int_list bumped_int_list;
  print_float_list bumped_float_list
