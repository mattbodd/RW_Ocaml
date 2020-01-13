open Core
open Base

(* functors are a powerful addition to modules which allow for functionality 
 * that is standardized between modules *)

(* trivial example *)
(* standard module *)
module type X_int = sig
  val x : int
end

(* functor *)
(* we annotate the input (and optionally output) to show that this functor
 * expects M which is an instance of X_int and will return an X_int *)
module Increment (M : X_int) : X_int = struct
  let x = M.x + 1
end

(* usage of module and functor *)
module Three = struct
  let x = 3
end

(* apply functor *)
module Four = Increment(Three)

let () =
  printf "%d - %d = %d\n" Four.x Three.x (Four.x - Three.x)

(* functors do not only apply to modules with exactly the same signature *)
module Three_and_more = struct
  let x = 3
  let y = "Three"
end

(* apply functor module with overlapping signature *)
module Four_and_more = Increment(Three_and_more)
let () =
  printf "Three_and_more { %d; %s } becomes: Four_and_more { %d }\n"
    Three_and_more.x Three_and_more.y Four_and_more.x

(* create a generic interval library *)
module type Comparable = sig
  type t
  val compare : t -> t -> int
end

(* an interface which Make_interval conforms to *)
module type Interval_intf = sig
  type t
  type endpoint
  val create       : endpoint -> endpoint -> t
  val is_empty     : t -> bool
  val contains     : t -> endpoint -> bool
  val intersect    : t -> t -> t
  val get_interval : t -> (endpoint * endpoint) option
end

module Make_interval (Endpoint : Comparable)
  (* use destructive substution to replace all occurances of endpoint with
   * Endpoint.t *)
  : Interval_intf with type endpoint := Endpoint.t =
struct

  (* because of destructive subtyping, the explicit definition of type endpoint
   * is unused 
     * NOT NEEDED
     type endpoint = Endpoint.t
  *)
  
  (* create a polymorphic endpoint of type Comparable *)
  type t = | Interval of Endpoint.t * Endpoint.t
           | Empty

  let create low high =
    if Endpoint.compare low high > 0 then Empty
    else Interval (low,high)

  let is_empty = function
    | Empty -> true
    | Interval _ -> false

  let contains t x =
    match t with
    | Empty -> false
    | Interval (l,h) ->
      Endpoint.compare x l >= 0 && Endpoint.compare x h <= 0

  let intersect t1 t2 =
    let min x y = if Endpoint.compare x y <= 0 then x else y in
    let max x y = if Endpoint.compare x y >= 0 then x else y in
    match t1, t2 with
    | Empty, _ | _, Empty -> Empty
    | Interval (l1,h1), Interval (l2,h2) ->
      create (max l1 l2) (min h1 h2)

  (* return polymorphic interval *)
  let get_interval t =
    match t with
    | Empty -> None
    | Interval (l,h) -> Some (l,h) 
end

(* create a module with an anonymous functor *)
module Int_interval =
  Make_interval(struct
    type t = int
    let compare = Int.compare
  end)

(* utility functions *)
let get_int_interval interval =
  match Int_interval.get_interval interval with
  | None -> "(Empty)"
  | Some (l,h) -> sprintf "(%d, %d)" l h

let three_seven_int_interval = Int_interval.create 3 7
let () =
  printf "is_empty new_int_interval: %b\n"
    (Int_interval.is_empty three_seven_int_interval);
  printf "new_int_interval contains 5: %b\n"
    (Int_interval.contains three_seven_int_interval 5);
  printf "new_int_interval contains 10: %b\n"
    (Int_interval.contains three_seven_int_interval 10)

let five_ten_int_interval = Int_interval.create 5 10
let intersection = Int_interval.intersect
    three_seven_int_interval five_ten_int_interval

let () =
  let three_seven_formatted =
    get_int_interval three_seven_int_interval in
  let five_ten_formatted =
    get_int_interval five_ten_int_interval in
  let intersection_formatted =
    get_int_interval intersection in
  printf "%s intersect %s = %s\n"
    three_seven_formatted
    five_ten_formatted
    intersection_formatted
