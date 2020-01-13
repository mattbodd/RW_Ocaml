open Core

let s = object
  val mutable v = [0;2]

  method pop =
    match v with
    | hd :: tl ->
      v <- tl;
      Some hd
    | [] -> None

  method push hd =
    v <- hd :: v

  method print_ints =
    List.iter ~f:(fun i -> printf "%d " i) v;
    printf "\n"
end

let () =
  (* print initial list *)
  s#print_ints;
  (* pop value *)
  match s#pop with
  | None -> printf "Could not pop empty list\n"
  | Some popped -> printf "Popped %d\n" popped;
  (* print modified list *)
  s#print_ints;
  (* push value *)
  s#push 1;
  (* print modified list *)
    s#print_ints

(* note that the methods within s were allowed to have no parameters unlike 
 * typical functions which have to at least take a unit parameter 
 * the difference stems from the observation that methods are rooted in object
 * instances (think self in Python or this in Java)*)
(* functions can be constructed by functions *)
let stack init = object
  val mutable v = init

  method pop =
    match v with
    | hd :: tl ->
      v <- tl;
      Some hd
    | [] -> None

  method push hd =
    v <- hd :: v

  (* getter for the inner list representing the stack *)
  method get_list =
    v
end

(* helper method to print integer values from stack *)
let print_ints s =
  List.iter ~f:(fun i -> printf "%d " i) s#get_list;
  printf "\n"

let fs = stack [13;11;12]

let () =
  (* print initial list *)
  print_ints fs;
  (* pop value *)
  match fs#pop with
  | None -> printf "Could not pop empty list\n"
  | Some popped -> printf "Popped %d\n" popped;
  (* print modified list *)
  print_ints fs;
  (* push value *)
  fs#push 10;
  (* print modified list *)
  print_ints fs

(* objects can contain polymorphic values and functions *)
let area sq = sq#width * sq#width
(* by annotating the value of width, the object is said to be closed meaning 
 * any input should only have the width field of type int *)
let area_closed (sq : < width : int >) = sq#width * sq#width

(* objects can be made to be immutable *)
let imm_stack init = object
  val v = init

  method pop =
    match v with
    (* this implementation creates copies of the object rather than updating
     * mutable fields *)
    | hd :: tl -> Some (hd, {< v = tl >})
    | []       -> None

  method push hd =
    {< v = hd :: v >}
end

let imm_s = imm_stack [3;2;1]
let () = s#push 4
let (ret : int option) = s#pop
let () =
  match ret with
  | Some num -> printf "%d\n" num
  | None     -> printf "Cannot pop empty list\n"

(* subtyping governs when an object of type A can be used in an expression that 
 * expects an object of type B *)
(* define a generic type, shape *)
type shape = < area : float >
(* implement an instance of shape *)
type square = < area : float; width : int >
let square w = object
  method area = Float.of_int (w * w)
  method width = w
end
(* this is an instance of width subtyping which is characterized by similar 
 * objects where one object (square here) has all the methods of another object
 * (shape here) and possibly more (width here) *)
let aSquare = (square 10 :> shape)

(* specify a signature for a new type  *)
type circle = < area : float; radius : int >
(* define the new type *)
let circle r = object
  method area = Float.acos(-1.) *. (Float.of_int r) **. 2.0
  method radius = r
end
(* instantiate an object of new type *)
(* notice that coin implements a shape method which is an instance of circle 
 * (a shape) *)
let coin = object
  method shape = circle 5
  method color = "silver"
end
(* map also implements a shape method which calls upon square this time *)
let map = object
  method shape = square 10
end
(* an item need only have a shape method which returns a shape object *)
type item = < shape : shape >
(* items is a list of shapes *)
(* each element is a shape coerced into an item  *)
let items = [(coin :> item) ; (map :> item) ]
(* the example above demonstrates depth subtyping which allows for objects to be
 * coerced if its individual methods can be safely coerced *)

(* polymorphic variant subtyping is used when coercing a polymorphic variant
 * into a larger polymorphic variant type *)
(* a polymorphic variant type A is a subtype of B if the tags of A are a subset
 * of the tags of B *)
type num = [ `Int of int | `Float of float ]
type const = [ num | `String of string ]
let n : num = `Int 3
let c : const = (n :> const)

(* define a polymorphic function which can take any object which subtypes shape
 * (ie: can be coerced into a shape )*)
let shape_to_string : shape -> string =
  fun s -> Printf.sprintf "Shape(%F)\n" s#area
(* specify that square_to_string will only accept arguments which are square *)
let square_to_string : square -> string =
  (shape_to_string :> square -> string)

module Either = struct
  type ('a, 'b) t =
    | Left of 'a
    | Right of 'b
  let left x = Left x
  let right x = Right x
end

let left_square = Either.left (square 40)
let right_circle = Either.right (circle 20)
(* defining only a signature (hiding actual definition) results in an error in
 * this case unless we use variance annotations (+/-) *)
(* - signifies covariance and + signifies contravariance *)
(* a brief, initial take on covariance and contravariance is that covariance 
 * allows for increasing specificity while contravariance allows generality *)
module Var_either : sig
  type (+'a, +'b) t
  val left : 'a -> ('a, 'b) t
  val right : 'b -> ('a, 'b) t
end = Either
(* in this example, contravariance is requires as we are trying to upwardly
 * coerce from a square to a shape (specific to general *)
let _ = (Var_either.left (square 40) :> (shape, _) Var_either.t)

type 'a stack = < pop : 'a option; get_list : 'a list; push : 'a -> unit >
type 'a readonly_stack = < pop : 'a option >
                         
let square_stack : square stack = stack [ square 30; square 10 ]
let circle_stack : circle stack = stack [ circle 20; circle 40 ]

(* it's important to use a readonly_stack type for this implementation as the 
 * push method in the originally defined type is not compatible with non-shape
 * subtypes *)
(* conceptually, the issue is that if the square or circle stack could be 
 * coerced into a shape stack, any shape could be pushed onto that stack which
 * would be illegal *)
let total_area (shape_stacks: shape readonly_stack list) =
  let stack_area acc st =
    let rec loop acc =
      match st#pop with
      | Some s -> loop (acc +. s#area)
      | None -> acc
    in
    loop acc
  in
  List.fold ~init:0.0 ~f:stack_area shape_stacks

let total_float_area = total_area [(square_stack :> shape readonly_stack);
                                   (circle_stack :> shape readonly_stack)]
let () =
  printf "Total area: %.2f\n" total_float_area

(* row polymorphism vs subtyping *)
(* row polymorphism is often times preferred over subtyping as it does not
 * conceal type information and does not require explicit coercion *)
(* row polymorphism does have limitations as it cannot be used when mixing types
 * in datatypes which expect homogeneity *)
(* for example, row polymorphism will not permit a list of square and circle
 * even though they are both shapes *)
(* will fail
     let shape_list : < area : float; .. > list = [circle 20; square 20]
*)
(* will also fail
     let shape_ref = < area : float; .. > ref = ref (circle 20)
*)
(* subtyping will allow for the following to examples to work, however *)
(* notice that for both examples, explicit coersion must be performed *)
let shape_list : shape list = [(circle 20 :> shape); (square 20 :> shape)]
                              
let shape_ref : shape ref = ref (circle 20 :> shape)
let () =
  shape_ref := (square 20 :> shape)
