open Core
open Stdio

(* helper functions *)
let print_int_list aList =
  printf "{ ";
  List.iter ~f:(fun i -> printf "%d " i) aList;
  printf "}\n"
(* end helper functions *)

(* define a variant type with 8 variant values *)
type basic_color =
  | Black | Red | Green | Yellow | Blue | Magenta | Cyan | White

let basic_color_to_int = function
  | Black -> 0  | Red     -> 1 | Green -> 2 | Yellow -> 3
  | Blue  -> 4  | Magenta -> 5 | Cyan  -> 6 | White  -> 7

let blue_red = List.map ~f:basic_color_to_int [Blue; Red]

(* generate a blue string in most terminals *)
let color_by_number number text =
  Printf.sprintf "\027[38;5;%dm%s\027[0m\n" number text

(* more complex variant type examples *)
type weight = Regular | Bold

type color =
  | Basic of basic_color * weight
  | RGB   of int * int * int
  | Gray  of int

(* instantiate more complex variant instances *)
let some_colors = [RGB (250,70,70); Basic (Green, Regular)]

let complex_color_to_int = function
  | Basic (basic_color, weight) ->
    let base = match weight with Bold -> 8 | Regular -> 0 in
    base + basic_color_to_int basic_color
  | RGB (r,g,b) -> 16 + b + g * 6 + r * 36
  | Gray i -> 232 + i

let color_print color s =
  printf "%s\n" (color_by_number (complex_color_to_int color) s)

(* algebraic data types leverage the power of product and sum types *)
(* this becomes especially useful when types contain similar information *)
(* here we only store the information that is unique to Log_entry *)
module Log_entry = struct
  type t = { important : bool;
             message   : string;
           }
end
(* here we only store the information that is unique to Heartbeat *)
module Heartbeat = struct
  type t = { status_message : string }
end
(* here we only store the information that is unique to Logon *)
module Logon = struct
  type t = { user        : string;
             credentials : string;
           }
end
(* use a variant to house the specific, varying details *)
type details =
  | Logon of Logon.t
  | Heartbeat of Heartbeat.t
  | Log_entry of Log_entry.t
                  
(* use a record to house the common and specific information *)
module Common = struct
  type t = { session_id : string;
             time: Time_ns.t;
           }
end

(* retrieve those messages from a specific user within a list of messages *)
let messages_for_user user (messages : (Common.t * details) list) =
  let (user_messages,_) =
    (* accumulate a list of messages whose user value matches the user input *)
    List.fold messages ~init:([],Set.empty (module String))
      ~f:(fun ((messages,user_sessions) as acc) ((common,details) as message) ->
          match details with
          | Logon m ->
            if String.(=) m.user user then
              (message::messages, Set.add user_sessions common.session_id)
            else
              acc
          | Heartbeat _ | Log_entry _ ->
            if Set.mem user_sessions common.session_id then
              (message::messages, user_sessions)
            else
              acc
        )
  in
  List.rev user_messages

(* function to handle alpha channel with RGB values *)
type extended_color =
  | Basic of basic_color * weight
  | RGB   of int * int * int
  | Gray  of int
  | RGBA  of int * int * int * int

(* polymorphic variant implementation *)
let poly_basic_color_to_int = function
  | `Black -> 0 | `Red     -> 1 | `Green -> 2 | `Yellow -> 3
  | `Blue  -> 4 | `Magenta -> 5 | `Cyan  -> 6 | `White  -> 7

let poly_color_to_int = function
  | `Basic (basic_color,weight) ->
    let base = match weight with `Bold -> 8 | `Regular -> 0 in
    base + poly_basic_color_to_int basic_color
  | `RGB (r,g,b) -> 16 + b + g * 6 + r * 36
  | `Gray i -> 232 + i

let extended_color_to_int = function
  | `RGBA (r,g,b,a) -> 256 + a + b * 6 + g * 36 + r + 216
  | (`Basic _ | `RGB _ | `Gray _) as color -> poly_color_to_int color

let () =
  print_int_list blue_red;
  printf "%s" (color_by_number 6 "Cyan");
  color_print (Basic (Magenta,Bold)) "A bold magenta!";
