open Core

(* define elements in a boolean language *)
(* doing so using a recusively defined variant *)
(* concrete type is generically defined as 'a for Base *)
type 'a expr =
  | Base  of 'a
  | Const of bool
  | And   of 'a expr list
  | Or    of 'a expr list
  | Not   of 'a expr

(* mail type *)
type mail_field = To | CC | Date | Subject
type mail_predicate = { field : mail_field;
                        contains : string
                      }

(* helper functions *)
(* end helper functions *)

(* evaluate a given expression *)
let rec eval expr base_eval =
  let eval' expr = eval expr base_eval in
  match expr with
  (* evaluate the Base type *)
  | Base  base  -> base_eval base
  (* stright forward boolean evaluation *)
  | Const bool  -> bool
  (* all elements must evaluate to true for And to hold true *)
  | And   exprs -> List.for_all exprs ~f:eval'
  (* only one member need evaluate to true for Or to hold true *)
  | Or    exprs -> List.exists exprs ~f:eval'
  (* call on existing not logic *)
  | Not   expr  -> not (eval' expr)

(* simply a given expression *)
let and_ l =
  (* check to see if a false exists in the given expression *)
  if List.exists l ~f:(function Const false -> true | _ -> false)
  then Const false
  else
    match List.filter l ~f:(function Const true -> false | _ -> true) with
    | [] -> Const true
    | [ x ] -> x
    | l -> And l

let or_ l =
  (* check to see if a true exists in the given expression *)
  if List.exists l ~f:(function Const true -> true | _ -> false) then Const true
  else
    match List.filter l ~f:(function Const false -> false | _ -> true) with
    | [] -> Const false
    | [ x ] -> x
    | l -> Or l

let not_ = function
  | Const b -> Const (not b)
  | Not e -> e
  | (Base _ | And _ | Or _) as e -> Not e

(* apply the simplification routines *)
let rec simplify = function
  | Base _ | Const _ as x -> x
  | And l -> and_ (List.map ~f:simplify l)
  | Or l  -> or_  (List.map ~f:simplify l)
  | Not e -> not_ (simplify e)

(* construct a Base value *)
let test field contains = Base { field; contains }

(* function used to insert proper number of indents *)
let rec pp_indenter indent_level =
  match indent_level with
  | 0 -> ""
  | _ -> "  " ^ (pp_indenter (indent_level-1))

let pp ?(closing=false) indent_level =
  match indent_level with
  | 0 -> ""
  | _ when closing -> pp_indenter indent_level
  | _ -> "\n" ^ (pp_indenter indent_level)

let open_ t sym indent_level =
  sprintf "%s%s %s" (pp indent_level) t sym

let close_ sym indent_level =
  sprintf "\n%s%s" (pp ~closing:true indent_level) sym

let rec print_expr indent_level = function
  | Const b -> printf "%s%b" (pp indent_level) b
  | Base b  -> printf "%sBase %s" (pp indent_level) b
  | And l   ->
    printf "%s" (open_ "And" "[" indent_level);
    (List.iter ~f:(fun e -> print_expr (indent_level+1) e; printf "; ") l);
    printf "%s" (close_ "]" indent_level)
  | Or l    ->
    printf "%s" (open_ "Or" "[" indent_level);
    (List.iter ~f:(fun e -> print_expr (indent_level+1) e; printf "; ") l);
    printf "%s" (close_ "]" indent_level)
  | Not e   ->
    printf "%s" (open_ "Not" "(" indent_level);
    print_expr (indent_level+1) e;
    printf "%s" (close_ ")" indent_level)

let some_expr =
  And [ Or [ test To "mboddewy";
             test CC "sree" ];
        test Subject "runtime";
      ]

let simplifiable_expr =
  (Not
     (And
        [Or [Base "it's snowing"; Const true];
         Base "it's raining"]
     )
  )

let simplified_expr = simplify simplifiable_expr

let () =
  (* print original expression *)
  print_expr 0 simplifiable_expr;
  printf "\n\n";
  (* print simplified expression *)
  print_expr 0 simplified_expr;
  printf "\n";
