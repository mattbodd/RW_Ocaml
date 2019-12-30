open Core

(* define the type of an element within the doubly-linked list *)
type 'a element =
  { value        : 'a;
    mutable next : 'a element option;
    mutable prev : 'a element option
  }

(* create a mutable pointer to the head of the list *)
type 'a t = 'a element option ref

(* create should return an empty reference at first *)
let create () = ref None
let is_empty t = !t = None

(* getter for value attribute *)
let value elt = elt.value

let first t = !t
let next elt = elt.next
let prev elt = elt.prev

(* insert an element to the front of the list *)
let insert_first t value =
  let new_elt = { prev = None; next = !t; value } in
  begin match !t with
    | Some old_first -> old_first.prev <- Some new_elt
    | None -> ()
  end;
  t := Some new_elt;
  new_elt

(* insert an element after some arbitrary element *)
let insert_after elt value =
  let new_elt = { value; prev = Some elt; next = elt.next } in
  begin match elt.next with
    | Some old_next -> old_next.prev <- Some new_elt
    | None -> ()
  end;
  elt.next <- Some new_elt;
  new_elt

let remove t elt =
  let { prev; next; _ } = elt in
  begin match prev with
    | Some prev -> prev.next <- next
    | None -> t := next
  end;
  begin match next with
    | Some next -> next.prev <- prev
    | None -> ()
  end;
  elt.prev <- None;
  elt.next <- None

let iter t ~f =
  let rec loop = function
    | None -> ()
    | Some el -> f (value el); loop (next el)
  in
  loop !t

let find_el t ~f =
  let rec loop = function
    | None -> None
    | Some elt ->
      if f (value elt) then Some elt
      else loop (next elt)
  in
  loop !t

(* utility functions *)
let print_dlist t =
  iter t ~f:(printf "%s -> ");
  printf "None\n"

(* example usage *)
let aDlist = create ();;

let () =
   printf "is_empty aDlist = %b\n" (is_empty aDlist)

let matt = insert_first aDlist "Matthew"
let _ = insert_first aDlist "Sree"

let () =
  printf "is_empty aDlist = %b\n" (is_empty aDlist);
  print_dlist aDlist

let liam = insert_after matt "Liam"
let phony = insert_after liam "Phony"

let () =
  print_dlist aDlist;
  remove aDlist phony;
  print_dlist aDlist

(* find entry *)
let search_for = "no one"
let found_matt = find_el aDlist ~f:(String.(=) search_for)
let () =
  match found_matt with
  | Some value' -> printf "Found %s\n" (value value')
  | None -> printf "Could not find %s!\n" search_for

