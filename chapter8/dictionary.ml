open Core

(* fundamental dictionary element *)
(* maintains a mutable notion of length, as well as buckets which are arrays *)
type ('a, 'b) t = { mutable length : int;
                    buckets: ('a * 'b) list array;
                  }

let num_buckets = 17

(* function to generate a has for a given key *)
let hash_bucket key = (Hashtbl.hash key) % num_buckets

(* function to create empty hashmap with num_buckets buckets where each is a
 * list of 'a elements *)
let create () =
  { length = 0;
    buckets = Array.create ~len:num_buckets [];
  }

(* simply returns the current size of the hashmap *)
let length t = t.length

(* fill in the find function which will search through a given bucket *)
let find t key =
  List.find_map t.buckets.(hash_bucket key)
    ~f:(fun (key', data) -> if key' = key then Some data else None)

let iter t ~f =
  (* for each bucket *)
  for i = 0 to Array.length t.buckets - 1 do
    (* iterate over the elements in a given bucket *)
    List.iter t.buckets.(i) ~f:(fun (key, data) -> f ~key ~data)
  done

(* check to see if a given bucket contains a given key *)
let bucket_has_key t i key =
  List.exists t.buckets.(i) ~f:(fun (key',_) -> key' = key)

(* add a given (key, data) pair to the hashmap *)
let add t ~key ~data =
  (* hash on key *)
  let i = hash_bucket key in
  (* check to see if bucket already has the specified key *)
  let replace = bucket_has_key t i key in
  let filtered_bucket =
    if replace then
      (* if replacing, keep track of those (key, value) pairs within a given 
       * bucket that do not contain the same key *)
      List.filter t.buckets.(i) ~f:(fun (key',_) -> key' <> key)
    else
      (* if not replacing, entire bucket will be used unmodified *)
      t.buckets.(i)
  in
  (* overwrite given bucket with (key, value) pair added *)
  t.buckets.(i) <- (key, data) :: filtered_bucket;
  (* if not replacing, increment the counter *)
  if not replace then t.length <- t.length + 1

(* remove a given key *)
let remove t key =
  (* hash on key *)
  let i = hash_bucket key in
  (* if bucket contains the specified key *)
  if bucket_has_key t i key then (
    (* create new bucket without specified key *)
    let filtered_bucket =
        List.filter t.buckets.(i) ~f:(fun (key',_) -> key' <> key)
    in
    (* overwrite bucket *)
    t.buckets.(i) <- filtered_bucket;
    (* decrement length *)
    t.length <- t.length - 1
  )

(* example usage *)
(* create a hashmap *)
let hmap = create ()

(* add values to the hashmap *)
let instantiate_hmap () =
  add hmap ~key:"Matthew" ~data:"Matt";
  add hmap ~key:"Robert" ~data:"Bob";
  add hmap ~key:"Sarah" ~data:"Sarah";
  add hmap ~key:"Emily" ~data:"Emmie"

(* utilize function to print the contents of the hashmap *)
let print_hmap () =
  printf "{\n";
  iter hmap ~f:(fun ~key -> fun ~data -> printf "  %s: %s\n" key data);
  printf "}\n"

let () =
  instantiate_hmap ();
  print_hmap ();
  printf "Size: %d\n" hmap.length;
  remove hmap "Robert";
  printf "Size: %d\n" hmap.length;
  print_hmap ()
