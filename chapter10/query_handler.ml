open Core_kernel
open Sexplib

module type Query_handler = sig
  
  type config [@@deriving sexp]

  (* identifier for a given query handler *)
  val name : string

  (* query handler state *)
  type t

  (* create a new query handler from a given config *)
  val create : config -> t

  (* evaluate a givne query, where both input and output are s-expressions *)
  val eval : t -> Sexp.t -> Sexp.t Or_error.t
end

(* implementation of Query_handler meant to produce unique identifiers *)
module Unique = struct
  (* instantiate s-expression handler for type in *)
  type config = int [@@deriving sexp]
  (* realize type t as a mutable integer *)
  type t = { mutable next_id : int }

  (* meta label for instances of Unique module *)
  let name = "unique"
  (* create an instance of Unique with a specified starting value *)
  let create start_at = { next_id = start_at }

  (* method to create a unique next_id for the given instance
   * in this case, generating unique next_id's is simply defined as incrementing
   * from the start value *)
  let eval t sexp =
    (* unit_of_sexp will convert the specified sexp to a value of type unit
     * in this case, `Ok ()` tries to match the return value indicating that 
     * the specified sexp is a well-formed s-expression *)
    (* unit_of_sexp is not the only viable option but alternatives return either
     * an irrelevant finite number of values
     * (match bool_of_sexp with -> | true | false)
     * or provide an infinite number of values
     * (match string_of_sexp with -> | Ok _)
     * which would simply be discarded as the value ultimately returned is not 
     * tied to the input to invocations of eval *)
    match Or_error.try_with (fun () -> unit_of_sexp sexp) with
    | Error _ as err -> err
    | Ok () ->
      (* construct an ok value of type sexp.t from an integer (next_id) *)
      let response = Ok (Int.sexp_of_t t.next_id) in
      (* increment the value of next_id *)
      t.next_id <- t.next_id + 1;
      (* return the ok value containing the sexp.t *)
      response
end

(* another implementation of the Query_handler module signature *)
module List_dir = struct
  (* instantiate s-expression handler for type string *)
  type config = string [@@deriving sexp]
  (* realize type cwd as a string *)
  type t = { cwd : string }

  (* return true if the path is absolute (starts with '/') *)
  let is_abs p =
    String.length p > 0 && Char.(=) p.[0] '/'

  (* meta label for instances of List_dir module *)
  let name = "ls"
  (* create an instance of List_dir with a specified working 'root' directory *)
  let create cwd = { cwd }

  let eval t sexp =
    (* attempt to create a string from the specified s-expression *)
    match Or_error.try_with (fun () -> string_of_sexp sexp) with
    (* if a string cannot be created, return an error *)
    | Error _ as err -> err
    (* if a string can be created, wrap in an Ok value *)
    | Ok dir ->
      (* ensure that dir is absolute by concatenating 'root' (cwd) with
       * specified value *)
      let dir =
        (* do not alter dir if already absolute *)
        if is_abs dir then dir
        (* concatenate 'root' with specified value *)
        else Core.Filename.concat t.cwd dir
      in
      (* create an Ok value that is an s-expression of string s-expressions 
       * created from reading all files present in dir *)
      (* Array.sexp_of_t : ('a -> Sexp.t) -> 'a sexp_array -> Sexp.t *)
      Ok (Array.sexp_of_t String.sexp_of_t (Core.Sys.readdir dir))
end

(* utility functions *)
(* take in a s-expression *)
let print_str_sexp sexp =
  (* convert a s-expression to a string *)
  Sexp.to_string_hum sexp
  |> print_endline

(* create an instance of Unique which starts incrementing at 0 *)
let unique = Unique.create 0
(* create an instance of List_dir which will default to searching from /var *)
let list_dir = List_dir.create "/var"

(* demonstrate simple usage of Query_handler implementations *)
let () =
  (* here, Or_error.ok_exn will return the contents of the Ok constructor or 
   * throw an appropriate exception *)
  (* notice that by opening Core, ok_exn is in the global namespace and can be 
   * referred to without prefixing with Or_error *)
  (* evaluate Unique implementation *)
  print_str_sexp (Or_error.ok_exn (Unique.eval unique (Sexp.List [])));
  print_str_sexp (ok_exn (Unique.eval unique (Sexp.List [])));
  print_str_sexp (ok_exn (Unique.eval unique (Sexp.List [])));
  (* evaluate List_dir implementation *)
  print_str_sexp (Or_error.ok_exn (List_dir.eval list_dir (sexp_of_string ".")))

(* create a first-class module to handle easy instantiation of Query_handler *)
module type Query_handler_instance = sig
  (* the module name here happens to be the same as its type: Query_handler *)
  module Query_handler : Query_handler
  (* inherit the type of a particular Query_handler *)
  val this : Query_handler.t
end

let build_instance
    (* define a locally abstract type *)
    (type a)
    (* parameter, Q, must be a Query_handler (from above signature) instance
     * with config type a *)
    (module Q : Query_handler with type config = a)
    config
  =
  (* instantiate an abstract module *)
  (module struct
    module Query_handler = Q
    (* create a record of type Query_handler.t *)
    let this = Q.create config
    (* annotate the type of the anonymous module to be Query_handler_instance *)
  end : Query_handler_instance)

(* utilize build_instance to create an instance of a module in one line *)
let unique_instance = build_instance (module Unique) 0
let list_dir_instance = build_instance (module List_dir) "/var"

(* build a dispatch table which maps module names to their instances *)
let build_dispatch_table handlers =
  (* create a table with strings for keys *)
  let table = Hashtbl.create (module String) in
  (* fill table using provided handlers *)
  List.iter handlers
    (* retrieve the name of an instance and store alongside the instance *)
    ~f:(fun ((module I : Query_handler_instance) as instance) ->
        Hashtbl.set table ~key:I.Query_handler.name ~data:instance);
  table

(* function to perform methods for a specified instance *)
let dispatch dispatch_table name_and_query =
  match name_and_query with
  (* name_and_query should be presented as a list of name (Atomic s-expression)
   * along with the query *)
  | Sexp.List [Sexp.Atom name; query] ->
    (* look for name within dispatch_table *)
    begin match Hashtbl.find dispatch_table name with
      (* if name could not be found *)
      | None ->
        Or_error.error "Could not find matching handler"
          name String.sexp_of_t
      (* if name could be found *)
      | Some (module I : Query_handler_instance) ->
        (* evaluate through the generic module *)
        (* utilize the specific instance I *)
        I.Query_handler.eval I.this query
    end
  | _ ->
    Or_error.error_string "malformed query"

let rec cli dispatch_table =
  (* start of input line *)
  printf ">>> %!";
  let result =
    match In_channel.(input_line stdin) with
    (* if EOF is passed, return stopped state *)
    | None -> `Stop
    | Some line ->
      match Or_error.try_with (fun () ->
          (* create s-expression from input line *)
          Core_kernel.Sexp.of_string line)
      with
      (* pass up error message if encountered and continue to accept input*)
      | Error e -> `Continue (Error.to_string_hum e)
      (* if quit is specified, terminate cli *)
      | Ok (Sexp.Atom "quit") -> `Stop
      | Ok query ->
        (* for well-formed query, attempt to dispatch *)
        begin match dispatch dispatch_table query with
          | Error e -> `Continue (Error.to_string_hum e)
          | Ok s    -> `Continue (Sexp.to_string_hum s)
        end;
  in
  match result with
  (* when `Stop is received, no longer recurse *)
  | `Stop -> ()
  (* when continue is received print the ouput and recursively evaluate another
   * input *)
  | `Continue msg ->
    (* subtle point: %! in printf will flush the output *)
    printf "%s\n%!" msg;
    cli dispatch_table

(* run cli: 
   let () =
     cli (build_dispatch_table [unique_instance; list_dir_instance])
*)

(* dynamically load and unload query handlers *)
module Loader = struct
  type config = (module Query_handler) list sexp_opaque
  [@@deriving sexp]
      
  (* create two different basic hashtables with (key : string, value : 'b) *)
  type t = { known  : (module Query_handler)          String.Table.t
           ; active : (module Query_handler_instance) String.Table.t
           }
           
  let name = "loader"
    
  let create known_list =
    (* create an empty table for active *)
    let active = String.Table.create () in
    (* create an empty tbale for known *)
    let known  = String.Table.create () in
    (* iterate over Query_handlers in known_list *)
    List.iter known_list
      ~f:(fun ((module Q : Query_handler) as q) ->
          Hashtbl.set known ~key:Q.name ~data:q);
    { known; active }

  let load t handler_name config =
    (* if the specified name is already contained in the active table, reject *)
    if Hashtbl.mem t.active handler_name then
      Or_error.error "Can't re-register an active handler"
        handler_name String.sexp_of_t
    else
      (* look handler up in known table *)
      match Hashtbl.find t.known handler_name with
      (* if handler name is not recognized, reject *)
      | None ->
        Or_error.error "Unknown handler" handler_name String.sexp_of_t
      (* if some Query_handler is pulled from the table *)
      | Some (module Q : Query_handler) ->
        let instance =
          (* create an anonymous module *)
          (module struct
            (* hold onto module (Query_handler) *)
            module Query_handler = Q
            (* store instance created using specified s-expression *)
            let this = Q.create (Q.config_of_sexp config)
          end : Query_handler_instance)
        in
        (* update active table *)
        Hashtbl.set t.active ~key:handler_name ~data:instance;
        (* return s-expression *)
        Ok Sexp.unit

  let unload t handler_name =
    (* verify handler is active to begin with, if not then reject *)
    if not (Hashtbl.mem t.active handler_name) then
      Or_error.error "Handler not active" handler_name String.sexp_of_t
      (* if trying to unload 'loader' give warning *)
    else if handler_name = name then
      Or_error.error_string "It's unwise to unload yourself"
    else (
      (* remove the specified s-expression from the table *)
      Hashtbl.remove t.active handler_name;
      Ok Sexp.unit
    )

  (* variant to store the different possible types of requests specified by the
   * user *)
  type request =
    | Load of string * Sexp.t
    | Unload of string
    | Known_services
    | Active_services
  [@@deriving sexp]

  let eval t sexp =
    match Or_error.try_with (fun () -> request_of_sexp sexp) with
    | Error _ as err -> err
    | Ok resp ->
      match resp with
      | Load (name,config) -> load   t name config
      | Unload name        -> unload t name
      | Known_services ->
        Ok ([%sexp_of: string list] (Hashtbl.keys t.known))
      | Active_services ->
        Ok ([%sexp_of: string list] (Hashtbl.keys t.active))
end

let () =
  let loader = Loader.create [(module Unique); (module List_dir)] in
  let loader_instance =
    (module struct
      module Query_handler = Loader
      let this = loader
    end : Query_handler_instance)
  in
  Hashtbl.set loader.Loader.active
    ~key:Loader.name ~data:loader_instance;
  cli loader.Loader.active
