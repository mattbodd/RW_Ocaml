open Core

type service_info =
  { service_name : string;
    port         : int;
    protocol     : string;
  }

let service_info_of_string line =
  let matches =
    Re.exec (Re.Posix.compile_pat "([a-zA-Z]+)[ \t]+([0-9]+)/([a-zA-Z]+)") line
  in
  (* the type of service_info here can be inferred using field name analysis *)
  {service_name = Re.Group.get matches 1;
   port = Int.of_string (Re.Group.get matches 2);
   protocol = Re.Group.get matches 3;
  }

let ssh = service_info_of_string
    "ssh              22/tcp     # SSH Remote Login Protocol"

(* demonstrate the confusing nature of overlapping record names *)
type log_entry =
  { session_id : string;
    time       : Time_ns.t;
    important  : bool;
    message    : string;
  }
type heartbeat =
  { session_id     : string;
    time           : Time_ns.t;
    status_message : string;
  }
type logon =
  { session_id  : string;
    time        : Time_ns.t;
    user        : string;
    credentials : string;
  }

(* when performing operations on ambiguous record fields, OCaml will pick the 
 * most recently defined type containing that field name *)
(* here, OCaml would annotate t as t : logon *)
let get_session_id t = t.session_id
(* here the heartbeat type is explicitly specified *)
let get_heartbeat_session_id (t : heartbeat) = t.session_id

(* to avoid overlapping field names, distinct modules can be used *)
module Log_entry = struct
  type t =
  { session_id : string;
    time       : Time_ns.t;
    important  : bool;
    message    : string;
  }
end
module Heartbeat = struct
  type t =
  { session_id     : string;
    time           : Time_ns.t;
    status_message : string;
  }
end
module Logon = struct
  type t =
    { session_id  : string;
      time        : Time_ns.t;
      user        : string;
      credentials : string;
    }
end

(* combining field and label punning can lead to concise definitions *)
(* 

let create_log_entry ~session_id ~important message =
  { Log_entry.time = Time_ns.now ();
    Log_entry.session_id;
    Log_entry.important;
    Log_entry.message
  }

*)

(* to avoid having to qualify module names repeatedly for fields, only one
 * module qualification for a record field to be able to infer the rest *)
let create_log_entry ~session_id ~important message : Log_entry.t =
  { time = Time_ns.now (); session_id; important; message }

(* to create a new record based off an existing instance with a few changes 
 * functional updates come in handy *)
type client_info =
  { addr                : Unix.Inet_addr.t;
    port                : int;
    user                : string;
    credentials         : string;
    last_heartbeat_time : Time_ns.t;
  }

(* originally 

let register_heartbeat t hb =
  { addr                = t.addr;
    port                = t.port;
    user                = t.user;
    credentials         = t.credentials;
    last_heartbeat_time = hb.Heartbeat.time;
  }

*)

(* using functional updates to write the above code snippet more tersely *)
(* here we create a new record of type client_info with only the
 * last_heartbeat_time field differing from the values in t *)
let register_heartbeat t hb =
  { t with last_heartbeat_time = hb.Heartbeat.time }

(* functional updates can be very helpful but also difficult to manage as
 * updates to a type will not impact functional updates. This means that if a 
 * new field is added to a type, a functional update will not care about the 
 * new field added and will continue to work although maybe ~not~ as expected *)

(* by default, records are immutable, although their individual fiels can be 
 * made mutable *)
type mut_client_info =
  { addr                          : Unix.Inet_addr.t;
    port                          : int;
    user                          : string;
    credentials                   : string;
    mutable last_heartbeat_time   : Time_ns.t;
    mutable last_heartbeat_status : string;
  }

let mut_register_heartbeat (t : mut_client_info) hb =
  t.last_heartbeat_time <- hb.Heartbeat.time

(* accessing a particular field from a collection of records is a common enough 
 * idiom that Core includes syntax to generate commonly used functions *)
                             
(* original

let get_users logons =
  List.dedup_and_sort ~compare:String.compare
    (List.map logons ~f:(fun x -> x.Logon.user))

*)
(* the ppx_jane preprocessor must be included when building in order to use 
 * this functionality *)
module Logon_ppx = struct
  type t =
    { session_id: string;
      time        : Time_ns.t;
      user        : string;
      full_name   : string;
      credentials : string;
    }
  [@@deriving fields]
end

(* we can now create a generic function for displaying a record field *)
let show_field field to_string record =
  (* Field.name is a generic way of accessing the name of a provided field *)
  let name = Field.name field in
  let field_string = to_string (Field.get field record) in
  name ^ ": " ^ field_string

(* creating an instnace of the Logon_ppx module *)
let logon = { Logon_ppx.
              session_id = "26685";
              time = Time_ns.of_string "2019-12-26 11:46:30 EST";
              user = "mboddewyn";
              full_name = "Matthew Boddewyn";
              credentials = "S0m3CR3d"
            }

(* utilize List like functions provided by ppx_jane *)
(* when updating the fields of a record, a generic function like iter can be
 * quickly adadpted to account for this new field by providing a function 
 * for handling that new field *)
let print_logon_ppx logon =
  let print to_string field =
    printf "%s\n" (show_field field to_string logon)
  in
  Logon_ppx.Fields.iter
    ~session_id:(print Fn.id)
    ~time:(print Time_ns.to_string)
    ~user:(print Fn.id)
    ~full_name:(print Fn.id)
    ~credentials:(print Fn.id)

let () =
  printf "service_name: %s\nport %d\nprotocol %s\n"
    ssh.service_name ssh.port ssh.protocol;

  (* utilize first class field abilities *)
  printf "%s\n" (show_field Logon_ppx.Fields.user Fn.id logon);
  printf "%s\n" (show_field Logon_ppx.Fields.time Time_ns.to_string logon);
  (* print all fields of logon *)
  print_logon_ppx logon;
