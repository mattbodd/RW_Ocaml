open Core

(* In/Out_channel derive from Core's library and are built off of the Unix
 * module provided natively *)
(* communication with the terminal is organized around three channels:
   * In_channel.stdin
   * Out_channel.stdout
   * Out_channel.stderr
*)

(* display time in specified timezone *)
let () =
  Out_channel.output_string stdout "Pick a timezone: ";
  (* a call to output_string will not necessarily invoke a system call to write
   * to Out_channel if the output string is short enough *)
  Out_channel.flush stdout;
  match In_channel.input_line In_channel.stdin with
  (* notice that In_channel.stdin returns a string option which can be None
   * if a terminating character is received *)
  | None -> failwith "No timezone provided"
  | Some zone_string ->
    let zone = Time.Zone.find_exn zone_string in
    let time_string = Time.to_string_abs (Time.now ()) ~zone in
    Out_channel.output_string stdout
      (String.concat
         ["The time in ";Time.Zone.to_string zone;" is ";time_string;".\n"]);
    Out_channel.flush stdout

(* work with file streams *)
let create_number_file filename numbers =
  let outc = Out_channel.create filename in
  List.iter numbers ~f:(fun x -> Out_channel.fprintf outc "%d\n" x);
  Out_channel.close outc

let sum_over_file filename =
  let file = In_channel.create filename in
  (* this sort of protection is critical to prevent unreaped resources *)
  Exn.protect ~f:(fun () ->
      let numbers = List.map ~f:Int.of_string (In_channel.input_lines file) in
      List.fold ~init:0 ~f:(+) numbers)
    ~finally:(fun () -> In_channel.close file)

let () =
  create_number_file "numbers.txt" [1;2;3;4;5];
  printf "Total: %d\n" (sum_over_file "numbers.txt")
