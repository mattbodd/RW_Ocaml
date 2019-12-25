open Base
open Stdio

let build_counts () =
  (* In_channel.fold_lines will behave similarly to List.fold in that it will
   * take in incoming lines and accumulate them starting with an empty list *)
  In_channel.fold_lines In_channel.stdin ~init:[] ~f:(fun counts line ->
    (* count will retrieve the existing frequency (or 0) of a particular line *)
    let count =
      match List.Assoc.find ~equal:String.equal counts line with
      | None -> 0
      | Some x -> x
    in
    (* a new list is allocated with a new (key, value) of (line, count+1) *)
    List.Assoc.add ~equal:String.equal counts line (count + 1)
  )

let () =
  build_counts ()
  (* sort (key, value) list in descending order on value *)
  |> List.sort ~compare:(fun (_,x) (_,y) -> Int.descending x y)
  (* retrieve the first 10 items (top 10) *)
  |> (fun l -> List.take l 10)
  (* print the top 10 *)
  |> List.iter ~f:(fun (line,count) -> printf "%3d: %s\n" count line)
