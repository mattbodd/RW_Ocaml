open Core

(* lazy computation is only performed when it is actually used *)
let z = lazy (print_endline "performing lazy computation"; Float.sqrt 36.)
let () =
  printf "%f\n" (Lazy.force z)

(* re-implementation of lazy values *)
type 'a lazy_state =
  | Delayed of (unit -> 'a)
  | Value of 'a
  | Exn of exn

(* create a lazy state *)
let create_lazy f = ref (Delayed f)
let v = create_lazy (fun () ->
    print_endline "performing lazy computation"; Float.sqrt 36.)

(* evaluate or retrieve a lazy_state value *)
let force v =
  match !v with
  | Value x -> x
  | Exn e -> raise e
  | Delayed f ->
    try
      let x = f () in
      (* on successful evaluation, the lazy_state passed in should be a Value *)
      v := Value x;
      x
    with exn ->
      (* ni the event of an error, the lazy_state passed in should be an 
       * exception *)
      v := Exn exn;
      raise exn

(* use re-implementation *)
let () =
  printf "%f\n" (force v)

(* memoization is a technique used to leverage redundant invocations *)
(* this function will memoize an input function with a single argument *)
let memoize f =
  let memo_table = Hashtbl.Poly.create () in
  (fun x ->
     Hashtbl.find_or_add memo_table x ~default:(fun () -> f x))

(* utility function to measure computation of function *)
let time f =
  let start = Time.now () in
  (* store output of call to f *)
  let x = f () in
  let stop = Time.now () in
  printf "Time: %F ms\n" (Time.diff stop start |> Time.Span.to_ms);
  x

(* example of fibonacci sequence computation aided by memoization *)
(* initial implementation *)
let rec nomemo_fib i =
  if i <= 1 then i
  else nomemo_fib (i - 1) + nomemo_fib (i - 2)

(* without memoization *)
(* Time: 0.0307559967041 ms *)
let _ = time (fun () -> nomemo_fib 20)
(* Time: 463.644981384 ms *)
let _ = time (fun () -> nomemo_fib 40)

let memo_rec f_norec x =
  let fref = ref (fun _ -> assert false) in
  let f = memoize (fun x -> f_norec !fref x) in
  fref := f;
  f x


let fib = memo_rec (fun fib i ->
    if i <= 1 then 1 else fib (i - 1) + fib (i - 2))

(* with memoization *)
(* Time: 0.00524520874023 ms *)
let _ = time (fun () -> fib 20)
(* Time: 0.00619888305664 ms *)
let _ = time (fun () -> fib 40)

(* the let rec construct excludes certain kinds of assignments.
 * It will allow:
   * function definitions
   * constructors
   * lazy evaluations 
 * this means chaining mutually recursive functions can be rejected to avoid 
 * sitautions like: let rec x = x + 1 *)
(* in order to construct a mutually recursive definition of our memoized 
 * fibonacci sequence calculator, we can use lazy values *)
let fib_norec fib i =
  if i <= 1 then i
  else fib (i - 1) + fib (i - 2)
         
let lazy_memo_rec f_norec x =
  let rec f = lazy (memoize (fun x -> f_norec (Lazy.force f) x)) in
  (Lazy.force f) x

(* Time: 0.00190734863281 ms *)
let _ = time (fun () -> lazy_memo_rec fib_norec 20)
(* Time: 0.00262260437012 ms *)
let _ = time (fun () -> lazy_memo_rec fib_norec 40)
