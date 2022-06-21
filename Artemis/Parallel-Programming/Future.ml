#directory "+threads";;
#load "unix.cma";;
#load "threads.cma";;

open Thread
open Event

module Future = struct
  type 'a msg = Result of 'a | Ex of exn
  type 'a t = 'a msg channel

  let create f a =
    let c = new_channel () in
    let task () =
      let r = try Result (f a) with e -> Ex e in
      sync (send c r)
    in
    let _ = Thread.create task () in
    c
  let get c =
    match sync (receive c) with
    | Result r -> r
    | Ex e -> raise e

  let then_ f c =
    let c' = new_channel () in
    let task () =
      let r = match sync (receive c) with
      | Result r -> Result (f r)
      | Ex e -> Ex e
      in
      sync (send c' r)
    in
    let _ = Thread.create task () in
    c'



  let when_any cs =
    let c' = new_channel () in
    let task () =
      let r = select (List.map receive cs) in
      sync (send c' r)
    in
    let _ = Thread.create task () in
    c'



  let when_all cs =
    let c' = new_channel () in
    let task () =
      let r = List.fold_left (fun a c -> sync (receive c)::a) [] cs |> List.rev in
      match List.find_opt (function Ex _ -> true | _ -> false) r with
      | Some (Ex e) -> sync (send c' (Ex e))
      | _ -> sync (send c' (Result (List.map (function Result r -> r | _ -> failwith "unreachable") r)))
    in
    let _ = Thread.create task () in
    c'

  (* additional stuff *)
  let memoize c =
    let c' = new_channel () in
    let task () =
      let r = sync (receive c') in
      let rec repeat () =
        sync (send c' r);
        repeat ()
      in
      repeat ()
    in
    let _ = Thread.create task () in
    c'

  let result_to receiver_c c =
    let task () =
      match sync (receive c) with
      | Result r -> sync (send receiver_c r)
      | Ex e -> raise e
    in
    let _ = Thread.create task () in
    ()

  let get_opt c = poll (receive c)
end



(* Future example *)
let read_lines filename =
  let file = open_in filename in
  let rec read_all l =
    try
      read_all (input_line file :: l)
    with End_of_file -> List.rev l
  in
  let content = read_all [] in
  close_in file;
  content

let write_file filename content =
  let file = open_out filename in
  output_string file content;
  close_out file

let print_list l =
  print_endline (String.concat "\n" l)

let main () =
  let f1 = Future.create read_lines "Pervasives.html" in
  let f1 = Future.then_ (List.filter ((<>) "")) f1 in
  let f2 = Future.create read_lines "List.html" in
  let f2 = Future.then_ (List.filter ((<>) "")) f2 in
(* let fany = Future.when_any [f1;f2] in
  print_list (Future.get fany)
  *)
  let fmerged = Future.when_all [f1;f2]
    |> Future.then_ (List.fold_left (@) [])
    |> Future.then_ (String.concat "\n")
    |> Future.then_ (write_file "merged.html") in
  try Future.get fmerged with e -> print_endline "Exception!"
