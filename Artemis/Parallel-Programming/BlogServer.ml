#directory "+threads";;
#load "unix.cma";;
#load "threads.cma";;



open Thread
open Event

type blog = string list
type user = string
type pass = string
type message = Post of user * pass * string
  | Read of user * blog channel
type t = message channel


(* begin solution *)
let start_server users =
  let c = new_channel () in
    let rec server_fun blogs =
      let get_blog user = match List.assoc_opt user blogs with
        None -> [] | Some b' -> b' in
      match sync (receive c) with
      | Post (user, pass, text) ->
        if List.assoc_opt user users = Some pass then
          server_fun ((user, get_blog user @ [text])  
            ::List.remove_assoc user blogs) 
        else server_fun blogs
      | Read (user, answer_c) ->
        sync (send answer_c (get_blog user));
        server_fun blogs
    in
    let _ = create server_fun [] in
    c
    
    
let post s u p t =
  sync (send s (Post (u, p, t))) 

let read s u =
  let answer_c = new_channel () in
  sync (send s (Read (u, answer_c)));
  sync (receive answer_c)

(* end solution *)
let test =
  let s = start_server [("userA", "passA"); ("userB", "passB")] in
  post s "userB" "passB" "Welcome to my OCaml blog.";
  post s "userA" "passA" "My name is A and I'm starting my own blog!";
  post s "userB" "12345" "I am a hacker attacking B's blog now!";
  post s "userB" "passB" "You can have threads in OCaml!";
  read s "userB"

(* alternative for start_server *)
let (|?) a b = match a with Some x -> x | None -> b

let rec start_server up =
  let open List in
  let c = new_channel () in
  let rec f bs =
    match sync (receive c) with
    | Post (u, p, b) ->
        if assoc_opt u up = Some p then
          f ((u, (assoc_opt u bs |? []) @ [b])::remove_assoc u bs)
        else
          f bs
    | Read (u, rc) ->
        sync (send rc (assoc_opt u bs |? []));
        f bs
  in
  ignore (create f []);
  c

