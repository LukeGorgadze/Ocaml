#directory "+threads";;
#load "unix.cma";;
#load "threads.cma";;



open Thread
open Event

module type Cell = sig
type 'a cell
val new_cell : 'a -> 'a cell
val get : 'a cell -> 'a
val put : 'a cell -> 'a -> unit
end

let get cell = let reply = new_channel ()
    in sync (send cell (Get reply));
        sync (receive reply)
        
let put cell x = sync (send cell (Put x))

let new_cell x = let cell = new_channel ()
    in let rec serve x = match sync (receive cell)
    with Get reply -> sync (send reply x);
        serve x
    | Put y -> serve y
    in let _ = create serve x
    in cell