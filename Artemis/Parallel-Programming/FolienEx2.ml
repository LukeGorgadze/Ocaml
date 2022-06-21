#directory "+threads";;
#load "unix.cma";;
#load "threads.cma";;

module Exchange = struct open Thread open Event
let thread ch = let x = sync (receive ch) in
    print_string (x ^ "\n");
    sync (send ch "got it!")
let zuka = let ch = new_channel () in
    let _ = create thread ch in
    print_string "main is running ...\n";
    sync (send ch "Greetings!");
    print_string ("He " ^ sync (receive ch) ^ "\n")
end