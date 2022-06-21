#directory "+threads";;
#load "unix.cma";;
#load "threads.cma";;

(* open Thread *)
(* open Event *)

module Echo = struct open Thread
    let echo () = print_string (
                read_line () ^
                "\n")
    let main = let t1 = create echo ()
        in join t1;
            print_int (id (self ()));
            print_string "\n"
end