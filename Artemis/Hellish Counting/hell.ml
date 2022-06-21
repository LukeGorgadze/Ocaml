#thread;;
#directory "+threads";;
#load "unix.cma";;
#load "threads.cma";;

open Thread
open Event

let rec count n k = 
    print_string "Thread ID: ";print_int (Thread.id (Thread.self ()));
    print_string " Value: ";print_int k;print_string "\n";
    Thread.yield();
    match k with
    k when n == k -> ()
    |k -> count n (k+1)

let spawn_counter n = Thread.create (count n ) 0;;

Thread.join (spawn_counter 10);;

let rec spawn_counters m n = 
    if m = 0 then []
    else (spawn_counter n):: spawn_counters (m-1) n;;

let rec joinThreads t1 =
    match t1 with [] -> ()
    |th::t -> Thread.join th; joinThreads t;;

let run_counters m n = 
    let t = spawn_counters m n in
    joinThreads t;;

(* NEW IMPLEMENTATION *)
let rec count (n,k,ch) =
    let _ = sync(receive ch) in
    print_string "Thread ID: ";print_int (Thread.id (Thread.self ()));
    print_string " Value: ";print_int k;print_string "\n";
    sync (send ch "got it");
    match k with
    k when n == k -> ()
    |k -> count (n,(k+1),ch)

let spawn_counter n =  
    let tpl = (n,0,Event.new_channel ()) in
    let _ = Thread.create count tpl in
    match n with
    n -> tpl

let sendData ls = 
    let rec aux l = match l with
    [] -> ()
    |(n,k,ch)::t -> 
            sync(send ch "a");
            aux t
    in aux ls;;
    
let receiveData ls = 
    let rec aux l = match l with
    [] -> ()
    |(n,k,ch)::t -> 
            sync(receive ch);
            aux t
    in aux ls;;

let rec sendAndReceive ls n =
    match n with
    0 -> ()
    |n ->   sendData ls;
            receiveData ls;
            sendAndReceive ls (n-1);;

let rec crMchannels mm n acc = match mm with
    0 -> acc
    |m -> crMchannels (mm-1) n ((spawn_counter n)::acc)

(* let l = crMchannels 2 3 [] *)

let run_counters m n = 
    let allChannels = crMchannels m n [] in
    sendAndReceive allChannels n;
