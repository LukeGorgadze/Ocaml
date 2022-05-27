type 'a tree = Empty | Node of 'a * ('a tree) * ('a tree)

type 'a ltree = LNode of 'a * (unit -> 'a ltree) * (unit -> 'a ltree)

let layer_tree r = 
    let rec aux n = LNode (n + r,(fun () -> aux (n + 1)),(fun () -> aux (n + 1)))
    in
    aux 0;;

let interval_tree l h = 
    let rec aux ll hh = LNode((ll,hh), (fun () -> aux ll ((ll+.hh) /. 2.)),(fun () -> aux ((ll+.hh)/.2.) hh))
    in
    aux l h;;

let rational_tree n d = 
    let rec aux n0 d0 = LNode((n0,d0), (fun () -> aux n0 (d0 + 1)),(fun () -> aux (n0+1) d))
    in
    aux n d;;

let top n t = 
    let rec aux nn tt = 
    match tt with
    |LNode(h,l,r) when nn = 1 -> Node(h,Empty,Empty)
    |LNode(h,l,r) when nn > 1 -> Node(h,aux (nn-1) (l ()), aux (nn-1) (r ()))
    |_ -> failwith "Error in Top"
    in aux n t

let map f t = 
    let rec aux tt = 
        match tt with
        |LNode(h,l,r) -> LNode(f (h), (fun () -> aux (l ())) , (fun () -> aux (r ())))
    in 
    aux t


let find p tree =
    let rec aux p queue =
    match queue with
    | LNode(x, l, r)::rest ->
        if p x then LNode(x, (fun () -> l ()), (fun () -> r ()))
        else aux p (rest@[(l()); (r())])
    | [] -> failwith "Error in find"

    in
    aux p [tree];;     


(* TESTCASES... YOU DON'T SAY *)
(* let rec ltake n (LNode (h,l,r)) =
 if n <= 0 then [] else h::ltake (n-1) (r ())
let ordTree = layer_tree 5;;
let x = top 5 ordTree;; 
let add1 x = x + 1;;
let modifiedTree = map add1 ordTree;;

let y = top 5 modifiedTree;;
let s = find (fun(x) -> x = 10) ordTree;;
let ss = top 3 s;;  *)

