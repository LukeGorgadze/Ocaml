(* let x = (fun x -> x 3) (fun y z -> z y) (fun w -> w+w);; *)
(* let rec impl = fun n a ->
  match n with 0 -> a | _ -> impl (n-1) (a * n * n)
and bar = fun n -> impl n 1 *)

 (* let rec fact n = match n with 0 -> 1
  | n -> n * fact (n-1)

let rec fact_aux x n = match n with 0 -> x
  | n -> fact_aux (n*x) (n-1)

let fact_iter = fact_aux 1 *)

let rec fl f a l = match l with [] -> a
| x::xs -> fl f (f a x) xs

let rec fr f l a = match l with [] -> a
| x::xs -> f x (fr f xs a)

let rec rev_map f l a = match l with [] -> a
| x::xs -> rev_map f xs (f x :: a)

let (+) a b = a + b

let l = [1;2;3];;

