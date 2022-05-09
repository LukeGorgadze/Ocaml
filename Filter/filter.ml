let rec filter p= function
    |[] -> []
    |h::t -> if p h then h :: filter p t else filter p t

let even x = x mod 2 = 0
let odd x = x mod 2 = 1

let evens lst = filter even lst
let odds lst = filter odd lst

(* TAIL RECURSIVE VERSION OF FILTER *)
let rec filter_aux p acc = function 
    |[] -> acc
    |h::t -> filter_aux p t (if p h then h::acc else acc)


let rec filter p lst = filter_aux p [] lst