let rec add1 = function
    |[] -> []
    |h::t -> (h+1)::add1 t

let rec concat3110 = function
    |[] -> []
    |h :: t -> (h^"3110") :: concat3110 t

let rec map f = function
    |[] -> []
    |h::t -> f h :: map f t

let add1' lst = map (fun x -> x + 1) lst
let concat3110' lst = map (fun x -> x ^ "3110") lst

