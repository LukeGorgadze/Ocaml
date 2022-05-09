let rec filter_aux p acc = function 
    |[] -> acc
    |h::t -> filter_aux p t (if p h then h::acc else acc)
let rec filter p lst = filter_aux p [] lst