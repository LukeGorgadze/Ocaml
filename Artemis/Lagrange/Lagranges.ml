let lagrange lss v = 
  List.fold_left (fun o p -> o +. p) 0.0 ( List.map 
  (fun (a,b) -> 
  b*.(List.fold_left ( *. ) 1.0 
  (List.map (fun (x,y) -> 
  if (x != a) then (v -. x)/.(a -. x) 
  else 1.0) lss))) lss);;

lagrange [100.,231.; 200.,12.; 300.,382.5] 0.3;;

let lagrangeR lst vall = 
  List.fold_right (+.) (List.map(fun(x,y) ->
  y *.(List.fold_right ( *. ) 
  (List.map (fun (j,k) -> if (j != x) then (vall -.j) /. (x -. j) else 1.0) lst) 1.0)) lst) 0.0;;

lagrangeR [100.,231.; 200.,12.; 300.,382.5] 0.3;;