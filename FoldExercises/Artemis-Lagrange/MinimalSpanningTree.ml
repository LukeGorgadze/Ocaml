
(* SOLVEDD USING PRIM'S ALGORITHM *)
type graph = (int * float * int) list

let divide cur visited ls = List.partition (fun (a,_,b) -> (b = cur) || (a = cur)) ls

let compareW (a1,w1,b1) (a2,w2,b2) = compare w1 w2

let minWeight ls = List.fold_left (fun y x -> if compareW x y < 0 then x else y) (List.hd ls) (List.tl ls)

let rec getAllNodes nodeLs = function
    |[] -> nodeLs
    |(a,w,b)::t -> if ((List.mem a nodeLs) && (List.mem b nodeLs)) then (getAllNodes (nodeLs) t) 
                    else if ((not (List.mem a nodeLs)) && (List.mem b nodeLs)) then (getAllNodes (a::nodeLs) t)
                    else if ((not (List.mem b nodeLs)) && (List.mem a nodeLs)) then (getAllNodes (b::nodeLs) t)
                    else (getAllNodes (a::b::nodeLs) t)

let mst lst = 
  let allNodes = List.sort compare (getAllNodes [] lst) in
  let rec aux acc visited edges stack tailNodes  = 
  if(tailNodes = []) then acc
  else 
  (match visited with
      |[] -> raise (Failure "Error")
      |x::xs  ->
      let connectedEdges,restEdges = divide x visited edges in
        let newPq = List.append stack connectedEdges in
        let bestEdge = minWeight newPq in
        let nicePQ = List.filter(fun a -> a != bestEdge) newPq in
        match bestEdge with (a,c,b) ->
        if(List.mem a visited && List.mem b visited) then aux acc visited edges nicePQ tailNodes 
        else 
        let newNode = if (List.mem a visited) then b
        else a in
        let ntailNodes = List.filter(fun x -> x != newNode) tailNodes in
        aux (bestEdge::acc) (newNode::visited) restEdges nicePQ ntailNodes
      )
    in aux [] [List.hd allNodes] lst [] (List.tl allNodes)
     ;;
(* mst [0,1.,1; 0,4.,2; 1,2.,2; 1,1.,3; 2,3.,3];;
mst [0,4.,1; 0,4.,7; 1,8.,2; 1,11.,7; 2,7.,3; 2,4.,5; 2,2.,8; 3,9.,4; 3,14.,5; 4,10.,5; 5,2.,6; 6,1.,7; 6,6.,8; 7,7.,8;];; *)
