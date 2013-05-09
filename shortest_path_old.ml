(* shortest path problem *)
(* node array *)
(* node indices: 0,1,2,3,4 *)
let get_forward_hash link = 
  let h = Hashtbl.create (Array.length link) in
  (Array.iter (fun x -> match x with (a,b) -> Hashtbl.add h a b) link; h);;

let get_backward_hash link = 
  let h = Hashtbl.create (Array.length link) in
  (Array.iter (fun x -> match x with (a,b) -> Hashtbl.add h b a) link;
  h);;

let get_cost_hash costs = 
  let h = Hashtbl.create (Array.length costs) in
  Array.iter (fun x -> match x with (a,b,c) -> Hashtbl.add h (a,b) c) costs;
  h;;

(* sample data *)
let nodes = [|0;1;2;3;4|];;
let forward_links = [|(0,1);(0,3);(1,2);(1,3);(2,4);(3,2);(3,4)|];;
let costs = [|(0,1,8);(0,3,15);(1,2,14);(1,3,3);(2,4,10);(3,2,5);(3,4,17)|];;

(* naive solution *)
let solve nodes links costs start_node end_node =
  let forward_link_hash = get_forward_hash links in
  let cost_hash = get_cost_hash costs in
  let n = Array.length nodes in 
  let v = Array.create (Array.length nodes) 100 (*Big-M is set to 100*) in
  v.(n-1) <- 0;
  let show v = 
    Array.iteri (fun i x -> Printf.printf "%d:%d " i x) v;
    Printf.printf "\n" in
  let update nodes cost_hash forward_link_hash v is_not_updated = 
    for i=0 to (n-2) do (* v.(i) <- min (v.(i), min_j (costs.(i,j) + v.(j)) *)
      let next_nodes = (Hashtbl.find_all forward_link_hash nodes.(i)) in 
      let candidate_v =
        List.fold_left (fun x y -> min x (v.(y) + Hashtbl.find cost_hash (nodes.(i), y))) 
        max_int next_nodes in
      is_not_updated.(i) <- candidate_v = v.(nodes.(i));
      if v.(nodes.(i)) > candidate_v then v.(nodes.(i)) <- candidate_v else ();
    done;
  in
  let is_not_updated = Array.create n true in
  show v;
  update nodes cost_hash forward_link_hash v is_not_updated;
  while not (Array.for_all (fun x -> x) is_not_updated) do
    show v;
    update nodes cost_hash forward_link_hash v is_not_updated;
  done;
  show v;
  Printf.printf "cost:%d\n" v.(nodes.(0));
  []
     
(* test *)
let test1 = solve nodes forward_links costs 0 4 = [0;1;3;2;4]

