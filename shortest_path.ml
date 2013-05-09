(* shortest path problem *)

(* sample input data *)
let links = [|[(1,8);(3,15)];[(2,14);(3,3)];[(4,10)];[(2,5);(4,17)]|];;

(* solve *)

(* list up next update values *) 
let get_candidate_values v next_nodes =
  List.map (function (j,w) -> w + v.(j)) next_nodes;; 
(* test *)
let test1 = 
  let v = Array.create 5 0 in 
    get_candidate_values v links.(0) = [8;15]   
let test2 = 
  let v = Array.create 5 10 in 
    get_candidate_values v links.(0) = [18;25]
let test3 =
  let v = Array.create 5 10 in 
    get_candidate_values v links.(3) = [15;27]

(* update *)
let update num_of_nodes v links is_not_updated path = 
  for i=0 to num_of_nodes - 1 do
    (* let candidates = get_candidate_values v links.(i) in 
    let min_of_candidates = List.fold_left (fun x y -> min x y) max_int candidates in *)
    let min_of_candidates = List.fold_left (fun x link -> let w = (v.(fst link) + snd link) in
        if w < x then (path.(i) <- fst link; w) else x) max_int links.(i) in
    (is_not_updated.(i) <- if min_of_candidates < v.(i) 
                             then (v.(i) <- min_of_candidates; false)
                           else true);
    Printf.printf "%d:%d\t" i v.(i); 
    ()
  done;
  Printf.printf "\n";
  ();;

(* backward *)
let update num_of_nodes v links is_not_updated path = 
  for i=num_of_nodes-1 downto 0 do
    (* let candidates = get_candidate_values v links.(i) in 
    let min_of_candidates = List.fold_left (fun x y -> min x y) max_int candidates in *)
    let min_of_candidates = List.fold_left (fun x link -> let w = (v.(fst link) + snd link) in
        if w < x then (path.(i) <- fst link; w) else x) max_int links.(i) in
    (is_not_updated.(i) <- if min_of_candidates < v.(i) 
                             then (v.(i) <- min_of_candidates; false)
                           else true);
    ()
  done;
  ();;

(* get path *)
let get_shortest_path links v =
  Array.map

let solve links = 
  let big_m = 10000 in 
  let num_of_nodes = (Array.length links) in
  let path = Array.create num_of_nodes (~-1) in
  let values = Array.create (num_of_nodes + 1) big_m in 
  values.(num_of_nodes) <- 0; 
  let is_not_updated_flags = Array.create num_of_nodes false in
  while not (Array.for_all (fun x -> x) is_not_updated_flags) do
    update num_of_nodes values links is_not_updated_flags path;
    Array.iteri (fun i x -> Printf.printf "%d:%d\t" i x) values;
    Printf.printf "\n";
  done;
  Array.iteri (fun i x -> Printf.printf "%d -> %d " i x) path; print_newline ();
  (Array.fold_left (fun x y -> x + y) 0 values);;



   
