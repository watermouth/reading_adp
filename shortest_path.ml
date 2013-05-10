(* shortest path problem *)
let max_int = max_int - 1000000;;

(* sample input data *)
let links = [|[(1,8);(3,15)];[(2,14);(3,3)];[(4,10)];[(2,5);(4,17)]|];;
let make_links num = 
  Array.init num (fun i -> (List.filter_map
                           (fun x -> if x < i || x > (i+1) then None else Some (x+1,10 * x * num / (i+1)))
                           (List.init num (fun j -> j))));;
let links_large = make_links 5000;;

(* update *)
let update1 num_of_nodes v links is_not_updated path = 
  for i=0 to num_of_nodes - 1 do
    let min_of_candidates = List.fold_left (fun x link -> let w = (v.(fst link) + snd link) in
        if w < x then (path.(i) <- fst link; w) else x) max_int links.(i) in
    (is_not_updated.(i) <- if min_of_candidates < v.(i) 
                             then (v.(i) <- min_of_candidates; false)
                           else true);
    (* Printf.printf "%d:%d\t" i v.(i);  *)
    ()
  done;
  (* Printf.printf "\n"; *)
  ();;

(* backward *)
let update2 num_of_nodes v links is_not_updated path = 
  for i=num_of_nodes-1 downto 0 do
    let min_of_candidates = 
      List.fold_left (fun x link -> 
        let w = (v.(fst link) + snd link) in
        if w < x then (path.(i) <- fst link; w) else x) max_int links.(i) in 
        (is_not_updated.(i) <- if min_of_candidates < v.(i) 
                               then (v.(i) <- min_of_candidates; false) else true);
    ()
  done;
  ();;

let solve_12 update links = 
  let big_m = max_int in 
  let num_of_nodes = (Array.length links) in
  let path = Array.create num_of_nodes (~-1) in
  let values = Array.create (num_of_nodes + 1) big_m in 
  values.(num_of_nodes) <- 0; 
  let is_not_updated_flags = Array.create num_of_nodes false in
  is_not_updated_flags.(num_of_nodes-1) <- true; (* for revised version *)
  while not (Array.for_all (fun x -> x) is_not_updated_flags) do
    update num_of_nodes values links is_not_updated_flags path;
    (* Array.iteri (fun i x -> Printf.printf "%d:%d\t" i x) values; 
    Printf.printf "\n"; *)
  done;
  (* Array.iteri (fun i x -> Printf.printf "%d -> %d " i x) path; print_newline (); *)
  let idx = ref 0 in
  while !idx < num_of_nodes do
    Printf.printf "-> %d " path.(!idx);
    idx := path.(!idx)
  done;
  Printf.printf "\ncost=%d" values.(0);();;
  (* (Array.fold_left (fun x y -> x + y) 0 values);; *)


(* ------------------- revised ? ------------------- *)
let update3 v backward_links path triggers = 
  let j = Array.findi (fun x -> x) triggers in
  (* search among nodes which can reach j by one step *)
  let i_lst = Hashtbl.find_all backward_links j in 
  (* v.(i) candidate *)
  List.iter 
    (fun link -> match link with | (i, cost) ->
       let v_hat = v.(j) + cost in
       if v_hat < v.(i) then 
         (path.(i) <- j; v.(i) <- v_hat;
          if triggers.(i) = false then triggers.(i) <- true else ())
       else ();
     ) i_lst;
  triggers.(j) <- false 

let solve_3 update links = 
  let big_m = max_int in 
  let num_of_nodes = (Array.length links) in
  let path = Array.create num_of_nodes (~-1) in
  let values = Array.create (num_of_nodes + 1) big_m in 
  values.(num_of_nodes) <- 0; 
  let triggers = Array.create (num_of_nodes + 1) false in
  triggers.(num_of_nodes) <- true;
  let backward_link_hash =
    let h = Hashtbl.create (num_of_nodes * num_of_nodes) in
    Array.iteri (fun i lst_links -> 
                 List.iter (fun (j, cost) -> Hashtbl.add h j (i,cost)) lst_links)
                links;
    h in
  while not (Array.for_all (fun x -> not x) triggers) do
    update values backward_link_hash path triggers ;
    (* Array.iteri (fun i x -> Printf.printf "%d:%d\t" i x) values;
    Printf.printf "\n"; *)
  done;
  (* Array.iteri (fun i x -> Printf.printf "%d -> %d " i x) path; print_newline (); *)
  let idx = ref 0 in
  while !idx < num_of_nodes do
    Printf.printf "-> %d " path.(!idx); 
    idx := path.(!idx)
  done;
  Printf.printf "\ncost=%d" values.(0);();;

