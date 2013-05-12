(* discrete budgetting problem *)

(* INPUT *)
(* task t = 0, 1, 2, ..., T-1 *)
(* contribution: c : (int -> float) array *)
(* initial budget: b0 *)

(* OUTPUT *)
(* action a: int array *)
(* value_function v_fun: (int -> float) array *)
(* value v: float *)

(* utility *)
let enumerate_by_step lower upper step =
  let lst = ref [] in
  let rec sub lower upper =
    if lower > upper then !lst 
    else if lower = upper then (lower :: !lst) 
    else (lower :: (sub (lower + step) upper)) 
  in sub lower upper

let max_of_fi ~f ~domain =
  let i = ref 0 and v = ref 0.0 in
  (List.iter (fun x -> let fx = (f x) in
               if fx > !v then (i:=x; v:=fx) else ()) domain);
   (!i, !v)

let transition action budget = budget - action;;

(* solve *)
let solve1 b0 c =
  let n = Array.length c in
  let v_fun = Array.create (n+1) (function (x:int) -> 0.0) in (* value function *)
  let counter = ref 0 in
  for i=n-1 downto 0 do
    v_fun.(i) <- (function x -> 
                    let domain = enumerate_by_step 0 x 1 in
                    let f action =
                      (c.(i) action) +. (v_fun.(i+1) (transition action x)) in
                    let (action, value) = max_of_fi ~f ~domain in
                    counter := !counter + 1;
                    Printf.printf "calc %dth v_fun.(%d) %d : %f \n" !counter i x value;
                    value)
  done;
  (v_fun.(0) b0)

(* This doesn't work. *)
let rec v_fun_template_old ~memo ~f ~lower ~step x =
  let v =
    try (Hashtbl.find memo x) 
    with Not_found ->
      let v = f x in 
      if x = lower then (v)
      else 
        let v_prev_step = (v_fun_template_old ~memo ~f ~lower ~step (x-step)) in 
        (max v_prev_step v)
  in ((Hashtbl.add memo x v); v)

(* revised version *)
let rec v_fun_template ~memo ~f ~lower ~step x =
  let v =
    try (Hashtbl.find memo x) 
    with Not_found ->
      let domain = enumerate_by_step lower x step in
      let (action, value) = max_of_fi ~f ~domain in
      (action, value)
  in ((Hashtbl.add memo x v); v)

(* solve *)
let solve2 b0 c =
  let n = Array.length c in
  let v_fun = Array.create (n+1) (function (x:int) -> (0, 0.0)) in (* value function *)
  let counter = ref 0 in
  for i=n-1 downto 0 do
    let memo = Hashtbl.create (b0 + 1) in
    v_fun.(i) <-
      (function x ->
       let f action = 
         let (aa, vv) = ((v_fun.(i+1) (transition action x))) in
         (c.(i) action) +. vv in
       let (action, value) = (v_fun_template ~memo ~f ~lower:0 ~step:1 x) in
       (counter := !counter + 1;
       (* Printf.printf "calc %dth\t v_fun.(%d) %d action:%d value:%f \n" !counter i x action value; *)
       (action, value)))
  done;
  let result = (v_fun.(0) b0) in
  (* for listing up actions *)
  let action_list = ref [] in
  let value_list = ref [] in
  let bd = ref b0 in
  Array.iter (fun f -> let (action, value) = (f !bd) in
                       (bd := (transition action !bd);
                        action_list := action :: !action_list;
                        value_list := value :: !value_list)) v_fun;
  action_list := List.rev !action_list;
  value_list := List.rev !value_list;
  (Printf.printf "action value\n");
  (List.iter2 (fun x y -> Printf.printf "%d\t%1.1f\n" x y) !action_list !value_list ; print_newline ());
  result
 
(* test *)
(* sample input data *)
let num_tasks = 3;;
let contribution1 num_tasks = (* later contributions are less than earlier contributions *) 
  Array.init num_tasks 
    (fun i -> (function action -> (float_of_int action) /. (float_of_int (i+1))));;
let contribution2 num_tasks = (* earlier contributions are less than later contributions *) 
  Array.init num_tasks 
    (fun i -> (function action -> (float_of_int action) /. (float_of_int (num_tasks - i))));;
let contribution3 num_tasks aclion_limit = (* earlier contributions are less than later contributions 
  but each task's contribution is worse when actions are too much *) 
  Array.init num_tasks 
    (fun i -> (function action -> if action > aclion_limit then (neg_infinity)
               else (float_of_int action) /. (float_of_int (num_tasks - i))));;

let b01 = 9;;
let solve = solve1
let test1 = solve b01 (contribution1 num_tasks) = (float_of_int b01)
let test2 = solve b01 (contribution2 num_tasks) = (float_of_int b01)

let solve = solve2
let test1 = solve b01 (contribution1 num_tasks) = (9, float_of_int b01)
let test2 = solve b01 (contribution2 num_tasks) = (0, float_of_int b01)
