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
  let v = ref 0.0 in (* value *)
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

(* revised ?*)
let rec v_fun_template ~memo ~f ~lower ~step x =
  let v =
    try (Hashtbl.find memo x) 
    with Not_found ->
(* This doesn't work. *)
      let v = f x in 
      if x = lower then (v)
      else 
        let v_prev_step = (v_fun_template ~memo ~f ~lower ~step (x-step)) in 
        (max v_prev_step v)
  in ((Hashtbl.add memo x v); v)

(* solve *)
let solve2 b0 c =
  let n = Array.length c in
  let v_fun = Array.create (n+1) (function (x:int) -> 0.0) in (* value function *)
  let v = ref 0.0 in (* value *)
  let counter = ref 0 in
  for i=n-1 downto 0 do
    let memo = Hashtbl.create (b0 + 1) in
    v_fun.(i) <-
      (function x ->
       let f action = (c.(i) action) +. (v_fun.(i+1) (transition action x)) in
       let value = (v_fun_template ~memo ~f ~lower:0 ~step:1 x) in
       (counter := !counter + 1;
       Printf.printf "calc %dth v_fun.(%d) %d : %f \n" !counter i x value;
       value))
  done;
  (v_fun.(0) b0)
 
(* test *)
(* sample input data *)
let num_tasks = 3;;
let contribution1 num_tasks = (* later contributions are less than earlier contributions *) 
  Array.init num_tasks 
    (fun i -> (function action -> (float_of_int action) /. (float_of_int (i+1))));;
let contribution2 num_tasks = (* earlier contributions are less than later contributions *) 
  Array.init num_tasks 
    (fun i -> (function action -> (float_of_int action) /. (float_of_int (num_tasks - i))));;

let b01 = 9;;
let solve = solve1
let test1 = solve b01 (contribution1 num_tasks) = (float_of_int b01)
let test2 = solve b01 (contribution2 num_tasks) = (float_of_int b01)

let solve = solve2
let test1 = solve b01 (contribution1 num_tasks) = (float_of_int b01)
let test2 = solve b01 (contribution2 num_tasks) = (float_of_int b01)
