open Input;;
open WriteSol;;

let get_node i = Hashtbl.find hNodes i;;
let get_edge i j = Hashtbl.find hEdges (i,j);;
let get_cost i j = let e = get_edge i j in e.cost;;
let get_length i j = let e = get_edge i j in e.length;;
let get_adjo i = let ni = get_node i in ni.adj_out();;
let get_adji i = let ni = get_node i in ni.adj_in();;



let add_neighbour i j = let ni = get_node i in
			let nj = get_node j in
			let adj_out_i = ni.adj_out () in
			let adj_in_j = nj.adj_in () in
			ni.adj_out <- (fun () -> (j::adj_out_i));
			nj.adj_in <- (fun () -> (i::adj_in_j));;

let assign_neighbours () =
  Hashtbl.iter  (fun (i,j) _ -> add_neighbour i j) hEdges;;

assign_neighbours();;

(* get_cost 2100 4641;; *)
(* get_length 2100 4641;; *)
(* let ni = get_node 2100;; *)
(* let adjo = get_adjo 2100;; *)

let visited = Array.make m0 false;;
let init() = 
  for i = 0 to (m0-1) do
    visited.(i) <- false;
  done;;

let traversed i = visited.(i);;
let traversedEdge i j = let e = get_edge i j in traversed (e.ind);;
let mark_visited_simple e =  visited.(e.ind) <- true;; 
let mark_visited i j = let e = get_edge i j in mark_visited_simple e;  if e.bidir then mark_visited_simple (get_edge j i);;

let trivial_solution() = 
  let rec aux = function
    | 0 -> []
    | k -> [start]::(aux (k-1))
  in aux 8;;

writeSolToFile (trivial_solution()) "output";;

let rec max_list bigger default = function
  | [] -> default
  | h::t -> if bigger h default then max_list bigger h t else max_list bigger default t;;

let get_cost2 i j = 
  let res = get_cost i j in print_string ("getting cost for: "^(soi i)^" "^(soi j)^" "^(string_of_float res)^"\n"); res
  ;;

let rec print_list = function
  | [] -> print_string "end of printing list \n"
  | h::t -> print_int h; print_newline (); print_list t;;

let get_lengthDiff edges i j =
  if ((traversedEdge i j) || List.mem (i,j) edges || List.mem (j,i) edges) then 0. else get_length i j;;

let rec strip i adj_out t = 
  let rec aux = function
    | [] -> []
    | a::b -> if get_cost i a > t then aux b else a::(aux b)
  in aux adj_out
;;

let localSearch i depth tmax =
  let rec aux i cost score best_path edges t = function
    | 0 -> (i::best_path,(score /. cost,t,edges))
    | k ->   let ni = get_node i in
	     let adj_out = ni.adj_out() in
	     let bigger (x,(y,u,edges)) (z,(t,w,edges1)) = (y>t) in
	     max_list bigger ([],(~-. 0.1,0.0,[])) (List.map (max_list bigger ([],(~-. 0.1,0.0,[]))) (List.map 
			      (fun j -> let c = get_cost i j and l = get_lengthDiff edges i j in [aux j (cost +. c) (score +. l) (i::best_path) ((i,j)::edges) (t -. c) (k-1)]) (strip i adj_out t)))
  in (aux i 0.0 0.0 [] [] tmax depth)
;;



let rec replicate l = function
  | 1 -> [l]
  | k -> l::(replicate l (k-1));;


(* writeSolToFile (replicate (fst(localSearch 4516 20)) 8) "output";; *)

let next_car car = (car + 1) mod 8;;

let parcours tmax depth start = 
  let res = Array.make 8 [] in
  let curpos = Array.make 8 start in
  let ended = Array.make 8 false in
  let timeLeft = Array.make 8 tmax in
  let rec treat_car t i car =
    (* print_string ("treating car "^(soi car)^" with remaining time "^(string_of_float t)^"\n"); *)
    let (stops, (_,tleft,edges)) = localSearch i depth t in
    let rec aux2 accu = function
      | h::t -> t@accu
    (* | [h] -> accu *)
    (* | h::t -> aux2 (h::accu) t *)
    (* | _ -> failwith "depth>=2 should avoid this" *)
    in match stops with
      | [] -> ended.(car) <- true
      | intersection::sequel -> 
	begin
	  List.iter (fun (x,y) -> mark_visited x y) edges;
	  res.(car) <- aux2 (res.(car)) stops; 
	  curpos.(car) <- intersection; 
	  timeLeft.(car) <- tleft; 
	  let ncar = (next_car car) in treat_car (timeLeft.(ncar)) (curpos.(ncar)) ncar
	end
  in
  treat_car tmax start 0;
    Array.iteri (fun i x -> res.(i) <- List.rev(res.(i))) res; res
;;


(* let res = parcours 100.0 4 start;; *)
let res = parcours (float_of_int t0) 10 start;;

(* init();; *)
(* localSearch start 4 100.0;;	    *)
(* localSearch 397 4 100.0;; *)


writeSolToFile(Array.to_list res) "output";;

(* List.length (res.(0));; *)
(* List.nth  res.(0) 2051;; *)
(* List.nth  res.(0) 2050;; *)
(* List.nth  res.(0) 2049;; *)
(* List.nth  res.(0) 2048;; *)