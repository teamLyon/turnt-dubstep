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

let get_lengthDiff edges i j = (* get the length of the path, counting only edges that have not been visited before or in the list of edges given as input*)
  if ((traversedEdge i j) || List.mem (i,j) edges || List.mem (j,i) edges) then 0. else get_length i j;;

let countlist i j l = (* count the number of times the edge i j has been used in the current exploration  *)
  let rec aux res = function
    | [] -> res
    | (i1,j1)::t -> let temp = if ((i1=i && j1=j) || (i1=j && j1=i)) then 1 else 0 in aux (res+temp) t
  in let res = aux 0 l in
     (* if res>= 2 then print_string "coucou!\n"; *) res
;;

let max_rep = ref 1 (* if t > 40000. then  1 else if t > 10000. then 2 else 3;; *) (* maximum number of repetition of edges allowed in an exploration*)
let max_repFun t = !max_rep;;

let rec strip i adj_out t edges = 
  let rec aux res = function
    | [] -> res
    | j::b -> if (get_cost i j >  t || (countlist i j edges >= max_repFun t)) then aux res b else (aux (j::res) b)
  in aux [] adj_out
;;

let localSearch i depth tmax =
  let rec aux i cost score best_path edges t = function
    | 0 -> (i::best_path,((score *. score) /. cost,t,edges))
    | k ->   let ni = get_node i in
	     let adj_out = ni.adj_out() in
	     let bigger (x,(y,u,edges)) (z,(t,w,edges1)) = (y>t) in
	     max_list bigger ([],(~-. 1.0,0.0,[])) (List.map (max_list bigger ([],(~-. 1.0,0.0,[]))) (List.map (* ~-. 1.0 is to avoid the empty solution being chosen instead of a zero score solution *) 
			      (fun j -> let c = get_cost i j and l = get_lengthDiff edges i j in [aux j (cost +. c) (score +. l) (i::best_path) ((i,j)::edges) (t -. c) (k-1)]) (strip i adj_out t edges)))
  in (aux i 0.0 0.0 [] [] tmax (depth tmax))
;;



let rec replicate l = function
  | 1 -> [l]
  | k -> l::(replicate l (k-1));;


(* writeSolToFile (replicate (fst(localSearch 4516 20)) 8) "output";; *)

let next_car car = (car + 1) mod 8;;

let reverse_array t = 
  let n = Array.length t in
  let res = Array.make n (t.(0)) in
  for i=0 to (n-1) do
    res.(i) <- t.(n-i-1)
  done; res;;

let parcours tmax depth start = 
  print_string "End of processing input. Computing routes...\n";
  let res = Array.make_matrix 8 5000 start in
  let rankInRes = Array.make 8 0 in
  let curpos = Array.make 8 start in
  let ended = Array.make 8 false in
  let timeLeft = Array.make 8 tmax in
  let rec treat_car t i car =
    let (stops, (_,tleft,edges)) = localSearch i depth t in
    let rec aux3  = function
      | [] -> ()
      | h::t -> res.(car).(rankInRes.(car)) <- h; rankInRes.(car) <- rankInRes.(car) + 1; aux3 t in
    let aux2 = function
      | h::t -> aux3 (List.rev t) (* forget the head because it is already counted *)
      | _ -> failwith "depth>=2 should avoid this"
    in match stops with
      | [] -> (* res.(car) <- curpos.(car)::(res.(car));  *)ended.(car) <- true
      | intersection::sequel -> 
	begin
	  List.iter (fun (x,y) -> mark_visited x y) edges;
	  aux2 stops; 
	  curpos.(car) <- intersection; 
	  timeLeft.(car) <- tleft; 
	  let ncar = (next_car car) in treat_car (timeLeft.(ncar)) (curpos.(ncar)) ncar
	end
  in
  treat_car tmax start 0;
  let res1 = Array.make 8 [] in
  for i=0 to 7 do
    let temp = ref [] in
    for j=0 to (rankInRes.(i)-1) do
      temp := (res.(i).(j)::!temp)
    done;
    res1.(i) <- List.rev(!temp);
  done;
res1
(* Array.iteri (fun i x -> res.(i) <- reverse_array res.(i)) res; res *)
;;

(* init();; *)
(* let res = parcours 10.0 2 start;; *)

(* init();; *)
(* localSearch start 4 100.0;;	    *)
(* localSearch 397 4 100.0;; *)

let eval_sol () =
  Hashtbl.fold (fun (i,j) e res -> if (visited.(e.ind) && ((not(e.bidir)) || i < j)) then res +. e.length else res) hEdges 0.0;;


(* let elapsed_time () = *)
(*   Hashtbl.fold (fun (i,j) e res -> if (visited.(e.ind) && ((not(e.bidir)) || i < j)) then res +. get_cost i j else res) hEdges 0.0;; *)

let print_edge i j = print_int i; print_string " -> "; print_int j; print_newline();;

let show_visited_edges () = Hashtbl.iter (fun (i,j) e -> if visited.(e.ind) then print_edge i j) hEdges;;
(* show_visited_edges();; *)

(* print_float (eval_sol()); print_newline();; *)


(* List.length (res.(0));; *)
(* List.nth  res.(0) 2051;; *)
(* List.nth  res.(0) 2050;; *)
(* List.nth  res.(0) 2049;; *)
(* List.nth  res.(0) 2048;; *)

(* Dijkstra to closest not visited edge *)
let get_distance i j h = try Hashtbl.find h (i,j) with | Not_found -> infinity;;
let get_previous i h = print_string "trying to get previous of "; print_int i; print_newline(); Hashtbl.find h i

let rec min_list def f = function
  | [] -> (def, f def)
  | h::t -> if (f h)<(f def) then min_list h f t else min_list def f t;;

let remove x l = 
  let rec aux res = function
  | [] -> res
  | h::t when h=x -> aux res t
  | h::t -> aux (h::res) t in
  List.rev (aux [] l);;

let get_path h i x =
  let rec aux path i x =
    match get_previous x h with
    | u when u=i -> i::path
    | u -> aux (x::path) i u
  in aux [] i x;;

let dijkstra i tmax = 
  let distances = Hashtbl.create 100 in
  List.iter (fun j -> Hashtbl.add distances (i,j) (get_cost i j)) (get_adjo i);
  let previous = Hashtbl.create 100 in
  List.iter (fun j -> Hashtbl.add previous j i) (get_adjo i);
  let adj_out = get_adjo i in
  let rec aux = function
    | [] -> failwith "in an impasse..."
    | h::t -> let (x,distix) = min_list h (fun j -> get_distance i j distances) t in
	      let newList = remove x (h::t) in
	      let rec aux2 = function
		| [] -> aux newList
		| k::fin -> 
		  let alt = distix +. get_cost x k in 
		  print_string "alt: "; print_float alt; print_newline();
		  if (get_distance i k distances) > alt then 
		    begin
		      Hashtbl.add distances (i,k) alt; 
		      print_string "coucou";
		      Hashtbl.add previous k x
		    end;
		      if (not(traversedEdge x k)) then
			get_path previous i k
		      else
		    aux2 fin
	      in aux2 (get_adjo x)
  in aux adj_out;;

(* dijkstra 4516 10.0;; *)

		
(* let main() = *)
(*   let nargs = Array.length Sys.argv in *)
(*   let suffix = ref "" in *)
(*   let def_depth = 3 in *)
(*   init(); *)
(*   let res =  *)
(*     if nargs <> 3 then *)
(*       begin *)
(* 	suffix := string_of_int def_depth; *)
(* 	parcours 100.0(\* (float_of_int t0) *\) *)
(* 	  (fun x -> def_depth) start *)
(*       end *)
(*     else *)
(*       begin *)
(* 	let depth = ios Sys.argv.(1) in *)
(* 	max_rep := ios Sys.argv.(2); *)
(* 	let depthFun t = (\* if t> 40000. then 10 else *\) depth in *)
(* 	parcours (float_of_int t0) depthFun start *)
(*       end *)
(*   in *)
(*   print_float (eval_sol()); print_newline(); *)
(*   writeSolToFile(Array.to_list res) ("output"^(!suffix)); *)
(*   res *)
(* ;; *)

(* let res = main();; *)
