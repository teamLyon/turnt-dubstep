open Input;;
open WriteSol;;

let get_node i = Hashtbl.find hNodes i;;
let get_edge i j = try Hashtbl.find hEdges (i,j) with Not_found -> failwith "there is no edge between these nodes!";;
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

let rec cutList res = function 
    | (l,0) -> res
    | (h::t,k) -> cutList (h::res) (t,k-1)
    | ([],k) -> res;;

(* cutList [] ([1;2;3;4],2);; *)

let costEdges edges = List.fold_right (fun (a,b) y -> (get_cost a b) +. y) (edges) 0.;;

let localSearchSmarter i depth tmax = 
  let (res,(score,tLeft,edges)) = localSearch i depth tmax in
  let newDepth = (depth tmax)/3 in
  (* let newPath = cutList [] (List.rev res,newDepth) in *)
  let newEdges = cutList [] (List.rev edges,newDepth-1) in
  let newPath = List.fold_right (fun (a,b) y -> (b::y)) newEdges [List.hd(List.rev(res))] in
  (newPath,(0.0,tmax -. costEdges newEdges,newEdges));;

let (path,(score,tLeft,edges)) = localSearch 4516 (fun x -> 10) 1000.;;
init();;
localSearchSmarter 4516 (fun x -> 10) 1000.;;



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

let cutListAt k l = (* keeps only up to the first k elements of a list *)
  let rec aux res = function
  | (_,0) -> res
  | (h::t,l) -> aux (h::res) (t,l-1)
  | ([],_) -> []
  in List.rev(aux []  (l,k));;

let parcours tmax depth start localSearch  = 
  print_string "End of processing input. Computing routes...\n";
  let res = Array.make_matrix 8 5000 start in
  let rankInRes = Array.make 8 0 in
  let curpos = Array.make 8 start in
  let ended = Array.make 8 false in
  let timeLeft = Array.make 8 tmax in
  let rec treat_car t i car =
    let (stops, (_,tleft,edges)) = localSearch i depth t in
    (* let stops = List.rev (cutListAt ((depth t)/2) (List.rev stops)) in *)
    (* let tleft = snd(List.fold_right (fun x (prevNode,res) -> (x,res +. get_cost prevNode x)) (List.tl stops) (List.hd stops,0.)) in *)
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
let get_previous i h = (* print_string "trying to get previous of "; print_int i;  *)let res = Hashtbl.find h i in (* print_string ("... and it is "^(soi res)); print_newline(); *) res

let rec min_list def f = function
  | [] -> (def, f def)
  | h::t -> if (f h)<(f def) then min_list h f t else min_list def f t;;

let remove x l = 
  let rec aux res = function
  | [] -> res
  | h::t when h=x -> aux res t
  | h::t -> aux (h::res) t in
  List.rev (aux [] l);;

let get_path h (i:int) x =
  let rec aux path edges i x =
    match get_previous x h with
    | u when u=i -> (i::x::path,(i,x)::edges)
    | u -> aux (x::path) ((u,x)::edges) i u
  in aux [] [] i x;;

let sof = string_of_float;;

let dijkstra (i : int) (tmax : float) =
  (* print_string ("starting Dijkstra's algorithm to find an unvisited edge from "^(soi i)^"\n"); *)
  let distances = Hashtbl.create 100 in
  Hashtbl.add distances (i,i) 0.;
  (* List.iter (fun j -> Hashtbl.add distances (i,j) (get_cost i j)) (get_adjo i); *)
  let previous = Hashtbl.create 100 in
  (* List.iter (fun j -> Hashtbl.add previous j i) (get_adjo i); *)
  (* let adj_out = get_adjo i in *)
  let rec aux = function
    | [] -> ([],[])
    | h::t -> let (x,distix) = min_list h (fun j -> get_distance i j distances) t in
	      let newList = remove x (h::t) in
	      let rec aux2 future = function
		| [] -> aux future
		| k::fin -> let alt = distix +. get_cost x k in
			    let cur_dist = (get_distance i k distances) in
			    (* print_string ("current distance from "^(soi i)^" to "^(soi k)^" is "^(sof cur_dist)^" and alt is "^(sof alt)^". tmax is "^(sof tmax)^"\n"); *)
			    if cur_dist > alt && (alt < tmax) then
			      begin
				(* print_string ("the distance from "^(soi i)^" to "^(soi k)^" has  been improved\n"); *)
				Hashtbl.add distances (i,k) alt;
				(* print_string ("the distance from "^(soi i)^" to "^(soi k)^" is now "^(sof (get_distance i k distances))^"\n"); *)
				Hashtbl.add previous k x;
				if (not(traversedEdge x k)) then (* we're done: we have found an unvisited edge *) 
				  get_path previous i k
				else (* we need to keep looking *)
				  aux2 (k::future) fin
			      end
			    else 
			      begin
				(* print_string ("the distance from "^(soi x)^" to "^(soi k)^" has not been improved\n"); *)
				aux2 future fin
			      end
	      in aux2 newList (get_adjo x)
  in let (path,edges) = aux [i]
     in (List.rev path,(0.0,tmax -. (List.fold_right (fun (a,b) y -> get_cost a b +. y) edges 0.),edges));;

let dijkstra i depth tmax =
  dijkstra i tmax;;


(* dijkstra 4516 (-1) 200.0;; *)


		
let main() =
  let nargs = Array.length Sys.argv in
  let suffix = ref "" in
  let def_depth = 3 in
  init();
  let res =
    if nargs <> 4 then
      begin
	print_string "not in terminal mode";
	suffix := string_of_int def_depth;
	parcours (float_of_int t0)
	  (fun x -> def_depth) start localSearch
      end
    else
      begin
	print_string "terminal mode";
	let depth = ios Sys.argv.(1) in
	suffix := string_of_int depth;
	max_rep := ios Sys.argv.(2);
	let dijk = bool_of_string (Sys.argv.(3)) in
	let localSearch start depth time = if time<3000. then dijkstra start depth time else localSearchSmarter start depth time in
(* if dijk then dijkstra else localSearch in *)
	let depthFun t = (* if t> 40000. then 10 else *) depth in
	parcours (float_of_int t0) depthFun  start localSearch
      end
  in
  print_float (eval_sol()); print_newline();
  writeSolToFile(Array.to_list res) ("output"^(!suffix));
  res
;;

let res = main();;
