
let read_file filename = 
let lines = ref [] in
let chan = open_in filename in
try
  while true; do
    lines := input_line chan :: !lines
  done; []
with End_of_file ->
  close_in chan;
  List.rev !lines ;;


let s = read_file "../paris.txt";;

let (constants,s) = (List.hd s,List.tl s);;

constants;;

let n0 = 11348;;
let m0 = 17958;;
let t0 = 54000;;
let c0 = 8;;
let start = 4516;;

type node = { indice : int; lat : float; lng : float; mutable adj_out : unit -> int list; mutable adj_in : unit -> int list };;

let make_node i (lat,lng) = 
{indice = i ; lat = lat; lng = lng; adj_out = (fun () -> []); adj_in = (fun () -> [])} ;;

let fos = float_of_string;;
let ios = int_of_string;;

let split = Str.split (Str.regexp_string " ");;

let hNodes = Hashtbl.create n0;; 

let get_lat_long s = match split s with
  | [a;b] -> (fos a,fos b)
  | _ -> failwith "wrong format for lat long";;

let read_input_lng_lat() =
  let rec aux = function
    | (-1,l) -> l
    | (k,h::t) -> (Hashtbl.add hNodes (n0- 1 - k) (make_node ((n0- 1 - k)) (get_lat_long h)); aux (k-1,t))
    | _ -> failwith "error in input"
  in aux (n0-1,s)
;;

let s = read_input_lng_lat();;

Hashtbl.find_all hNodes (n0-1);;

type edge = { ind : int; from : int; towards : int; bidir : bool; cost: float; length : float};;

let hEdges = Hashtbl.create m0;;

let make_edge ind i j bidir c l =
  let edge1 = {ind = ind; from = i; towards = j; bidir = bidir; cost = c; length = l} in
  if bidir then
    begin
    let edge2 = { ind = ind; from = j; towards = i; bidir = true; cost = c; length = l} in
    Hashtbl.add hEdges (i,j) edge1; Hashtbl.add hEdges (j,i) edge2
    end
  else
    Hashtbl.add hEdges (i,j) edge1;;

let treat_edge i s = 
  (* print_string ("printing_edge "^(s)^"\n"); *)
  match (split s) with
    | [aj;bj;"1";cj;lj] -> make_edge i (ios aj) (ios bj) false (fos cj) (fos lj)
    | [aj;bj;"2";cj;lj] -> make_edge i (ios aj) (ios bj) true (fos cj) (fos lj)
    | _ -> failwith "wrong format for edge";;


let read_input_edges() = 
  let rec aux = function
    | (-1,_) -> ()
    | (k,h::t) -> treat_edge (m0-k-1) h; aux(k-1,t)
    | _ -> failwith "wrong input format for edges"
  in aux(m0-1,s);;

read_input_edges();;

