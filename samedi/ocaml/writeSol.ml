open Input;;
open Printf;;

let soi = string_of_int;;

let writeSol l = 
  let rec unroll s = function
    | [] -> s
    | [h] -> (s^(soi h))
    | h::t -> (unroll (s^((soi h)^"\n")) t) in
  let rec aux s = function
    | [] -> s
    | l1::t -> 
      let n = List.length l1 in
      let s1 = unroll ((soi n)^"\n") l1 in
      aux (s^s1^(if t=[] then "" else "\n")) t
  in aux (soi (List.length l)^"\n") l
;;

(* print_string (writeSol [[0];[0;1;2]]);; *)


let writeSolToFile l filename =
  let s = writeSol l in
  let oc = open_out filename in
  fprintf oc "%s\n" s;
  close_out oc;;
  


