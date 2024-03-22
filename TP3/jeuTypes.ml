(* AMERKHANOVA Aida E204135L, BOUDJEDIR Amina E213138X 685K *)
(************ TP3 *************)

(************* EXO 1 **************)

(* int -> (int -> int) -> 'a -> bool *)
let f x identite y =
  if((x+1) = (identite x)) then true
  else false
;;

(* ('a -> bool list) -> 'a -> int *)
let f x y =
    match (x y) with
    | t::q-> (if(t=true)then 1 else 0)
    | [] -> 0
;;

(* 'a -> 'b *)
let rec indefini x = indefini x
