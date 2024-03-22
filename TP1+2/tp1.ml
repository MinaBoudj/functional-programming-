(********* TP1 ************)
(* AMERKHANOVA, BOUDJEDIR 685K *)

(********* Exercice 1 *********)
(* une expression arithmétique sur les entiers avec au moins 3 opérateurs *)
3+2-1;;

 
(* une expression booléenne avec au moins 3 opérateurs et deux déclarations *)
let x=3;;
let y = x+42;;
x+1<y-2;;

 
(* une expression contenant au top level un if *)
let x=3;;
let y = x+42;;
if x > y then print_string ("hello")
else print_string ("bye")
;;

 
(* une expression contenant au top level un match *)
match x with
  |3 -> true
  |_ -> false
;;


(* une fonction qui prend en argument une fonction *)
(* la fonction à passer en argument *)
let inc x = x+1;;
(* appel de la fonction principale, prenant en param inc et 5 *)
(fun toto z -> z + toto z) inc 5
;;


(* une fonction récursive simple de votre choix *)
(* somme de k entiers : exemple pour k = 3 : 3 + 2 + 1 = 6 *)
(* ici la fonction aux permet de initialiser la future somme m à 0 *)
let somme k =
	let rec aux k m =
		if k = 0 then m
        else aux(k-1) m + k
	in aux k 0;;
							

(* une expression provoquant les erreurs de typage suivantes *)
(* Error: This expression has type float but an expression was
 expected of type int *)
3+3.;;
 
(* Error: This function has type int -> int It is applied to
 too many arguments; maybe you forgot a ‘;’. *)
let f q = q+1;;
f 3 2;;
 

(* Error: This expression has type int This is not a function;
 it cannot be applied. *)
t = 2;;
t 5;;


(********* Exercice 2 *********)
(* Q1: La fonction Ackermann est recursive, de type int -> int -> int *)
 
(* Q2: La fonction Ackermann *)
let rec ackermann m n = match m with
    | 0 -> n + 1
    | _ -> if n = 0 && m>0 then ackermann(m-1) 1
            else ackermann(m-1) (ackermann m (n-1))
;;
 
 
(* Fonction qui gère les espacements des affichages *)
let rec tab cpt =
    if cpt > 0 then (print_string "    "; tab (cpt-1))
    else ()
;;
				
 
let print m n =
    print_string "Ackermann ";
    print_int m;
    print_string " ";
    print_int n;
    print_newline()
;;
 
 
(* Q3: Ackermann avec l'affichage des appels successifs *)
let pretty_ackermann m n =
    let rec aux m n cpt =
    tab cpt;
    print m n;
    match m with
        | 0 -> n + 1
        | _ -> if n = 0 && m>0 then aux(m-1) 1 cpt
                else aux(m-1) (aux m (n-1) (cpt+1)) cpt
    in aux m n 0;
;;




