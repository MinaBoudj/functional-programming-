(************* TP2 **********)
(* AMERKHANOVA AIDA , BOUDJEDIR AMINA *)
(* EXO1 *)

(*fonction qui renvoie un entier aléatoire entre 1 et 6*) 
Random.self_init () ;;
let lancer_de = 1+ Random.int 5;;
let x = lancer_de;;


let lancer_de () = 
  Random.self_init();
  let resulte = Random.int 6 in
  resulte + 1 
;;

lancer_de();;
lancer_de();;
lancer_de();;


(* permet de jouer au jeu et renvoyer le score atteint
  Si on fait un 5 ou un 6, on relance le dès pour augmenter 
son score, sinon on s'arrete *) 
(* val jouer : unit -> int = <func> *) 
let jouer() =
  let rec aux score de = 
    print_string ("Le resultat du lancer est : " ^ (string_of_int de) ^ ", le score courant est : "^(string_of_int (de+score)));
    print_newline();
    match de with 
    |5 -> aux (score+5) (lancer_de ())
    |6 -> aux (score+6) (lancer_de ())
    |_-> (print_string ("score de Fin = "^(string_of_int (score+de))) ; print_newline(); score)
         
  in aux 0 (lancer_de ())
;;


(* val jouer_tour : unit -> unit = <func> *)
let jouer_tour () = 
  let rec aux tours = 
    if tours > 0 then (print_newline(); print_string "Nouveau tours :";
                       print_newline();
                       print_int (jouer()); 
                       aux (tours-1))
    else (print_newline(); print_string "Fin des tours ")
      
  in aux 2;  
;;

jouer_tour();;