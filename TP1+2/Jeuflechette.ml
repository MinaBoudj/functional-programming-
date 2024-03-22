(************* TP2 **********)
(* AMERKHANOVA AIDA , BOUDJEDIR AMINA *)
(* EXO2 *)

(* Si sont score courant est plus petit que 3 il pourra plus atteindre 0 *)

(* val tour_501 : int -> int = <fun> *)
let tour_501 score_courrant =
  let acc = (lancer_de()+lancer_de()+lancer_de()) in
  if(acc<=score_courrant)then score_courrant-acc
  else score_courrant
;;

tour_501 41;;
tour_501 2;;


(* Fonction pour simuler une partie complète de 501 pour un nombre donné de joueurs *)
(* val jouer_501 : int -> unit = <fun> *)
let jouer_501 nb_joueurs =
  let rec tour_joueur joueur score =
    print_string ("Joueur " ^ (string_of_int joueur) ^ " ");
    let nouveau_score = tour_501 score in
    print_string ("Score: "^(string_of_int nouveau_score) ^ " ");
    print_newline();
    if nouveau_score = 0 then (
      print_string ("Le joueur "^(string_of_int joueur)^" a gagné !");
      print_newline(); 
    )
    else tour_joueur ((joueur mod nb_joueurs) + 1) nouveau_score
  in
  print_string "Tour 1";
  print_newline();
  tour_joueur 1 501; 
  
;;

(* Exemple d'utilisation avec 2 joueurs *) 
jouer_501 2;;

  