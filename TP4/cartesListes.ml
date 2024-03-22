(* AMERKHANOVA Aida E204135L, BOUDJEDIR Amina E213138X 685K *)
(************ TP4 *************)


(* Face qui fera partue du type Carte *)
type face =
    Deux
  | Trois
  | Quatre
  | Cinq | Six | Sept | Huit | Neuf
  | Dix
  |Valet
  | Dame
  | Roi
  | As
;;

(* Couleur qui fera partie du type Carte *)
type couleur =
    Pique | Coeur | Carreau | Trefle;;

(* Carte constituée d'une couleur et d'une face *)
type carte = Carte of face * couleur;;

(* Liste de faces *)
let faces = [Deux; Trois; Quatre; Cinq; Six; Sept; Huit; Neuf; Dix; Valet; Dame; Roi; As];;

(* Liste des couleurs *)
let couleurs = [Pique; Coeur; Carreau; Trefle];;
 
 
(* 1: fonction deck qui retourne toutes les cartes possibles
    ici la complexité n'est pas meilleure car on fait un parcours complet de
    la liste à chaque fois.
    La fonction peut être refaite avec un fold left *)
(* type : face list -> couleur list -> carte list -> carte list = <fun> *)

let rec combine_faces_couleurs faces couleurs acc =
  match faces with
  | [] -> acc
  | face :: rest_faces ->
      let combinaison = List.map (fun couleur -> Carte (face, couleur)) couleurs in
      combine_faces_couleurs rest_faces couleurs (acc @ combinaison)
;;

(* Création d'un deck *)
let deck = combine_faces_couleurs faces couleurs [];;


(* 2: Place la i-ème carte en tête de liste *)
(* type:'a list -> int -> 'a list = <fun> *)
let echange l i =
  let rec aux l i acc =
    match l with
      [] -> acc
    |a::s -> if i=0 then (a :: acc)@s
        else aux s (i-1) (acc@[a])
  in aux l i []
;;

(* Exemple d'utilisation *)
let exemple = echange [1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13] 7;;


(* 3: melange les cartes un nombre de fois donnée, en echangeant deux cartes *)
(* type: 'a list -> int -> 'a list = <fun> *)
let melange l nbfois =
  let rec aux l nbfois =
    if nbfois = 0 then l
    else let i = Random.int (List.length l) in
      let l = echange l i in aux l (nbfois - 1)
  in aux l nbfois
;;


(* 4: Melange la liste puis la divise en 2 listes de même taille.
    Si la liste a un nombre impair des elements, on ne distribue pas
    le dernier element *)
(* type: 'a list -> 'a list * 'a list = <fun> *)
    
let distribue l =
  let l = melange l (List.length l / 2) in
  let rec aux l1 l2 l =
    match l with
      [] -> (l1, l2)
    |a::[] -> (l1, l2)
    |a::reste -> aux (a::l1) (List.hd reste::l2) (List.tl reste)
  in aux [] [] l
;;


(* 5: Si les deux cartes ont la même face, on remet ces cartes à la fin et remelange la liste *)


(* Fonction qui retourne le nombre d'une face *)
(* type: face -> int  *)
let number_carte valeur =
  match valeur with
  | Deux -> 2
  | Trois -> 3
  | Quatre -> 4
  | Cinq    -> 5
  | Six -> 6
  | Sept -> 7
  | Huit -> 8
  | Neuf -> 9
  | Dix -> 10
  | Valet -> 11
  | Dame -> 12
  | Roi -> 13
  | As -> 14
;;

(* Fonction qui retourne le nombre d'une carte *)
(* type: carte -> int = <fun> *)
let number_carte1 carte =
    match carte with
    Carte (face, couleur)->number_carte face
;;

(* Fonction qui affiche le nom de la face *)
(* type: face -> string = <fun> *)
let afficher_face face =
  match face with
  | As -> "As" | Roi -> "Roi" | Dame -> "Dame"
  | Valet -> "Valet" | Deux -> "2" | Trois -> "3"
  | Quatre -> "4" | Cinq -> "5" | Six -> "6"
  | Sept -> "7" | Huit -> "8" | Neuf -> "9"
  | Dix -> "10"
;;

(* Fonction qui gère le cas d'égalité: on remet la première carte à la fin,
    puis on melange les cartes *)
(* type: 'a list -> 'a list = <fun> *)
let fix_egalite l =
  echange l ((List.length l)-1);
  melange l 3
;;


(* 6: Fonction qui prend en entrée deux listes de cartes, détermine la carte gagnante, et renvoie les deux nouveaux jeu de cartes, celui du perdant privé de la carte jouée, celui du gagnant augmenté de la carte qu’il vient de gagner. En cas d'égalité on melange les cartes. *)
(* type : carte list -> carte list -> carte list * carte list = <fun> *)

let un_tour jeu1 jeu2 =
  let carte1 = List.hd jeu1 in
  let carte2 = List.hd jeu2 in
  match (carte1, carte2) with
  | Carte (face1, couleur1), Carte (face2, couleur2) ->
      let numero1 = number_carte face1 in
      let numero2 = number_carte face2 in
      print_newline(); print_string ("Joueur 1 : "^afficher_face face1^". Joueur 2: "^afficher_face face2);
      if numero1 > numero2 then (print_newline(); print_string("Joueur 1 a gagné"); (List.tl (jeu1@[carte1;carte2]), List.tl jeu2))
      else if numero1 < numero2 then (print_newline(); print_string("Joueur 2 a gagné"); (List.tl jeu1, List.tl (jeu2@[carte1;carte2])))
      else ( print_newline(); print_string("Egalité. On mélange les cartes"); ((fix_egalite jeu1), (fix_egalite jeu2)))
;;


(* 2eme version de la fonction un tour qui prend en entrée les strategies choisies par joueur. En cas d'égalite, on refait le tour en melangeant les cartes et sans utiliser les strategies pour ne pas boucler *)
(* type: carte list -> carte list -> (carte list -> carte) -> (carte list -> carte) -> carte list * carte list = <fun> *)

let un_tour_v2 jeu1 jeu2 strat1 strat2=
  let carte1 = strat1 jeu1 in
  let carte2 = strat2 jeu2 in
  match (carte1, carte2) with
  | Carte (face1, couleur1), Carte (face2, couleur2) ->
      let numero1 = number_carte face1 in
      let numero2 = number_carte face2 in
      print_newline(); print_string ("Joueur 1 : "^afficher_face face1^". Joueur 2: "^afficher_face face2);
      if numero1 > numero2 then (print_newline(); print_string("Joueur 1 a gagné"); (List.tl (jeu1@[carte1;carte2]), List.tl jeu2))
      else if numero1 < numero2 then (print_newline(); print_string("Joueur 2 a gagné"); (List.tl jeu1, List.tl (jeu2@[carte1;carte2])))
      else ( print_newline(); print_string("Egalité. On mélange les cartes"); (un_tour (fix_egalite jeu1) (fix_egalite jeu2)))
;;


(* Fonction qui compare deux cartes en fonction de leur face.
    Utilisé dans la fonction List.sort *)
(* type: carte -> carte -> int = <fun> *)
let compare a b =
  if((number_carte1 a) > (number_carte1 b)) then 1
  else if ((number_carte1 a) < (number_carte1 b)) then -1
  else 0
;;

(* Fonction qui verifie si la liste des cartes est vide *)
(* type: 'a list -> bool = <fun> *)
let jeu_vide jeu =
  match jeu with
    [] -> true
  | _ -> false
;;


(* 7: Fonction qui implémente l’ensemble du jeu : distribution des cartes, tours de jeu jusqu’à ce qu’un des joueurs n’aient plus de cartes
    ou jusqu'à on n'a pas atteint un nombre n de tours *)
(* type: carte list -> unit = <fun> *)

let bataille_ouverte deck =
  let jeu1, jeu2 = distribue deck in
  let rec aux jeu1 jeu2 n =
    if n<500 then
      
      match jeu1, jeu2 with
        _,[] ->print_string "Joueur 1 a gagnée FIN !"
      |[],_ ->print_string "Joueur 2 a gagné FIN !"
      |_,_ -> let newjeu1, newjeu2 = un_tour jeu1 jeu2 in  aux newjeu1 newjeu2 (n+1)
    
    else if (List.length jeu1 < List.length jeu2) then (print_newline();
                                                        print_string("Le nombre de tours possibles est depassé, Joueur 2 a gagné avec "
                                                                     ^string_of_int(List.length jeu2)^" cartes"))
    else if (List.length jeu1 > List.length jeu2) then (print_newline();
                                                        print_string("Le nombre de tours possibles est depassé, Joueur 1 a gagné avec "
                                                                     ^string_of_int(List.length jeu1)^" cartes"))
    else (print_newline(); print_string("Le nombre de tours possibles est depassé. Les deux jeueurs ont le même nombre de cartes. Égalité."))
  in aux jeu1 jeu2 0
;;



(* 8: STRATEGIES *)

(* Fonction qui trie les cartes dans l'ordre croissant*)
(* type: carte list -> carte = <fun> *)
let strategie_faibles l =
  List.hd (List.sort compare l)
;;

(* Fonction qui trie les cartes dans l'ordre decroissant*)
(* type: carte list -> carte = <fun> *)
let strategie_fortes l =
  List.hd (List.sort (fun a b -> -(compare a b)) l)
;;

(* Fonction qui n'applique aucune startiegie *)
(* type: 'a list -> 'a = <fun> *)
let strategie_vide l =
  List.hd l
;;


(* 9: 2eme version de bataille_ouverte qui prend en entrée les strategies des joueurs *)
(* type: (carte list -> carte) -> (carte list -> carte) -> unit = <fun> *)
let bataille_ouverte_v2 strat1 strat2 =
  let jeu1, jeu2 = distribue deck in
  let rec aux jeu1 jeu2 n =
    if n<500 then
      
      match jeu1, jeu2 with
        _,[] ->print_string "Joueur 1 a gagnée FIN !"
      |[],_ ->print_string "Joueur 2 a gagné FIN !"
      |_,_ -> let newjeu1, newjeu2 = un_tour_v2 jeu1 jeu2 strat1 strat2 in  aux newjeu1 newjeu2 (n+1)
    
    else if (List.length jeu1 < List.length jeu2) then (print_newline();
                                                        print_string("Le nombre de tours possibles est depassé, Joueur 2 a gagné avec "
                                                                     ^string_of_int(List.length jeu2)^" cartes"))
    else if (List.length jeu1 > List.length jeu2) then (print_newline();
                                                        print_string("Le nombre de tours possibles est depassé, Joueur 1 a gagné avec "
                                                                     ^string_of_int(List.length jeu1)^" cartes"))
    else (print_newline(); print_string("Le nombre de tours possibles est depassé. Les deux jeueurs ont le même nombre de cartes. Égalité."))
  in aux jeu1 jeu2 0
;;


(* appels avec differentes strategies *)
bataille_ouverte_v2 strategie_faibles strategie_fortes;;

bataille_ouverte_v2 strategie_faibles strategie_vide;;