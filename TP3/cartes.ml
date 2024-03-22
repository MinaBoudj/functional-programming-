(* AMERKHANOVA Aida E204135L, BOUDJEDIR Amina E213138X 685K *)
(************ TP3 *************)

(************* EXO 2 **************)
(****************** 1 *****************)

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

(****************** 2 *****************)

type couleur =
    Pique | Coeur | Carreau | Trefle;;
(*type couleur = Rouge | Noir*)

(****************** 3 *****************)

type carte = Carte of face * couleur;;

(****************** 4 *****************)

let roipique = Carte (Roi,Pique);;

let ascoeur = Carte (As,Coeur);; 

(****************** 5 *****************)

(** let compare carte1 carte2 = carte1 = carte2 ;; **)

(** fonction qui affecte un nombre à une face **)
(* number_carte : face -> int  *)
let number_carte valeur =
  match valeur with
  | Deux -> 2
  | Trois -> 3
  | Quatre -> 4
  | Cinq	-> 5
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


(** comparer deux carte **)
(* compare : face -> face -> bool *)
let compare carte1 carte2 =  
  (number_carte  carte1) > (number_carte carte2)
;;


compare Deux Trois ;;
(* - : bool = false *)

compare As Roi;;
(* - : bool = true *)

(****************** 6 *****************)

(** fonction qui donne la valeur de la carte à partir d'un nombre **)
(* valeur_carte : int -> face *)
let valeur_carte number =
  match number with
  |2 	-> Deux
  |3 	-> Trois		
  |4  -> Quatre	
  |5 	-> Cinq		
  |6 	-> Six
  |7	-> Sept		
  |8	-> Huit
  |9	-> Neuf
  |10	-> Dix
  |11	-> Valet
  |12	-> Dame
  |13	-> Roi
  |14	-> As
;;

(** fonction qui donne un nombre à une valeur de la couleur **)
(* valeur_couleur : int -> couleur *)
let valeur_couleur number =
  match number with
  | 1 -> Pique 
  | 2 -> Coeur
  | 3 -> Carreau 
  | 4 -> Trefle 
;;

(** fonction qui tire une carte aleatoirement dans le paquet de 52 carte une carte au hasard **)
(* tire_random : unit -> carte *)
let tire_random () = 
  let random_face = valeur_carte((Random.int 13)+2) in
  let random_couleur = valeur_couleur((Random.int 4)+1) in
  Carte(random_face, random_couleur) 
;;

tire_random() ;;
(* - : carte = Carte (Sept, Carreau) *)

tire_random() ;;
(* - : carte = Carte (Deux, Pique) *)

tire_random() ;;
(* - : carte = Carte (Six, Trefle) *)

(**6 fonction test qui teste la fonction tire_random au moins 100 tirages aleatoire **)
(* test : bool = true *)
let test =
  let rec aux a_tester i  =
    if i = 0  then	true 
    else aux a_tester (i-1) 
  in aux tire_random 100
;; 

test ;;
(* - : bool = true *)

(****************** 7 *****************)

(** fonction qui attribue un score à une carte : as gagne 11 points , 10 gagne 10 points, roi gagne 4 points 
Dame gagne 3 points , valet gagne 2 points, le reste ne gagne rien **)
(* score_carte : carte -> int *)
let score_carte carte =
  match carte with 
    Carte (As, _) -> 11
  	| Carte (Dix, _) -> 10
  	| Carte (Roi, _) -> 4
  	| Carte (Dame, _) -> 3 
  	| Carte (Valet, _) -> 2
  	| Carte (_, _) -> 0
;;
 
score_carte roipique ;;
(* - : int = 4 *)

score_carte (Carte(Neuf,Pique)) ;;
(*- : int = 0 *)

(****************** 8 *****************)

let bataille  =
	let joueur_un = tire_random() in
	let joueur_deux = tire_random() in
	let score_un = score_carte joueur_un in
	let score_deux = score_carte joueur_deux in(
		if (score_un > score_deux) then	(
      		print_string "Player one wins "; (score_un + score_deux))
   		else if (score_un < score_deux ) then ( 
				print_string "Player two wins "; (score_un + score_deux)) 
   			else 
     		(print_string "Egalité" ; 0))
;;
 

(****************** 9 *****************)
(* Le jeu  que nous avons implementé est une version simplifiée de la bataille où les joueurs
  tirent juste une carte.
  Pour le rendre equivalent à une vrai bataille il faut :
  - gerer les egalites : dans la vraie bataille, lorsqu'il y a une egalité, les joueurs place une  
  carte face cachée et une carte face si c'est encore une égalité on recommence.
*)