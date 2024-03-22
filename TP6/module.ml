type nom = Do | Re | Mi | Fa | Sol | La | Si ;;
type alteration = Becarre | Diese;;
type hauteur = {nom : nom ; alteration : alteration ; octave : int};;

let do4={nom = Do; alteration = Becarre ; octave = 4};;
let dod4={nom = Do; alteration = Diese ; octave = 4};;
let mi3={nom = Mi; alteration = Becarre ; octave = 3};;
let la2={nom = La; alteration = Becarre ; octave = 2};;
let la3={nom = La; alteration = Becarre ; octave = 3};;
let la4={nom = La; alteration = Becarre ; octave = 4};;

type score = Score of hauteur list;;

let partition = Score [do4;dod4;mi3;mi3;la2;la4;la3;la3;do4;mi3];;

let note2hauteur n =
  let degre= match n.nom with
    | Do -> 0
    | Re -> 2
    | Mi -> 4
    | Fa -> 5
    | Sol -> 7
    | La -> 9
    | Si -> 11 in
  let midibec = 60+(n.octave-3)*12+degre in
  match n.alteration with
  | Becarre -> midibec
  | Diese -> midibec + 1
;;


let nom2string n =
  match n with
  | Re -> "Re"
  | Do -> "Do"
  | Mi -> "Mi"
  | Fa -> "Fa"
  | Sol -> "Sol"
  | La -> "La"
  | Si -> "Si"
;;

let alteration2string a =
  match a with
  | Becarre -> ""
  | Diese -> "#"
;;

let print_hauteur h =
  print_string ((nom2string h.nom)^(alteration2string h.alteration)^(string_of_int h.octave));
  print_newline ()
;;

(*Q1*)
let compare_hauteur h1 h2 = 
  if note2hauteur h1 > note2hauteur h2 then 1 
  else if note2hauteur h1 < note2hauteur h2 then -1
  else 0;;

(*Q2*)
module HauteurOrdonnee =
struct
  type t = hauteur
  let compare = compare_hauteur
end

module EnsembleHauteurs = Set.Make(HauteurOrdonnee);;

(*Q3*) 
let ensemble_hauteurs partinion = match partition with
    Score l ->
      List.fold_left (fun acc h -> EnsembleHauteurs.add h acc) EnsembleHauteurs.empty l ;;
 
(*Q4*)
let afficher_partition ens_h = EnsembleHauteurs.iter print_hauteur ens_h;;

afficher_partition (ensemble_hauteurs partition);;

(*Q5*)
module MapHauteurs = Map.Make(HauteurOrdonnee);;

let enumeration_hauteurs score =


    









