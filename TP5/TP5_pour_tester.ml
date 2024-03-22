(* Ci-dessous des fonctions permettant de construire des arbres de type abr (TP5) de tailles variables.
LEs arbres sont de trois sortes : peigne (chaque noeud n'a qu'un enfant, à gauche ici), 
plein (tous les noeuds ont deux enfants) ou quelconque (chaque enfant a une probabilité 1/2 d'être une feuille, 
1/2 d'être un noeud). Pour chaque sorte, on peut constuire un arbre qui est bien un ABR, 
et un qui n'en est probablement pas un (les labels des noeuds sont choisis aléatoirement).
*)

let rec construit_abr_peigne n =
    (* ici n est la profondeur et le nombre de noeuds *)
  if n=0 then Leaf else Node (construit_abr_peigne (n-1), n, Leaf);;

let rec construit_non_abr_peigne n maxval=
    (* ici n est la profondeur et le nombre de noeuds *)
  if n=0 then Leaf else Node (Leaf, Random.int maxval, (construit_non_abr_peigne (n-1) maxval));;

let construit_abr_plein n =
  (* attention ici n est la profondeur *)
  let rec pow2 n = if n = 0 then 1 else (if (n mod 2=0) then 1 else 2) * (pow2 (n/2) * pow2 (n/2)) in
  let rec aux n pivot base=
  if n = 0 then Leaf
    else Node (aux (n-1) (pivot / 2) base, pivot / 2+base, aux (n-1) (pivot/2) (base+pivot/2) ) in
    aux n (pow2 n) 0
  ;;

let rec construit_non_abr_plein n maxval=
  (* attention ici n est la profondeur *)
  if n = 0 then Leaf
    else Node (construit_non_abr_plein (n-1) maxval, Random.int maxval, construit_non_abr_plein (n-1) maxval)
  ;;

let construit_abr_quelconque n =
  (* attention ici n est la profondeur *max*, possiblement pas atteinte *)
  let rec pow2 n = if n = 0 then 1 else (if (n mod 2=0) then 1 else 2) * (pow2 (n/2) * pow2 (n/2)) in
 let rec aux n pivot base =
  if n=0 then Leaf 
    else if Random.int 2=0 then Leaf
      else Node (aux (n-1) (pivot / 2) base, base+pivot / 2, aux (n-1) (pivot/2) (base+pivot/2)) in
    aux n (pow2 n) 0
  ;;

let rec construit_non_abr_quelconque n maxval =
  (* attention ici n est la profondeur *max*, possiblement pas atteinte *)
  if n=0 then Leaf 
    else if Random.int 2=0 then Leaf
      else Node (construit_non_abr_quelconque (n-1) maxval, Random.int maxval, construit_non_abr_quelconque (n-1) maxval)
  ;;