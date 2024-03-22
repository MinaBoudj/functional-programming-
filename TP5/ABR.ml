(* AMERKHANOVA Aida E204135L, BOUDJEDIR Amina E213138X 685K *)
(************ TP5 ABR *************)

type 'a abr = Leaf | Node of 'a abr * 'a * 'a abr;;

let compare x y = (x<=y);;

(*Q1*)
let a = Leaf;; (* arbre vide *)
let b = Node (Node (Leaf , 2, Leaf),3, Node(Leaf,4,Leaf));;
let c = Node (Node (Leaf, 4, Leaf), 2, Node(Leaf, 1, Leaf));;

(*Q2*)
let rec recherche f a arbre = match arbre with
    Leaf -> false 
  | Node (gauche, x, droite) -> if(x=a)then true else if (f a x = true) then recherche f a gauche
      else recherche f a droite
;;

recherche compare 2 b;;
recherche compare 0 b;;

(*Q3*)
let rec insere f a arbre = match arbre with
    Leaf -> Node(Leaf, a, Leaf)
  | Node(gauche, x, droite) ->if(x=a)then arbre else if(f a x) then Node (insere f a gauche,x, droite) 
      else  Node (gauche, x, insere f a droite) 
;;

insere compare 4 b;;
insere compare 0 b;;

(*Q4*)
(*supprimer le maximun de l'arbre*)
let rec supp_max arbre = match arbre with
    Leaf -> arbre
  | Node (gauche, x, droite) -> match droite with
      Leaf -> gauche
    | Node(_,_,droite1) -> Node(gauche, x, supp_max droite1)
;;

(*retourne le max de l'arbre a*)
let rec max arbre = match arbre with
    Leaf -> -1
  | Node(gauche, x, droite) -> match droite with
      Leaf -> x
    | Node (_,_,droite1) -> max droite1
;;

(*supprimer le deuxieme argument du troisieme argument*)
(*’a -> ’a -> bool) -> ’a -> ’a abr -> ’a abr*)
let rec supprime f a arbre = match arbre with
    Leaf -> arbre 
  | Node(gauche, x, droite) -> if(x=a)then match (gauche,droite) with
      |(Leaf,_) -> droite
      |(_, Leaf) -> gauche
      |(_,_) -> Node(supp_max gauche, max gauche, droite) 
      else if (compare a x)then Node(supprime f a gauche, x, droite)
      else Node (gauche, x, supprime f a droite)
;;

supprime compare 3 b;;
supprime compare 5 b;;

(*Q5*)
(*verifie si une liste est triée dans l'ordre droissant*)
(*(’a -> ’a -> bool) -> a list -> bool*)
let rec liste_triee f l = match l with
    [] -> true
  | [x] -> true
  |x::y::t -> f x y && liste_triee f (y::t)
;;

(*Q6*)
(*collecter les elements contenus dans un ABR*)
(*’a abr -> ’a list*)
let collecte arbre = 
  let rec aux arbre = match arbre with
      Leaf -> []
    | Node(gauche, x, droite) -> (aux gauche)@[x]@(aux droite)
  in aux arbre
;;

collecte b;;

(*Q7*)
(*verifier si un arbre est un vrai ABR*)
(*('a -> 'a -> bool) -> 'a abr -> bool*)
let est_abr1 f arbre = liste_triee f (collecte arbre);;

est_abr1 compare b;;
est_abr1 compare c;;

(*Q8*)
(*verifier que tous les elements de l'arbre verifie le predicat*)
(*(’a -> bool) -> ’a abr -> bool*)
let rec check predicat arbre = match arbre with
    Leaf -> true
  | Node (gauche, valeur, droite) ->
      predicat valeur && check predicat gauche && check predicat droite
;;

(*Q9*)
(* Fonction est_abr2 qui utilise check pour vérifier si un arbre est un ABR *) 
let rec est_abr2 arbre = 
  match arbre with
  | Leaf -> true
  | Node(gauche,valeur,droite) -> check (fun x -> x >= valeur) droite && check (fun x -> x <= valeur) gauche && est_abr2 gauche && est_abr2 droite
;;

est_abr2 b;;
est_abr2 c;;

(*Q10*)
(*Observez l'efficacité de abr1 et abr2 avec trace <nom_fonction>*) 
#trace est_abr1;;
#trace est_abr2;;

let trace1 = est_abr1 compare b;;
let trace2 = est_abr2 b;;

(*Q11*)
(*On peut observer le temps de calcul avec la fonction Sys.time : unit -> float1.*)
(*fonction observe : qui renvoie le temps de calcul d’un appel de fonction*)
(*(’a -> ’b) -> ’a -> float *) 
let observe f x = let debut = Sys.time() in let res = f x in let fin = Sys.time () in
  print_float (fin -. debut); res;;


let rec construit_abr_peigne n =
    (* ici n est la profondeur et le nombre de noeuds *)
  if n=0 then Leaf else Node (construit_abr_peigne (n-1), n, Leaf);;

let arbre = construit_abr_peigne 50;; 

let temps1 = observe est_abr1 compare arbre;;

let temps2 = observe est_abr2 arbre;;