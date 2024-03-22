(*  BOUDJEDIR Amina E213138X, AMERKHANOVA Aida E204135L 685K *)
(************ TP Optionnel *************)

(* les notes sont de type prmitif entier*)

(*int -> int -> int -> int -> int list = <fun>*)
let rec arpegie_montante basse itv1 itv2 nbOctave = 
	if(nbOctave = 0) then []
	else (basse+itv1)::(basse+itv2)::(basse+12)::(arpegie_montante (basse+12)  itv1 itv2 (nbOctave-1))
;;
    
 (*int -> int -> int -> int -> int list = <fun>*)
 let rec arpegie_descendante basse itv1 itv2 nbOctave = 
	if(nbOctave = 0) then []
	else List.rev (arpegie_montante basse itv1 itv2 nbOctave)
;;

(*int -> int -> int -> int -> int list = <fun>*)
let arpegie basse itv1 itv2 nbOctave =
	(arpegie_montante basse itv1 itv2 nbOctave)@(arpegie_descendante basse itv1 itv2 nbOctave);;

let midi_to_frequence m = 440.*.(2.**(((float_of_int m)-.69.)/.12.)) ;; 
 
List.map midi_to_frequence (arpegie 60 4 7 2);;
(*[329.62755691286992; 391.995435981749267; 523.251130601197247;
 659.255113825739841; 783.990871963498535; 1046.50226120239449;
 1046.50226120239449; 783.990871963498535; 659.255113825739841;
 523.251130601197247; 391.995435981749267; 329.62755691286992]*)

List.map midi_to_frequence (arpegie 45 7 4 2);;
(*[164.81377845643496; 138.591315488436038; 220.; 329.62755691286992;
 277.182630976872076; 440.; 440.; 277.182630976872076; 329.62755691286992;
 220.; 138.591315488436038; 164.81377845643496]*)
