# Functional programming

Ces TPs sont des implementations de tp dans le cadre de travaux partique à l'université de Nantes.

## TP1
## Premiers pas et fonctions
## Compilation et exécution  
Pour pouvoir commencet le tp, il est nécessaire de suivre les étapes suivantes :

- **1** : Télécharger le module Ocaml correspondant à l'adresse suivante : [https://gluonhq.com/products/javafx/](https://ocaml.org).
- **2** : Pour compiler les fichiers vous pouvez utiliser la commande suivante :

        ocamlc -o test test.ml

## Fonction ackermann
La fonction ackermann est définit ainsi :
          A(m,n) = n + 1           si m = 0
                 = A(m-1,1)        si n = 0 et m > 0
                 = A(m-1,A(m,n-1)) sinon
                 
implentez les fonctions suivantes :                
 1. Donnez le type de la fonction Ackermann.
 2. Ecrire la fonction ackermann qui calcule la fonction d'Ackermann.
 3. Ecrire une fonction pretty_ackermann qui affiche les appels successifs dans le calcul de la fonction.
 
## Supports utilisés
La liste des supports utilisés dans le tp :
- [Ocaml](https://ocaml.org) 
- [webSite](https://try.ocamlpro.com)


## Auteurs 
- BOUDJEDIR Amina
- AMERKHANOVA Aida
