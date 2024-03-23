# Functional programming

Ces TPs sont des implementations de tp dans le cadre de travaux partique à l'université de Nantes.

### TP1
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

### TP2
## Dés

Simulation de suite de lancers de dès, on commence avec un score de 0. On lance un dé et on additionne le résultat obtenu au score. Si on fait 5 ou un 6, on relance le dé pour augmenter son score, sinon on s'arrête :
 1. Fonction lancer_de qui renvoie un entier aléatoire entre 1 et 6.
 2. Fonction récursive jouer qui permet au jeu décrit et renvoie le score atteint.
 3. Un programme qui permet de jouer plusieurs tours de jeu. On initialisera le générateur aléatoire une seule fois au début du programme. 

## Jeu de fléchettes

On veut jouer au jeu de fléchettes du 501, adapté aux dés. Chaque joueur part avec 501 points, son objectif étant d’arriver à 0 exactement. Chacun son tour, chaque joueur tire trois dès, et, si le total de ces dès est plus petit que son score courant, il le retire à son score courant. Sinon, il garde son score courant. Le premier joueur à atteindre 0 a gagné :
 1. fonction tour_501 qui prend en entrée le score actuel d’un joueur, et simule un tour de jeu pour ce joueur, en retournant le score après ce tour. Vous vous assurerez que la fonction termine toujours.
 2. onctionjouer_501 qui prend en entrée le nombre de joueurs, et simule une partie complète de 501, en imprimant le joueur gagnant avant de terminer.

## Supports utilisés
La liste des supports utilisés dans le tp :
- [Ocaml](https://ocaml.org) 
- [webSite](https://try.ocamlpro.com)


## Auteurs 
- BOUDJEDIR Amina
- AMERKHANOVA Aida
