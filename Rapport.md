## IDENTIFIANTS

BATTAGLINI, Nicolas, 71801244, @Tireur2Cables  
BARRAULT, Victor, 71803922, @Barrault  

## FONCTIONNALITÉS DU PROJET
Nous avons réalisé le sujet minimal, en prenant en compte le maximum de `ou mieux ...`.  Ainsi nous avons un programme compilable via dune, et executable par la commande ./run,
indépendant du toplevel ocaml.  
Notre programme lit les fichiers formatés comme les fichiers fournis dans le répertoire `examples/`. Il lit ainsi les trois caractéristiques du L-système, son `axiom`  de base,
ses `regles de transformations`, et son interprétations, séparés chacun par une ligne vide. Ainsi, n'importe quel L-Système créé par l'utilisateur peut être donné 
au programme avec l'option `-c` suivis du chemin du fichier et/ou du nombre d'itération souhaité.  
exemple : `./run -c ./examples/br1.sys 4`  
Les chaines parenthésées sont éxécutées à la volée sans mise en mémoire, on transforme chaque caractère via la règle du système autant de fois que le nombre 
d'itération voulu, puis on l'affiche à l'écran selon son interprétation, puis on passe au caractère suivant. Ainsi, on ne stock en mémoire que le sous-arbre des appels pour chaque 
caractères, comme mentionné dans la section 2.4.  
L'interface du programme est `épurée`, uniquement une fenetre affichant le résultat de notre L-système.  
Le calcul de la taille des bornes du graphique est fait automatiquement par le programme, en executant une première fois le L-Système, et en calculant sa taille 
d'affichage avec ses parametres actuels. Lors du second calcul et de l'affichage, on applique un coefficient multiplicateur sur les `lineto` et `moveto` de la tortue.  
Le calcul de la taille et du placement de la forme nous ont posé beaucoup de problème, certains qui ne sont toujours pas vraiment résolus.
  
Dans le sujet minimal, nous n'avons pas réalisé l'animation de dessin de la tortue, le tracé de la forme s'affiche directement après les calculs.

## COMPILATION ET EXECUTION
Compilation et execution.  

## DECOUPAGE MODULAIRE
Découpage modulaire.  

## ORGANISATION DU TRAVAIL
Organisation du travail.  

## AUTRE