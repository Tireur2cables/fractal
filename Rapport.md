## IDENTIFIANTS

BATTAGLINI, Nicolas, 71801244, @Tireur2Cables  
BARRAULT, Victor, 71803922, @Barrault  

## FONCTIONNALITÉS DU PROJET
Nous avons réalisé le sujet "minimal", en prenant en compte les `ou mieux ...`.  Ainsi nous avons un programme compilable via dune, exécutable par la commande `./run`, et indépendant du toplevel OCaml.  
Notre programme lit les fichiers formatés comme les fichiers fournis dans le répertoire `examples/`. Il lit ainsi les trois caractéristiques d'un L-système :  
Son `axiome` initial, ses `règles de transformations` ainsi que ses `interprétations`, séparés chacun par une ligne vide.  
Aussi, n'importe quel L-Système créé par l'utilisateur dans un tel fichier peut être donné au programme via l'option `-c` suivis du chemin jusqu'au fichier et/ou du nombre d'itérations souhaité.  
Par exemple : `./run -c examples/snow.sys 5` lance le programme sur le fichier `snow.sys` du dossier `examples` et appliquera 4 fois les règles de transformations.  
Les chaînes parenthésées sont exécutées à la volée sans mise en mémoire, on transforme chaque caractère via la règle du L-système autant de fois que le nombre
d'itération voulu, puis on l'affiche à l'écran selon son interprétation, et l'on passe ensuite au caractère suivant. Ainsi, on ne stock en mémoire que le sous-arbre des appels pour chaque caractère, comme suggéré dans la section 2.4 du sujet.  
L'interface du programme est `épurée`, disposant uniquement d'une fenêtre affichant le résultat de notre L-système, sur laquelle il suffit de cliquer pour quitter le programme une fois le dessin fini.  
Le calcul de la taille des bornes du graphique est fait automatiquement par le programme, en exécutant une première fois le L-Système, pour calculer sa taille d'affichage. Cela nous permet lors d'un second calcul du L-système, et de son affichage, d'appliquer un coefficient multiplicateur sur les actions de déplacement, `lineto` et `moveto`, de la tortue.  
Le calcul de la taille et du placement de la forme nous ont posé beaucoup de problèmes, notamment avec le type `Float` et les arrondis nécessaires lors de l'affichage du graphique qui peuvent entraîner un refus de coopérer de la part de notre très cher tortue qui ne peut pas afficher à l'écran des moitiés de pixels.  
Cependant nous avons autorisé notre tortue à ce reposer un court moment entre deux traits ce qui créer une animation du dessin. Ce laps de temps est calculer par rapport à la taille du dessin et longueur des traits. (cf README.md pour désactiver l'animation pour les tests).  
Nous avons aussi rajouté certaines couleurs pour le dessin. Afin d'obtenir un résultat lisible et agréable, nous prenons la couleur en fonction de la position du trait dans la fenêtre, formant ainsi un `degradé` de la fréquence d'apparation des couleurs.  


## COMPILATION ET EXÉCUTION

Le programme disposant d'un fichier `Makefile` il suffit de taper la commande `make` pour le compiler.  
Vous pourrez ensuite exécuter le programme grâce à la commande `./run` citée précédemment avec ou sans l'option `-c` permettant de spécifier un fichier contenant un L-système et/ou un nombre d'itérations spécifique.  

## FONCTIONNEL PUR ET DÉCOUPAGE MODULAIRE

Nous avons écris ce programme dans l'esprit de la programmation fonctionnelle pure. Cependant ne voyant pas d'autres alternatives, nous avons du nous résoudre à faire une petite entorse à cette idée lors de la gestion des arguments en utilisant le tableau `argv`.  
Notre programme se veut également modulaire avec un fichier `systems.ml` traitant la lecture et la création du L-système qui sera utilisé par le fichier `main.ml` pour obtenir chacune des commandes relative à la tortue. Ces commandes sont enfin utilisées par le fichier `turtle.ml` qui s'occupe d'interpréter les commandes de la tortue pour utiliser les bonnes fonctions de la bibliothèque `Graphics` de OCaml.  

## ORGANISATION DU TRAVAIL

Ayant eu des emplois du temps extrêmement chargés dans la fin de l'année 2020, nous n'avions pas pu autant avancer que prévu sur le projet avant ce début d'année 2021.  
Cependant, avec une bonne répartition des tâches nous avons pu rapidement rattrapé le retard pris.  
En effet la création de la tortue et la lecture des fichiers L-système ont été nos premiers objectifs. Nous avions décidé à l'annonce du sujet que Nicolas s'occuperais de la tortue et ses déplacements, et que Victor travaillerais sur la lecture des fichiers et leur transformations en type system.    
Nous avons pu ensuite nous pencher tous les deux sur la tâche plus délicate de la transformations des L-systèmes en commandes pour la tortue.  
Nous pouvions à présent tester notre programme et pour l'un résoudre les quelques bugs visibles à présent (comme certains problèmes de conversions des `Float`, déjà!, lors des appels aux fonctions de `Graphics`), pour l'autre ajouter la possibilité de spécifier un fichier et/ou nombre d'itérations via l'option `-c`.  
Ensuite nous avons pu réfléchir ensemble au problème non négligeable de calcul de la taille du dessin et de son positionnement qui nous auront posés pas mal de soucis, tant au niveau de la réflexion qu'avec les `Float` (encore!).  
Arrivés à ce stade nous avons pu nous atteler à animer la tortue et centrer au maximum le dessin sur la fenêtre.  

## AUTRE

Comme nous avons pu le cité nous avons pu rencontrés beaucoup de problèmes avec les `Float` et les opérations sur ceux-ci ou encore certaines conversions obligatoires en entiers (par exemple pour `Graphics`). Cependant nous avons aussi rencontrés des problèmes dus à notre version d'OCaml qui est la `4.05.0`
alors que beaucoup, et bien plus que ce qu'indique la documentation visiblement, de nouvelles fonctions nécessitent une version supérieure à `4.08`.  
Nous avions également prévu d'ouvrir une fenêtre de Graphics de la taille de l'écran. Cependant nous n'avons pas pu trouver de modules permettant de faire
cela et avons du nous résoudre à laisser une taille générique de 1000px par 1000px.  
Nous rencontrons aussi certains problèmes quand le dessin à afficher est beaucoup plus grand que la fenêtre d'affichage. En effet, on ne peut pas faire de tracés de moins de 1px avec la tortue, ce qui implique qu'un dessin trop grand ne pourra pas être affiché, ou sera déformé (plus ou moins gravement).  
