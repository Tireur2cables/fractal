Projet PF5 2020 : L-systèmes
============================

## Prérequis à installer  

  - `OCaml` évidemment (>= `4.05.0`).
  - `dune` et `make` sont indispensables.
  - La bibliothèque `Graphics` si elle ne vient pas déjà avec OCaml.  

## Compilation et lancement

Par défaut, `make` est seulement utilisé pour abréger les commandes `dune` (voir `Makefile` pour plus de détails) :

- `make` sans argument lancera la compilation `dune` de `main.exe`, c'est-à-dire votre programme en code natif.

- `make byte` compilera si besoin en bytecode, utile pour faire tourner votre code dans un toplevel OCaml, voir `lsystems.top`.

- `make clean` pour effacer le répertoire provisoire `_build` produit par `dune` lors de ses compilations.

Enfin pour lancer le programme: `./run` suffit.  
Vous pouvez également spécifiez un fichier L-système et/ou un nombre spécifique d'itérations via la l'option `-c`.  
Par exmple : `./run -c examples/snow.sys 5`  

## Fichiers L-systèmes

Pour que votre fichier L-système marche correctement avec notre programme il vous suffit de respecter les codes d'écritures présents dans les fichiers `.sys` du dossier `examples/`, à savoir :  

- Les lignes commençant par # sont des lignes de commentaires, donc ignorées par le programme.  

- Le fichier contient 3 "bloques de données" séparés par des lignes vides ou retour à la ligne (`\n`).  

- Le premier bloque correspond à l'axiome initial du L-système.  

- Le second bloque comprend une ligne par règle de transformations permettant d'agrandir l'axiome au fil des itérations. Si une variable ne possède pas une ligne de ce bloque le programme ne la transformera pas lors des itérations.  

- Le dernier bloque est celui des interprétations en commandes tortues pour chaque variable. Il est important ici que chaque variables présentes dans l'axiome y possède une ligne dédiée.  

## Test du programme

Pour certains L-Systems, l'animation de la tortue rend le programme un peu lent, mais nous n'avons pas eu le temps d'ajouter une option en ligne de commande pour la désactiver. Pour tester plusieurs L-Systems à la suite, vous pouvez commenter la ligne `138` du fichier turtle.ml qui indique `Unix.sleepf(time)` pour désactiver l'animation et gagner un peu de temps. La prochaine étape de programmation aurait été de rajouter des options en lignes de commandes afin de pouvoir permettre à l'utilisateur d'activer ou désactiver aisément couleurs et animations dans le programme.

