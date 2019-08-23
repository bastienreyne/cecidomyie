# Petites explications

Je décris radpidement la liste des données et scripts nécessaires à une calibration «de base».

## Liste des fichiers

### Les données

Les données utilisées par le modèle :

+ le fichier *p_pup15j* contient le vecteur *p_pup15j* qui correspond à la probabilité de pupaison en fonction de la température (sur 15 jours) pour chacun des 80 jours de l'année 2017 ;
+ le fichier *sortie_diapause2017.Rdata* contient le vecteur *sortie_diapause2017* qui contient la proportion de diapausante (du stock) qui sort chaque jour pour les 80 jours de l'année 2017.

Les données utilisées par la fonction objectif :

+ le fichier *date2017.Rdata* contient les 80 dates de l'année 2017 dans le vecteur *date2017* et les dates de relevés dans le vecteur *true_date2017* ;
+ le fichier *attrative_simulated.csv* contenant les inflorescences attractives simulées à partie de l'algo de Fred ;
+ le fichier *inflosCDE.Rdata* contient les inflorescences C/D/E (16 jours) : la matrice *inflosCDE* contient les inflos C/D/E calculées à partir des débourrements observés pour les sous-blocs ER et PS et des débourrements simulés pour le sous-bloc EH ; et la matrice *inflosCDEsim* contient les inflos C/D/E calculées à partir des débourrements simulés pour les trois sous-blocs ;
+ le fichier *2017_piege.csv* contient (notamment) les larves observées.

### Le modèle

Le modèle actuel se trouve dans le fichier *model_new.R*.

### La fonction objectif

La fonction objectif se trouve dans le script *objectif.R*


### La calibration

Le script pour la calibration se nomme *opti.R*.

### Affichage des résultats

Le script *plot_res.R* permet d'afficher les résultats.
Il contient deux fonctions : *plot_dynamics* qui trace les dynamiques de larves observées et simulées ; *plot_decompo* trace les dynamiques de larves observées et les dynamiques de larves simulées décomposées.

## Pour lancer des calibrations

Pour calibrer les paramètres du modèle, il faut ouvrir le script *opti.R* et le lancer.
Des réglages sont possibles, notamment pour le nombre de répétitions de NSGA-II et de la taille de la population.
Le nombre de répétitions de NSGA-II est contrôlé par la variable *n_iter*.
La taille de la population est contrôlée par la variable *taille_pop*.
Le nombre de générations de NSGA-II est géré par la variable *n_gen*.
Pour donner une idée, une exécution avec une taille de populaion de 200 prends ~5/7 minutes sur mon ordinateur.

L'ensemble des solutions non-dominées se trouve dans la matrice *parameters*. Par défaut, je sélectionne la solution qui minimise la norme 1 dans le vecteur *arg1*.
La commande *plot_decompo(arg1, inflosCDE)* permet d'afficher le résultat produit.
La commande *plot_decompo(parameters[i, ], inflosCDE)* permet d'afficher la solution produite par la *i*^{ème} solution.
