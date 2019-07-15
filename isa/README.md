# Petites explications

Je décris radpidement la liste des données et scripts nécessaires à une calibration «de base».

## Liste des fichiers

### Les données

Les données utilisées par le modèle :

+ le fichier *p_pup15j* contient le vecteur *p_pup15j* qui correspond à la probabilité de pupaison en fonction de la température (sur 15 jours) pour chacun des 80 jours de l'année 2017 ;
+ le fichier *sortie_diapause2017.Rdata* contient le vecteur *sortie_diapause20117* qui contient la proportion de diapausante (du stock) qui sort chaque jour pour les 80 jours de l'année 2017.

Les données utilisées par la fonction objectif :

+ le fichier *date2017.Rdata* contient les 80 dates de l'année 2017 dans le vecteur *date2017* et les dates de relevés dans le vecteur *true_date2017* ;
+ le fichier *attrative_simulated.csv* contenant les inflorescences attractives simulées à partie de l'algo de Fred ;
+ le fichier *inflosCDE.Rdata* contient les inflorescences C/D/E (16 jours) : la matrice *inflosCDE* contient les inflos C/D/E calculées à partir des débourrements observés pour les sous-blocs ER et PS et des débourrements simulés pour le sous-bloc EH ; et la matrice *inflosCDEsim* contient les inflos C/D/E calculées à partir des débourrements observés pour les trois sous-blocs ;
+ le fichier *2017_piege.csv* contient (notamment) les larves observés.

### Le modèle

Le modèle actuel se trouve dans le fichier *model_new.R*.

### La fonction objectif

La fonction objectif se trouve dans le script *objectif.R*

### L'analyse de sensibilité



### La calibration



### Affichage des résultats

Le script *plot_res.R* permet d'afficher les résultats.
Il contient deux fonctions : *plot_dynamics* qui trace les dynamiques de larves observées et simulées ; *plot_decompo* trace les dynamiques de larves observées et les dynamiques de larves simulées décomposées.

## Pour lancer des calibrations


