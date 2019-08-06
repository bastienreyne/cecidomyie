## Fichiers de données

Ce dossier regroupe les données relatives au projet :

### Nature des fichiers

Dans le dossier **raw** se trouvent les données brutes. 
Les données du fichier *floraison0.xlsx* (**dataset 1**) recense, pour un certain nombre d'unités de croissances, les dates de naissances et de morts des inflorescences existantes
Les données du fichier *piege0.xlsx* (**dataset 2**) dénombrent le nombre de larves piégées par un certain nombre d'inflorescences. Cela permettra d'avoir une idée du nombre de cécidomyies dans le verger.

Ensuite, parmi les **scripts R**, il y a :

+ *data_piege.R* qui s'occupe de mettre en forme le dataset 2 (pour les 2 vergers).
+ *flo_b1 / b2.R* s'occupent de mettre en forme le dataset 1.
+ *correction_bloc1 / bloc2.R* corrigent les dynamiques d'inflos du dataset 1 et effectuent la mise à l'échelle du verger.
+ *inflosCDE_bloc1 / bloc2.R* simulent les dynamiques d'inflorescences aux stades C/D/E.

Parmi les **fichiers csv**, on peut trouver :

+ *2017_piege(_bloc2).csv* qui contient les données du dataset 2 (inflorescences vivantes et larves pour chacune des trois modalités de couverture du sol).
+ *2017_inflos_dataset1_bloc1/2.csv* qui contient les dynamiques d'inflorescences vivantes issues du dataset 1.
+ *2017_bursts_death_bloc1/2.csv* qui contient le nombre de débourrements et de morts quotidien observés sur le dataset 1 pour chacune des trois modalités.
+ *2017_inflos1_echelle_dataset1_bloc1/2.csv* qui contient les dynmaiques d'inflorescences vivantes du dataset 1 non corrigées et mises à l'échelle du verger.
+ *2017_inflos_corrected_bloc1/2.csv* qui contient les dynamiques d'inflorescences vivantes issues du dataset 1 corrigées et mises à l'échelle du verger.
+ *2017_bursts_echelle_bloc1/2.csv* qui contient le nombre de débourrements quotidien mis à l'échelle du verger.
+ *2017_inflosCDE_bloc1/2.csv* qui contient les dynamiques (simulées) d'inflorescences aux stades C/D/E.

Parmi les fichiers **Rdata**, il y a :

+ *date2017.Rdata* qui contient les dates issues du dataset 2 (ainsi que les dates de relevés).
+ *date_floraison2017.Rdata* qui contient les dates issues du dataset 1.
