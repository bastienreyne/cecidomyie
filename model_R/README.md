# Implémentation du modèle en R

Le modèle est implémenté dans le fichier *model1_new.R*.

Différentes fonction objectif sont disponibles dans le fichier *objectif.R*.

Le script *plot_res.R* permet l'affichage des résultats.

Les scripts *optimisations_x.R* sont utilisés pour calibrer les paramètres.
Ils produisent les fichiers *calibration_x.Rdata*.
Les solutions produites sont explorées dans les fichiers *exploration_x.R*.
La lettre *x* correspond au modèle testé :

## Modèle A

Le modèle utilise en entrée les inflorescences vivantes.
La probabilité de pupaison est constante dans cette version du modèle (0.77).
Il n'y a pas de phénomène de saisonnalité.

Les **fonctions** spécifiques à ce modèle présentes dans *model_new.R* sont :
+ **emerging_A** : gère l'émergence des cécidomyies en utilisant la proba de pupaison fixé à 0.77
+ **dynamics_A** : renvoie la dynamiques de larves pour chacune des trois sous-parcelles
+ **decomposition_A** : renvoie les différentes dynamiques (larves, femelles, femelles exogènes...)

Pour effectuer la **représentation graphique**, il y dans le fichier *plot_res.R* les fonctions :
+ **plot_dynamics_A** : trace les dynamiques de larves pour chacune des trois sous-parcelles (estimées et observées)
+ **plot_decompo_A** : trace les dynamiques des larves observées et estimées pour chacune des trois sous-parcelles. La provenance des femelles étant à l'origine des larves est disponible pour les dynamiques de larves estimées.

## Modèle B

Le modèle utilise en entrée les inflorescences vivantes.
La probabilité de pupaison est dépend de la température.
Il n'y a pas de phénomène de saisonnalité.

Les **fonctions** spécifiques à ce modèle présentes dans *model_new.R* sont :
+ **emerging_B** : gère l'émergence des cécidomyies en utilisant une proba de pupaison dépendant de la tepérature.
+ **dynamics_B** : renvoie la dynamiques de larves pour chacune des trois sous-parcelles
+ **decomposition_B** : renvoie les différentes dynamiques (larves, femelles, femelles exogènes...)

Pour effectuer la **représentation graphique**, il y dans le fichier *plot_res.R* la fonction :
+ **plot_decompo_B** : trace les dynamiques des larves observées et estimées pour chacune des trois sous-parcelles. La provenance des femelles étant à l'origine des larves est disponible pour les dynamiques de larves estimées.

### Modèle B2

C'est une variante du modèle B.
Le modèle est ici plus libre dans la manière de calibrer la probabilité de pupaison.

Les **fonctions** spécifiques à ce modèle présentes dans *model_new.R* sont :
+ **emerging_B2** : gère l'émergence des cécidomyies en utilisant une proba de pupaison dépendant de la tepérature.
+ **dynamics_B2** : renvoie la dynamiques de larves pour chacune des trois sous-parcelles
+ **decomposition_B2** : renvoie les différentes dynamiques (larves, femelles, femelles exogènes...)

Pour effectuer la **représentation graphique**, il y dans le fichier *plot_res.R* la fonction :
+ **plot_decompo_B2** : trace les dynamiques des larves observées et estimées pour chacune des trois sous-parcelles. La provenance des femelles étant à l'origine des larves est disponible pour les dynamiques de larves estimées.

## Modèle C

Le modèle utilise en entrée les inflorescences au stade phénologiques C, D et E.
La probabilité de pupaison est calculé par une régression linéaire en fonction de la température.
Il n'y a pas de phénomène de saisonnalité.

Les **fonctions** spécifiques à ce modèle présentes dans *model_new.R* sont :
+ **emerging** : gère l'émergence des cécidomyies en utilisant une proba de pupaison dépendant de la tepérature.
+ **dynamics** : renvoie la dynamiques de larves pour chacune des trois sous-parcelles
+ **decomposition** : renvoie les différentes dynamiques (larves, femelles, femelles exogènes...)

Pour effectuer la **représentation graphique**, il y dans le fichier *plot_res.R* la fonction :
+ **plot_decompo** : trace les dynamiques des larves observées et estimées pour chacune des trois sous-parcelles. La provenance des femelles étant à l'origine des larves est disponible pour les dynamiques de larves estimées.

### Modèle C2

Variante du modèle C.
On utilise pas ici les inflorescences aux stades C, D et E (durée d'attractivité de 16 jours), c'est le modèle qui choisit la durée d'attractivité.

Les **fonctions** spécifiques à ce modèle présentes dans *model_new.R* sont :
+ **emerging** : gère l'émergence des cécidomyies en utilisant une proba de pupaison dépendant de la tepérature.
+ **inflos_attractives** : renvoie les dynamiques d'inflorescences en fonction de la durée d'attractivité donnée.
+ **dynamics_C** : renvoie la dynamiques de larves pour chacune des trois sous-parcelles
+ **decomposition_C2** : renvoie les différentes dynamiques (larves, femelles, femelles exogènes...)

Pour effectuer la **représentation graphique**, il y dans le fichier *plot_res.R* la fonction :
+ **plot_decompo_C2** : trace les dynamiques des larves observées et estimées pour chacune des trois sous-parcelles. La provenance des femelles étant à l'origine des larves est disponible pour les dynamiques de larves estimées.

## Modèle D

Le modèle utilise en entrée les inflorescences au stade phénologiques C, D et E.
La probabilité de pupaison est calculé par une régression linéaire en fonction de la température.
Il y a un phénomène de saisonnalité sur la population de femelles.

Les **fonctions** spécifiques à ce modèle présentes dans *model_new.R* sont :
+ **emerging** : gère l'émergence des cécidomyies en utilisant une proba de pupaison dépendant de la tepérature.
+ **dynamics_season_b1** : renvoie la dynamiques de larves pour chacune des trois sous-parcelles du verger n°1
+ **dynamics_season_b2** : renvoie la dynamiques de larves pour chacune des trois sous-parcelles du verger n°2
+ **decomposition_season_b1** : renvoie les différentes dynamiques (larves, femelles, femelles exogènes...) pour le verger n°1
+ + **decomposition_season_b1** : renvoie les différentes dynamiques (larves, femelles, femelles exogènes...) pour le verger n°2

Pour effectuer la **représentation graphique**, il y dans le fichier *plot_res.R* la fonction :
+ **plot_decompo_season** : trace les dynamiques des larves observées et estimées pour chacune des trois sous-parcelles du verger n°1. La provenance des femelles étant à l'origine des larves est disponible pour les dynamiques de larves estimées.
+ **plot_decompo_season_b2** : trace les dynamiques des larves observées et estimées pour chacune des trois sous-parcelles du verger n°2. La provenance des femelles étant à l'origine des larves est disponible pour les dynamiques de larves estimées.









