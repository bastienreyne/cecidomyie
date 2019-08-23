## Implémentation du modèle en R

Le modèle est implémenté dans le fichier *model1_new.R*.

Différentes fonction objectif sont disponibles dans le fichier *objectif.R*.

Le script *plot_res.R* permet l'affichage des résultats.

Les scripts *optimisations_x.R* sont utilisés pour calibrer les paramètres.
Ils produisent les fichiers *calibration_x.Rdata*.
Les solutions produites sont explorées dans les fichiers *exploration_x.R*.
La lettre *x* correspond au modèle testé :
+ modèle A : inflorescences vivantes, probabilité de pupaison constante (0.77), pas de saisonnalité.
+ modèle B : inflorescences vivantes, pupaison en fonction de la température, pas de saisonnalité.
+ modèle C : inflorescences C/D/E, pupaison en fonction de la température, pas de saisonnalité.
+ modèle D : inflorescences C/D/E, pupaison en fonction de la température, paramètre de saisonnalité sur les femelles.
