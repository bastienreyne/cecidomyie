## Script qui effectue des simulations in silico.

# Packages / data ---------------------------------------------------------

library(tidyverse)
source("/home/bastien/cecidomyie/model_R/model_new.R")
source("/home/bastien/cecidomyie/model_R/plot_res.R")

# Créations de dynamiques d'inflos ----------------------------------------

## Nombre d'inflos vivantes observées moyen pour chacun des 3 sous-blocs.
n_inflos <- sum(inflos_obs) / 3

## Durée entre deux flushs supérieurs au cycle de développement de la cécidomyie 
duree_inter_flush <- 18

## Durée saison : 80 jours
jours <- 1:80
