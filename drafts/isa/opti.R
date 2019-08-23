## Script qui réalise la calibration

# packages / data ---------------------------------------------------------

library(tidyverse)
library(magrittr)
library(mco)
library(emoa)
source("model_new.R")
source("objectif.R")
source("plot_res.R")

# Calibration -------------------------------------------------------------

n_repet <- 2
taille_pop <- 200
nb_gen <- 200

binf <- c(0, 0, 0, 0, 1, 10900, 1)
bsup <- c(0.1, 1, 1, 1, 100, 20300, 11)

front <- matrix(NA, nrow = n_repet * taille_pop, ncol = 3)
parameters <- matrix(NA, nrow = n_repet * taille_pop, ncol = 7)

for (iter in 1:n_repet) {
    tmp <- nsga2(objCDE, 7, 3,
                 lower.bounds = binf,
                 upper.bounds = bsup,
                 popsize = taille_pop, generations = nb_gen)
    front[((iter - 1) * taille_pop + 1):((iter - 1) * taille_pop + taille_pop), ] <- 
        tmp$value
    parameters[((iter - 1) * taille_pop + 1):((iter - 1) * taille_pop + taille_pop), ] <- tmp$par
}

# Pareto front ------------------------------------------------------------

## Indices correspondant aux résultat sur le front de Pareto
ind_pareto <- !is_dominated(t(front))

pareto_front <- front[ind_pareto, ]
pareto_parameters <- parameters[ind_pareto, ]


# Sélection solution min norme 1 ------------------------------------------

## Là je prends la solution qui minimise la norme 1
ind1 <- pareto_front %>% 
    as.data.frame() %>% 
    mutate(norm1 = abs(V1) + abs(V2) + abs(V3)) %$%
    which.min(norm1)

arg1 <- pareto_parameters[ind1, ]


# Affichage résultats -----------------------------------------------------

plot_decompo(arg1, inflosCDE)

## Pour tester d'autres solutions, changer le 12 par un autre indice
plot_decompo(pareto_parameters[12, ], inflosCDE)
