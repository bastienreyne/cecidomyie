## Script qui effectue une régression PLS2 des solutions du modèle A afin de trouver les 3 scénarios envisagés

# Packages / data ---------------------------------------------------------

library(tidyverse)
library(pls)
load("/home/bastien/cecidomyie/model_R/calibration_A.Rdata")

# Allons-y ! --------------------------------------------------------------

X <- scale(params_front_A)
Y <- scale(pareto_front_A)

mod <- plsr(Y ~ X, validation = "CV")

# Choix du nombre de composantes ------------------------------------------

## On calcule l'erreur quadratique moyenne pour chacun des critères en fonction du nombre de composantes
eqm <- rbind(colMeans((Y[, ] - mod$validation$pred[, , 1])^2),
             colMeans((Y[, ] - mod$validation$pred[, , 2])^2),
             colMeans((Y[, ] - mod$validation$pred[, , 3])^2),
             colMeans((Y[, ] - mod$validation$pred[, , 4])^2),
             colMeans((Y[, ] - mod$validation$pred[, , 5])^2),
             colMeans((Y[, ] - mod$validation$pred[, , 6])^2),
             colMeans((Y[, ] - mod$validation$pred[, , 7])^2))

eqm %>%
    as.data.frame() %>% 
    gather(Y1, Y2, Y3, key = critere, value = EQM) %>%
    ggplot +
    aes(x = rep(1:7, 3), y = EQM) +
    geom_point() +
    geom_line() + 
    theme_bw() +
    facet_grid(. ~ critere) +
    xlab("Nombre de composantes")
## On retiendra 4 composantes, ce qui amène l'EQM de Y3 à 20%

# Graphiques des 2 premiers plan factoriels -------------------------------

