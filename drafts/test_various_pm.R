## Script pour tester rapidement les nouveaux échanges


# packages / data ---------------------------------------------------------

library(tidyverse)
library(magrittr)
library(mco)
source("../model_R/model.R")
source("../model_R/objectif.R")
inflos_attractive <- read.csv("../data/attractive_simulated.csv")[3:5]

# Optimisation ------------------------------------------------------------

res_isa <- nsga2(obj_isa, 5, 3,
                 lower.bounds = c(0, 0, 0, 0, 1),
                 upper.bounds = c(1, 1, 1, 1, 70),
                 popsize = 300, generations = 200)

res_fred <- nsga2(obj_fred, 5, 3,
                  lower.bounds = c(0, 0, 0, 0, 1),
                  upper.bounds = c(1, 1, 1, 1, 70),
                  popsize = 300, generations = 200)

ind_isa <- res_isa$value %>% 
    as_tibble %>% 
    mutate(norm1 = abs(V1) + abs(V2) + abs(V3)) %$%
    which.min(norm1)

ind_fred <- res_fred$value %>% 
    as_tibble %>% 
    mutate(norm1 = abs(V1) + abs(V2) + abs(V3)) %$%
    which.min(norm1)

arg_isa <- res_isa$par[ind_isa, ]
arg_fred <- res_fred$par[ind_fred, ]

# Plots -------------------------------------------------------------------

estimation <- dynamics_isa(res_isa$par[285, ], inflos_simulated)
proportion <- decomposition_isa(res_isa$par[285, ], inflos_simulated)

ploter <- data.frame(Date = date2017,
                     Sol = factor("Enherbement ras", levels = c("Enherbement ras",
                                                                "Paillage synthétique",
                                                                "Enherbement haut")),
                     method = factor("Modification", levels = c("Référence", "Modification")),
                     Observation = larves1,
                     Estimation = estimation[, 1],
                     Endogène = proportion[[3]][, 1],
                     Side = proportion[[4]][, 1],
                     Exogène = proportion[[2]][, 1]) %>% 
    gather(Exogène, Side, Endogène, key = prov, value = prop, factor_key = TRUE)

plotps <- data.frame(Date = date2017,
                     Sol = factor("Paillage synthétique", levels = c("Enherbement ras",
                                                                     "Paillage synthétique",
                                                                     "Enherbement haut")),
                     method = factor("Modification", levels = c("Référence", "Modification")),
                     Observation = larves2,
                     Estimation = estimation[, 2],
                     Endogène = proportion[[3]][, 2],
                     Side = proportion[[4]][, 2],
                     Exogène = proportion[[2]][, 2]) %>% 
    gather(Exogène, Side, Endogène, key = prov, value = prop, factor_key = TRUE)

ploteh <- data.frame(Date = date2017,
                     Sol = factor("Enherbement haut", levels = c("Enherbement ras",
                                                                 "Paillage synthétique",
                                                                 "Enherbement haut")),
                     method = factor("Modification", levels = c("Référence", "Modification")),
                     Observation = larves3,
                     Estimation = estimation[, 3],
                     Endogène = proportion[[3]][, 3],
                     Side = proportion[[4]][, 3],
                     Exogène = proportion[[2]][, 3]) %>% 
    gather(Exogène, Side, Endogène, key = prov, value = prop, factor_key = TRUE)

    
to_plot <- bind_rows(ploter, plotps, ploteh)
to_plot %>% ggplot +
    aes(x = Date) +
    geom_area(aes(y = prop, fill = prov), alpha = 0.5) +
    geom_line(aes(y = Observation, color = "Observation"), lwd = 0.75) +
    geom_line(aes(y = Estimation, color = "Estimation"), lwd = 0.75) +
    geom_point(aes(y = Observation, color = "Observation")) +
    geom_point(aes(y = Estimation, color = "Estimation")) +
    theme_bw() +
    facet_grid(. ~ Sol) +
    scale_color_manual(values = c("green4", "black")) +
    theme(legend.title = element_blank(), legend.position = "bottom") +
    ylab("Nombre de larves s'éjectant")
