## Script pour tester la mise en place de la diapause

# packages / data ---------------------------------------------------------

library(tidyverse)
library(magrittr)
library(mco)
source("../model_R/model_diapause.R")
source("../model_R/objectif.R")

# Optimisation ------------------------------------------------------------

res1 <- nsga2(obj_diap0, 6, 3,
              lower.bounds = c(0, 0, 0, 0, 0, 3700),
              upper.bounds = c(0.1, 1, 1, 1, 70, 6700),
              popsize = 200, generations = 200)

res2 <- nsga2(obj_diap0, 6, 3,
              lower.bounds = c(0, 0, 0, 0, 0, 3700),
              upper.bounds = c(0.1, 1, 1, 1, 70, 6700),
              popsize = 200, generations = 200)

res3 <- nsga2(obj_diap0, 6, 3,
              lower.bounds = c(0, 0, 0, 0, 0, 3700),
              upper.bounds = c(0.1, 1, 1, 1, 70, 6700),
              popsize = 200, generations = 200)


# Arguments ---------------------------------------------------------------

ind1 <- res1$value %>%
    as_tibble %>% 
    mutate(norm = abs(V1) + abs(V2) + abs(V3)) %$%
    which.min(norm)

ind2 <- res2$value %>%
    as_tibble %>% 
    mutate(norm = abs(V1) + abs(V2) + abs(V3)) %$%
    which.min(norm)

ind3 <- res3$value %>%
    as_tibble %>% 
    mutate(norm = abs(V1) + abs(V2) + abs(V3)) %$%
    which.min(norm)

arg1 <- res1$par[ind1, ]
arg2 <- res1$par[ind2, ]
arg3 <- res1$par[ind3, ]


# Plots -------------------------------------------------------------------

obs <- larves
est <- dynamics_diap(c(0.048, 0.986, 0.815, 0.167, 0.150, 6147), inflos_simulated)

er <- data.frame(date = date2017, obs = obs[, 1], est = est[, 1]) %>%
    mutate(Sol = factor("ER", levels = c("ER", "PS", "EH"))) %>% 
    gather(obs, est, key = statut, value = nombre, factor_key = TRUE)

ps <- data.frame(date = date2017, obs = obs[, 2], est = est[, 2]) %>%
    mutate(Sol = factor("PS", levels = c("ER", "PS", "EH"))) %>% 
    gather(obs, est, key = statut, value = nombre, factor_key = TRUE)

eh <- data.frame(date = date2017, obs = obs[, 3], est = est[, 3]) %>%
    mutate(Sol = factor("EH", levels = c("ER", "PS", "EH"))) %>% 
    gather(obs, est, key = statut, value = nombre, factor_key = TRUE)

to_plot <- bind_rows(er, ps, eh)
to_plot %>% ggplot +
    aes(x = date, y = nombre, color = statut) +
    geom_point() +
    geom_line() +
    facet_grid(. ~ Sol) +
    theme_bw() +
    theme(legend.title = element_blank()) +
    scale_color_discrete(labels = c("Observation", "Estimation")) +
    xlab("Date") +
    ylab("Nombre de larves")
