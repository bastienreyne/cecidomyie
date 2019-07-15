## Script pour tester les inflos CDE

# packages / data ---------------------------------------------------------

library(tidyverse)
library(magrittr)
library(mco)
source("../model_R/model_new.R")
source("../model_R/objectif.R")
source("../model_R/plot_res.R")

# Calibration -------------------------------------------------------------

res1 <- nsga2(objCDE, 7, 3,
              lower.bounds = c(0, 0, 0, 0, 1, 10900, 1),
              upper.bounds = c(0.1, 1, 1, 1, 100, 20300, 11),
              popsize = 200, generations = 200)

res2 <- nsga2(objCDE, 7, 3,
              lower.bounds = c(0, 0, 0, 0, 1, 10900, 1),
              upper.bounds = c(0.1, 1, 1, 1, 100, 20300, 11),
              popsize = 200, generations = 200)

res3 <- nsga2(objCDE, 7, 3,
              lower.bounds = c(0, 0, 0, 0, 1, 10900, 1),
              upper.bounds = c(0.1, 1, 1, 1, 100, 20300, 11),
              popsize = 200, generations = 200)

# Arguments ---------------------------------------------------------------

ind1 <- res1$value %>%
    as_tibble %>% 
    mutate(norm = abs(V1) + abs(V2) + abs(V3)) %$%
    which.min(norm)

arg1 <- res1$par[ind1, ]

ind2 <- res2$value %>%
    as_tibble %>% 
    mutate(norm = abs(V1) + abs(V2) + abs(V3)) %$%
    which.min(norm)

arg2 <- res2$par[ind2, ]

ind3 <- res3$value %>%
    as_tibble %>% 
    mutate(norm = abs(V1) + abs(V2) + abs(V3)) %$%
    which.min(norm)

arg3 <- res3$par[ind3, ]

# plots -------------------------------------------------------------------

plot_decompo(arg1, inflosCDE)
plot_decompo(arg2, inflosCDE)
plot_decompo(arg3, inflosCDE)
