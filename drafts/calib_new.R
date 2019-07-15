## Script pour essayer de mettre en évidence k(t) avec I/N ou N/I 

# packages / data ---------------------------------------------------------

library(tidyverse)
library(magrittr)
library(mco)
source("../model_R/model_new.R")
source("../model_R/objectif.R")
source("../model_R/plot_res.R")

# Optimisation ------------------------------------------------------------

res1 <- nsga2(obj, 7, 3,
              lower.bounds = c(0, 0, 0, 0, 0, 10900, 1),
              upper.bounds = c(0.1, 1, 1, 1, 1, 20300, 10),
              popsize = 200, generations = 200)

res2 <- nsga2(obj, 7, 3,
              lower.bounds = c(0, 0, 0, 0, 0, 10900, 1),
              upper.bounds = c(0.1, 1, 1, 1, 1, 20300, 10),
              popsize = 200, generations = 200)

res3 <- nsga2(obj, 7, 3,
              lower.bounds = c(0, 0, 0, 0, 0, 10900, 1),
              upper.bounds = c(0.1, 1, 1, 1, 1, 20300, 10),
              popsize = 200, generations = 200)

res4 <- nsga2(obj0, 7, 3,
              lower.bounds = c(0, 0, 0, 0, 0, 10900, 1),
              upper.bounds = c(0.1, 1, 1, 1, 1, 20300, 10),
              popsize = 200, generations = 200)

res5 <- nsga2(obj0, 7, 3,
              lower.bounds = c(0, 0, 0, 0, 0, 10900, 1),
              upper.bounds = c(0.1, 1, 1, 1, 1, 20300, 10),
              popsize = 200, generations = 200)

res6 <- nsga2(obj0, 7, 3,
              lower.bounds = c(0, 0, 0, 0, 0, 10900, 1),
              upper.bounds = c(0.1, 1, 1, 1, 1, 20300, 10),
              popsize = 200, generations = 200)

res7 <- nsga2(obj0, 7, 3,
              lower.bounds = c(0, 0, 0, 0, 0, 10900, 1),
              upper.bounds = c(0.1, 1, 1, 1, 1, 20300, 10),
              popsize = 200, generations = 400)

# toto <- optim(c(0.6, 0.7, 1, 0.8, 15000, 140, rep(1, 240)), 
#               obj_ressources0, method = "L-BFGS-B", lower = 0, upper = 1)

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

ind4 <- res4$value %>%
    as_tibble %>% 
    mutate(norm = abs(V1) + abs(V2) + abs(V3)) %$%
    which.min(norm)

arg4 <- res4$par[ind4, ]

ind5 <- res5$value %>%
    as_tibble %>% 
    mutate(norm = abs(V1) + abs(V2) + abs(V3)) %$%
    which.min(norm)

arg5 <- res5$par[ind5, ]

ind6 <- res6$value %>%
    as_tibble %>% 
    mutate(norm = abs(V1) + abs(V2) + abs(V3)) %$%
    which.min(norm)

arg6 <- res6$par[ind6, ]

ind7 <- res7$value %>%
    as_tibble %>% 
    mutate(norm = abs(V1) + abs(V2) + abs(V3)) %$%
    which.min(norm)

arg7 <- res7$par[ind7, ]

ind100 <- res100$value %>%
    as_tibble %>% 
    mutate(norm = abs(V1) + abs(V2) + abs(V3)) %$%
    which.min(norm)

arg100 <- res100$par[ind100, ]

# plots -------------------------------------------------------------------

plot_decompo(arg1, inflos_simulated)
plot_decompo(arg2, inflos_simulated)
plot_decompo(arg3, inflos_simulated)
plot_decompo(arg4, inflos_simulated)
plot_decompo(arg5, inflos_simulated)
plot_decompo(arg6, inflos_simulated)
plot_decompo(arg7, inflos_simulated)

argument <- c(0.024, 1, 0.950, 0.041, 0.081, 20064, 9.983)
plot_decompo(argument, inflos_simulated)



# 100 jours ---------------------------------------------------------------


res100 <- nsga2(obj100, 7, 3,
              lower.bounds = c(0, 0, 0, 0, 0, 10900, 1),
              upper.bounds = c(0.1, 1, 1, 1, 1, 20300, 10),
              popsize = 200, generations = 200)

