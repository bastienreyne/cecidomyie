## Script pour essayer de mettre en évidence k(t) avec I/N ou N/I 

# packages / data ---------------------------------------------------------

library(tidyverse)
library(magrittr)
library(mco)
source("../model_R/model_new.R")
source("../model_R/objectif.R")

# Optimisation ------------------------------------------------------------

res1 <- nsga2(obj_ressources, 247, 3,
              lower.bounds = c(0, 0, 0, 0, 10900, 1, rep(0, 240)),
              upper.bounds = c(0.1, 1, 1, 1, 20300, 10, rep(1, 240)),
              popsize = 200, generations = 10000)
        
# res2 <- nsga2(obj_ressources, 245, 3,
#               lower.bounds = c(0, 0, 0, 0, 3700, rep(0, 240)),
#               upper.bounds = c(0.1, 1, 1, 1, 6700, rep(100, 240)),
#               popsize = 200, generations = 1500)                              
# 
# res3 <- nsga2(obj_ressources, 245, 3,
#               lower.bounds = c(0, 0, 0, 0, 3700, rep(0, 240)),
#               upper.bounds = c(0.1, 1, 1, 1, 6700, rep(100, 240)),
#                  popsize = 200, generations = 1500)
    

# Arguments ---------------------------------------------------------------

ind1 <- res1$value %>%
    as_tibble %>% 
    mutate(norm = abs(V1) + abs(V2) + abs(V3)) %$%
    which.min(norm)

            # ind2 <- res2$value %>%
            #     as_tibble %>% 
            #     mutate(norm = abs(V1) + abs(V2) + abs(V3)) %$%
            #     which.min(norm)
            # 
            # ind3 <- res3$value %>%
            #     as_tibble %>% 
            #     mutate(norm = abs(V1) + abs(V2) + abs(V3)) %$%
            #     which.min(norm)

arg1 <- res1$par[ind1, ]
# arg2 <- res1$par[ind2, ]
# arg3 <- res1$par[ind3, ]

# plots -------------------------------------------------------------------

obs <- larves
est <- dynamics_ressources(arg1, inflos_simulated)

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

# test sur les k(t) -------------------------------------------------------

k_er <- arg1[8:87]
k_ps <- arg1[88:167]
k_eh <- arg1[168:247]

N <- femelles_ressources(arg1, inflos_simulated)

IsurN <- inflos_simulated / N
NsurI <- N / inflos_simulated

plot(IsurN[, 1], k_er)
plot(IsurN[, 2], k_ps)
plot(IsurN[, 3], k_eh)

plot(NsurI[, 1], k_er)
plot(NsurI[, 2], k_ps)
plot(NsurI[, 3], k_eh)


p1 <- data.frame(kdet = k_er, ratio = NsurI[, 1]) %>% 
    ggplot +
    aes(x = ratio, y = kdet) +
    geom_point() +
    theme_bw() +
    ylab(expression(k[ER](t))) +
    xlab(expression(N/I))
p2 <- data.frame(kdet = k_ps, ratio = NsurI[, 2]) %>% 
    ggplot +
    aes(x = ratio, y = kdet) +
    geom_point() +
    theme_bw() +
    ylab(expression(k[PS](t))) +
    xlab(expression(N/I))
p3 <- data.frame(kdet = k_eh, ratio = NsurI[, 3]) %>% 
    ggplot +
    aes(x = ratio, y = kdet) +
    geom_point() +
    theme_bw() +
    ylab(expression(k[EH](t))) +
    xlab(expression(N/I))

gridExtra::grid.arrange(p1, p2, p3, ncol = 3)