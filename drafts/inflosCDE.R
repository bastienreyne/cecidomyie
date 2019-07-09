## Script pour ne prendre en compte que l'aspect stade phénos C/D/E des inflos.

# packages / data ---------------------------------------------------------

library(tidyverse)
library(magrittr)
library(mco)
library(hydroGOF)
source("/home/bastien/cecidomyie/model_R/model_diapause.R")
bursts <- read.csv("../data/2017_bursts_simulated.csv")[2:4]


# dynammiques inflos ------------------------------------------------------

inflosCDE_er <- rep(NA, 80)
inflosCDE_ps <- rep(NA, 80)
inflosCDE_eh <- rep(NA, 80)
for (t in 1:80) {
    if (t <= 16) {
        inflosCDE_er[t] <- sum(bursts[1:t, 1])
        inflosCDE_ps[t] <- sum(bursts[1:t, 2])
        inflosCDE_eh[t] <- sum(bursts[1:t, 3])
    } else {
        inflosCDE_er[t] <- sum(bursts[(t - 15):t, 1])
        inflosCDE_ps[t] <- sum(bursts[(t - 15):t, 2])
        inflosCDE_eh[t] <- sum(bursts[(t - 15):t, 3])
    }
}

inflosCDE <- cbind(inflosCDE_er, inflosCDE_ps, inflosCDE_eh)

# objectif ----------------------------------------------------------------

load("/home/bastien/cecidomyie/data/date2017.Rdata")
laps <- c(7, 7, 7, 8, 2, 5, 2, 4, 3, 5, 2, 5, 2, 5, 2, 4, 3, 4, 3)
true_index <- which(date2017 %in% true_date2017)
data_piege <- read.csv("/home/bastien/cecidomyie/data/2017_piege.csv")
larves1 <- data_piege %>% filter(Sol == "ER") %>% pull(larves)
larves2 <- data_piege %>% filter(Sol == "PS") %>% pull(larves)
larves3 <- data_piege %>% filter(Sol == "EH") %>% pull(larves)
larves <- cbind(larves1, larves2, larves3)

obj_diap <- function(x) {
    larves_estimees <- dynamics_diap(x, inflosCDE)
    larvesER <- larves_estimees[, 1]
    larvesPS <- larves_estimees[, 2]
    larvesEH <- larves_estimees[, 3]
    
    larves_est <- matrix(NA, nrow = length(laps), ncol = 3)
    for (i in 1:length(laps)) {
        indices <- (true_index[i] - laps[i] + 1):true_index[i]
        larves_est[i, ] <- c(mean(larvesER[indices]),
                             mean(larvesPS[indices]),
                             mean(larvesEH[indices]))
    }
    
    larves_observed <- larves[true_index, ]
    
    c(nrmse(larves_est[, 1], larves_observed[, 1], norm = "maxmin"),
      nrmse(larves_est[, 3], larves_observed[, 3], norm = "maxmin"),
      critere(x, inflos_simulated))
}


obj_diap0 <- function(x) {
    larves_estimees <- dynamics_diap(x, inflosCDE)
    larvesER <- larves_estimees[, 1]
    larvesPS <- larves_estimees[, 2]
    larvesEH <- larves_estimees[, 3]
    
    larves_est <- matrix(NA, nrow = length(laps), ncol = 3)
    for (i in 1:length(laps)) {
        indices <- (true_index[i] - laps[i] + 1):true_index[i]
        larves_est[i, ] <- c(mean(larvesER[indices]),
                             mean(larvesPS[indices]),
                             mean(larvesEH[indices]))
    }
    
    larves_observed <- larves[true_index, ]
    
    c(nrmse(larves_est[, 1], larves_observed[, 1], norm = "maxmin"),
      nrmse(larves_est[, 2], larves_observed[, 2], norm = "maxmin"),
      nrmse(larves_est[, 3], larves_observed[, 3], norm = "maxmin"))
}

# nsga2 -------------------------------------------------------------------

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
est <- dynamics_diap(c(0.02, 0.094, 0.989, 0.709, 0.232, 5708), inflosCDE)

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

# comp attractives simulated et cde ---------------------------------------

inflos_simulated <- as.matrix(read.csv("/home/bastien/cecidomyie/data/attractive_simulated.csv")[, 3:5])

to_plot <- rbind(inflos_simulated, inflosCDE) %>% 
    as.data.frame() %>% 
    mutate(statut = rep(c("Attractives", "C/D/E"), each = 80))
colnames(to_plot) <- c("ER", "PS", "EH", "statut")
to_plot %>% 
    gather(ER, PS, EH, key = Sol, value = nombre, factor_key = TRUE) %>% 
    ggplot +
    aes(x = rep(date2017, 6), y = nombre, color = statut) +
    geom_line(lwd = 0.75) +
    theme_bw() +
    facet_grid(. ~ Sol) +
    theme(legend.title = element_blank()) +
    xlab("Date") +
    ylab("Nombre d'inflorescences")
