## Script modification processus

# Packages / data ---------------------------------------------------------

library(tidyverse)
library(magrittr)
library(mco)
source("../model_R/model.R")
load("../data/date2017.Rdata")
laps <- c(7, 7, 7, 8, 2, 5, 2, 4, 3, 5, 2, 5, 2, 5, 2, 4, 3, 4, 3)
true_index <- which(date2017 %in% true_date2017)
inflos_simulated <- as.matrix(read.csv("../data/attractive_simulated.csv")[, 3:5])
data_piege <- read.csv("../data/2017_piege.csv")
larves1 <- data_piege %>% filter(Sol == "ER") %>% pull(larves)
larves2 <- data_piege %>% filter(Sol == "PS") %>% pull(larves)
larves3 <- data_piege %>% filter(Sol == "EH") %>% pull(larves)
larves <- cbind(larves1, larves2, larves3)


my_mae <- function(x, y) {
    n_obs <- length(x)
    sum(abs(x - y)) / n_obs
}

min_ecart_max <- function(x, y) {
    max(abs(x-y))
}

obj <- function(x) {
    larves_estimees <- dynamics2(x, inflos_simulated)
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
    
    c(my_mae(larves_est[, 1], larves_observed[, 1]),
      my_mae(larves_est[, 2], larves_observed[, 2]),
      my_mae(larves_est[, 3], larves_observed[, 3]))
}

set.seed(1848)
res_ref <- nsga2(obj, 6, 3,
                 lower.bounds = c(0, 0, 0, 0, 1, 0),
                 upper.bounds = c(1, 1, 1, 1, 70, 1),
                 popsize = 200, generations = 200)

indref <- res_ref$value %>% as_tibble %>%
    mutate(norm = abs(V1 + V2 + V3)) %$%
    which.min(norm)

argref <- res_ref$par[indref, ]

# Plots -------------------------------------------------------------------

estimation <- dynamics2(argref, inflos_simulated)
proportion <- decomposition2(argref, inflos_simulated)

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
    gather(Endogène, Side, Exogène, key = prov, value = prop, factor_key = TRUE)

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
    gather(Endogène, Side, Exogène, key = prov, value = prop, factor_key = TRUE)

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
    gather(Endogène, Side, Exogène, key = prov, value = prop, factor_key = TRUE)


to_plot <- bind_rows(ploter, plotps, ploteh, ploterref, plotpsref, plotehref)
to_plot %>% ggplot +
    aes(x = Date) +
    geom_area(aes(y = prop, fill = prov), alpha = 0.5) +
    geom_line(aes(y = Observation, color = "Observation"), lwd = 0.75) +
    geom_line(aes(y = Estimation, color = "Estimation"), lwd = 0.75) +
    geom_point(aes(y = Observation, color = "Observation")) +
    geom_point(aes(y = Estimation, color = "Estimation")) +
    theme_bw() +
    facet_grid(method ~ Sol) +
    scale_color_manual(values = c("green4", "black")) +
    theme(legend.title = element_blank(), legend.position = "bottom") +
    ylab("Nombre de larves s'éjectant")
