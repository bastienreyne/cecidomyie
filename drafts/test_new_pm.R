## Script illustrant la différence entre le nouveau p_m et l'ancien

# Packages / data ---------------------------------------------------------

library(tidyverse)
library(magrittr)
library(mco)
library(gridExtra)
source("../model_R/model.R")
donnees <- read.csv("../data/2017_piege.csv")

inflos_ER <- donnees %>% filter(Sol == "ER") %>% pull(inflos)
inflos_PS <- donnees %>% filter(Sol == "PS") %>% pull(inflos)
inflos_EH <- donnees %>% filter(Sol == "EH") %>% pull(inflos)
inflos <- cbind(inflos_ER, inflos_PS, inflos_EH)

larves_ER <- donnees %>% filter(Sol == "ER") %>% pull(larves)
larves_PS <- donnees %>% filter(Sol == "PS") %>% pull(larves)
larves_EH <- donnees %>% filter(Sol == "EH") %>% pull(larves)
larves <- cbind(larves_ER, larves_PS, larves_EH)

load("../data/date2017.Rdata")
laps <- c(7, 7, 7, 8, 2, 5, 2, 4, 3, 5, 2, 5, 2, 5, 2, 4, 3, 4, 3)
true_index <- which(date2017 %in% true_date2017)
# Optimisation ------------------------------------------------------------

my_mae <- function(x, y) {
    n_obs <- length(x)
    sum(abs(x - y)) / n_obs
}

obj1 <- function(x, inflos) {
    larves_estimees <- dynamics2(x, inflos)
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

obj2 <- function(x, inflos) {
    larves_estimees <- dynamics4(x, inflos)
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

obj3 <- function(x, inflos) {
  larves_estimees <- dynamics2(x, inflos)
  larvesER <- larves_estimees[, 1]
  larvesPS <- larves_estimees[, 2]
  larvesEH <- larves_estimees[, 3]


  c(my_mae(larvesER, larves_ER),
    my_mae(larvesPS, larves_PS),
    my_mae(larvesEH, larves_EH))
}

obj4 <- function(x, inflos) {
  larves_estimees <- dynamics4(x, inflos)
  larvesER <- larves_estimees[, 1]
  larvesPS <- larves_estimees[, 2]
  larvesEH <- larves_estimees[, 3]


  c(my_mae(larvesER, larves_ER),
    my_mae(larvesPS, larves_PS),
    my_mae(larvesEH, larves_EH))
}

res1 <- nsga2(obj1, 5, 3, inflos,
              lower.bounds = c(0, 0, 0, 0, 1),
              upper.bounds = c(1, 1, 1, 1, 70),
              popsize = 200, generations = 100)

res2 <- nsga2(obj2, 5, 3, inflos,
              lower.bounds = rep(0, 5),
              upper.bounds = c(1, 1, 1, 1, 70),
              popsize = 200, generations = 100)

res3 <- nsga2(obj3, 5, 3, inflos,
              lower.bounds = c(0, 0, 0, 0, 1),
              upper.bounds = c(1, 1, 1, 1, 70),
              popsize = 200, generations = 100)

res4 <- nsga2(obj4, 5, 3, inflos,
              lower.bounds = rep(0, 5),
              upper.bounds = c(1, 1, 1, 1, 70),
              popsize = 200, generations = 100)

ind1 <- res1$value %>% as_tibble %>%
    mutate(norm = abs(V1 + V2 + V3)) %$%
    which.min(norm)

arg1 <- res1$par[ind1, ]

ind2 <- res2$value %>% as_tibble %>%
    mutate(norm = abs(V1 + V2 + V3)) %$%
    which.min(norm)

arg2 <- res2$par[ind2, ]

ind3 <- res3$value %>% as_tibble %>%
  mutate(norm = abs(V1 + V2 + V3)) %$%
  which.min(norm)

arg3 <- res3$par[ind3, ]

ind4 <- res4$value %>% as_tibble %>%
  mutate(norm = abs(V1 + V2 + V3)) %$%
  which.min(norm)

arg4 <- res4$par[ind4, ]


# Plots -------------------------------------------------------------------
  
ref <- dynamics2(arg1, inflos)
new <- dynamics4(arg2, inflos)
ref2 <- dynamics2(arg3, inflos)
new2 <- dynamics4(arg4, inflos)

df_er <- data.frame(Date = date2017,
                    Observés = larves_ER,
                    Référence = ref[, 1],
                    Nouveau_pm = ref2[, 1])

er2 <- df_er %>% ggplot +
    aes(x = Date) +
    geom_line(aes(y = Observés), col = "black") +
    geom_line(aes(y = Référence), col = "blue") +
    geom_line(aes(y = Nouveau_pm), col = "green4") +
    geom_point(aes(y = Observés), col = "black") +
    geom_point(aes(y = Référence), col = "blue") +
    geom_point(aes(y = Nouveau_pm), col = "green4") +
    theme_bw()

df_ps <- data.frame(Date = date2017,
                    Observés = larves_PS,
                    Référence = ref[, 2],
                    Nouveau_pm = ref2[, 2])

ps2 <- df_ps %>% ggplot +
    aes(x = Date) +
    geom_line(aes(y = Observés), col = "black") +
    geom_line(aes(y = Référence), col = "blue") +
    geom_line(aes(y = Nouveau_pm), col = "green4") +
    geom_point(aes(y = Observés), col = "black") +
    geom_point(aes(y = Référence), col = "blue") +
    geom_point(aes(y = Nouveau_pm), col = "green4") +
    theme_bw()

df_eh <- data.frame(Date = date2017,
                    Observés = larves_EH,
                    Référence = ref[, 3],
                    Nouveau_pm = ref2[, 3])

eh2 <- df_eh %>% ggplot +
    aes(x = Date) +
    geom_line(aes(y = Observés), col = "black") +
    geom_line(aes(y = Référence), col = "blue") +
    geom_line(aes(y = Nouveau_pm), col = "green4") +
    geom_point(aes(y = Observés), col = "black") +
    geom_point(aes(y = Référence), col = "blue") +
    geom_point(aes(y = Nouveau_pm), col = "green4") +
    theme_bw()


