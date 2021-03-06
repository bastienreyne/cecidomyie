## Script contenat les différentes fonctions objectifs

library(hydroGOF) ## Fonction NRMSE
library(tidyverse) ## Fonction %>%
source("/home/bastien/cecidomyie/model_R/model_new.R") ## Chargement modele

## Chargement date 2017
load("/home/bastien/cecidomyie/data/date2017.Rdata")
laps <- c(7, 7, 7, 8, 2, 5, 2, 4, 3, 5, 2, 5, 2, 5, 2, 4, 3, 4, 3)
true_index <- which(date2017 %in% true_date2017)

## Chargement inflos
inflosCDE <- as.matrix(read.csv(file = "/home/bastien/cecidomyie/data/2017_inflosCDE_bloc1.csv",
                                row.names = "id"))


## Chargement larves observees  
data_piege <- read.csv("/home/bastien/cecidomyie/data/2017_piege.csv")
larves1 <- data_piege %>% filter(Sol == "ER") %>% pull(larves)
larves2 <- data_piege %>% filter(Sol == "PS") %>% pull(larves)
larves3 <- data_piege %>% filter(Sol == "EH") %>% pull(larves)
larves <- cbind(larves1, larves2, larves3)

## Chargement inflos vivantes
inflos1 <- data_piege %>% filter(Sol == "ER") %>% pull(inflos)
inflos2 <- data_piege %>% filter(Sol == "PS") %>% pull(inflos)
inflos3 <- data_piege %>% filter(Sol == "EH") %>% pull(inflos)
inflos <- cbind(inflos1, inflos2, inflos3)

obj_A <- function(x) {
  ## ER, PS et EH
  ## Modèle A
  larves_estimees <- dynamics_A(x, inflos)
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

obj_B <- function(x) {
  ## ER, PS et EH
  ## Modèle B
  larves_estimees <- dynamics_B(x, inflos)
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

obj_B2 <- function(x) {
  ## ER, PS et EH
  ## Modèle B2
  larves_estimees <- dynamics_B2(x, inflos)
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

obj <- function(x) {
  ## ER, PS et EH
  ## Modèle C
  larves_estimees <- dynamics(x, inflosCDE)
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

obj0_season <- function(x) {
  ## ER, PS et EH
  ## Modèle D
  larves_estimees <- dynamics_season_b1(x, inflosCDE)
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


obj0_season_inflos <- function(x) {
  ## ER, PS et EH
  ## Modèle saisonnalité sur inflos
  larves_estimees <- dynamics_season_inflos_b1(x, inflosCDE)
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


obj_C <- function(x) {
  ## ER, PS et EH
  ## Modèle C avec calibration durée attra
  # larves_estimees <- dynamics_C(x, inflosCDE)
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
