library(tidyverse)
library(hydroGOF)
source("/home/bastien/cecidomyie/model_R/model_new.R")
load("date2017.Rdata")
load("inflosCDE.Rdata")
laps <- c(7, 7, 7, 8, 2, 5, 2, 4, 3, 5, 2, 5, 2, 5, 2, 4, 3, 4, 3)
true_index <- which(date2017 %in% true_date2017)
inflos_simulated <- as.matrix(read.csv("attractive_simulated.csv")[, 3:5])
data_piege <- read.csv("2017_piege.csv")
larves <- cbind(data_piege %>% filter(Sol == "ER") %>% pull(larves),
                    data_piege %>% filter(Sol == "PS") %>% pull(larves),
                    data_piege %>% filter(Sol == "EH") %>% pull(larves))


obj <- function(x) {
  ## ER, EH et max endo
  ## Inflorescences «attractives»
  larves_estimees <- dynamics(x, inflos_simulated)
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


obj0 <- function(x) {
  ## ER, PS et EH
  ## Inflorescences «attractives»
  larves_estimees <- dynamics(x, inflos_simulated)
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


objPS <- function(x) {
  ## PS
  ## Inflorescences attractives
  ## Fonction utilisée pour l'analyse exploratoire des solutions
  larves_estimees <- dynamics(x, inflos_simulated)
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
  
  nrmse(larves_est[, 2], larves_observed[, 2], norm = "maxmin")
}

objCDE <- function(x) {
  ## ER, EH et max endo
  ## Inflorescences C/D/E
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
    nrmse(larves_est[, 3], larves_observed[, 3], norm = "maxmin"),
    critere(x, inflos_simulated))
}

# old ---------------------------------------------------------------------


obj100 <- function(x) {
  ## ER, EH et max endo
  ## Simulation sur 100 jours
  larves_estimees <- dynamics(x, rbind(inflos_simulated, matrix(1, nrow = 20, ncol = 3)))
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
    critere(x, rbind(inflos_simulated, matrix(0, nrow = 5, ncol = 3))))
}

obj_ressources0 <- function(x) {
  ## Disponibilité ressources k(t)
  larves_estimees <- dynamics_ressources(x[8:245], inflos_simulated)
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
  
  nrmse(larves_est[, 1], larves_observed[, 1], norm = "maxmin") +
    nrmse(larves_est[, 2], larves_observed[, 2], norm = "maxmin") +
    nrmse(larves_est[, 3], larves_observed[, 3], norm = "maxmin")
}


obj_isa <- function(x) {
  ## OLD
  larves_estimees <- dynamics_isa(x, inflos_simulated)
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

obj_fred <- function(x) {
  ## OLD
  larves_estimees <- dynamics_fred(x, inflos_simulated)
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


obj_diap <- function(x) {
  ## OLD
  larves_estimees <- dynamics_diap(x, inflos_simulated)
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
    critere_diap(x, inflos_simulated))
}


obj_diap0 <- function(x) {
  ## OLD
  larves_estimees <- dynamics_diap(x, inflos_simulated)
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


obj_ressources <- function(x) {
  ## Disponibilité ressources k(t)
  larves_estimees <- dynamics_ressources(x, inflos_simulated)
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
