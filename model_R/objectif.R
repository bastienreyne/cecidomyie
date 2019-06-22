library(tidyverse)
library(magrittr)
library(mco)
library(hydroGOF)
source("/home/bastien/cecidomyie/model_R/model.R")
load("/home/bastien/cecidomyie/data/date2017.Rdata")
laps <- c(7, 7, 7, 8, 2, 5, 2, 4, 3, 5, 2, 5, 2, 5, 2, 4, 3, 4, 3)
true_index <- which(date2017 %in% true_date2017)
inflos_simulated <- as.matrix(read.csv("/home/bastien/cecidomyie/data/attractive_simulated.csv")[, 3:5])
data_piege <- read.csv("/home/bastien/cecidomyie/data/2017_piege.csv")
larves1 <- data_piege %>% filter(Sol == "ER") %>% pull(larves)
larves2 <- data_piege %>% filter(Sol == "PS") %>% pull(larves)
larves3 <- data_piege %>% filter(Sol == "EH") %>% pull(larves)
larves <- cbind(larves1, larves2, larves3)


my_mae <- function(x, y) {
  n_obs <- length(x)
  sum(abs(x - y)) / n_obs
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
  
  c(nrmse(larves_est[, 1], larves_observed[, 1], norm = "maxmin"),
    nrmse(larves_est[, 3], larves_observed[, 3], norm = "maxmin"),
    critere(x, inflos_simulated))
}


obj0 <- function(x) {
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
  
  c(nrmse(larves_est[, 1], larves_observed[, 1], norm = "maxmin"),
    nrmse(larves_est[, 2], larves_observed[, 2], norm = "maxmin"),
    nrmse(larves_est[, 3], larves_observed[, 3], norm = "maxmin"))
}


objPS <- function(x) {
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
  
  nrmse(larves_est[, 2], larves_observed[, 2], norm = "maxmin")
}
