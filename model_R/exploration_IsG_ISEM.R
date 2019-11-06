## Script pour afficher les solutions du rapport de stage de Bastien / version congres ISEM2019

# Packages / data ---------------------------------------------------------

library(tidyverse)
library(corrplot)
source("model_new.R")
source("objectif.R")
source("plot_res_IsG_ISEM.R")


### modele A
load("D:/Mes donnees/GitHub_Bastien/cecidomyie-master/model_R/calibration_A3.Rdata")
plot_decompo_A(c(0.08, 0.494, 0.976, 0.046, 1.928, 5600, 3.084), inflos_obs)
plot_decompo_A(c(0, 0.968, 1, 0.025, 0.100, 14483, 6.26), inflos_obs)
plot_decompo_A(c(0.026, 0.093, 0.565, 0.647, 0.174, 13823, 4.818), inflos_obs)


### modele B
load("D:/Mes donnees/GitHub_Bastien/cecidomyie-master/model_R/calibration_B.Rdata")
plot_decompo_B(c(0.044, 0.715, 0.998, 0.002, 0.143, 1548, 4.202, 1.167), inflos_obs)
plot_decompo_B(c(0, 1, 1, 0.001, 0.1, 17188, 6.3, 1.036), inflos_obs)
plot_decompo_B(c(0.002, 0.280, 0.923, 0.986, 0.177, 20066, 3.288, 1.110), inflos_obs)


### modele C
load("D:/Mes donnees/GitHub_Bastien/cecidomyie-master/model_R/calibration_C.Rdata")
plot_decompo(c(0.115, 0.189, 0.898, 0.012, 1.877, 515, 3.097), inflosCDE)
plot_decompo(c(0.004, 0.490, 0.990, 0.018, 0.242, 9970, 5.638), inflosCDE)
plot_decompo(c(0, 0.094, 0.999, 0.514, 0.181, 18214, 8.259), inflosCDE)


### modele C2
load("D:/Mes donnees/GitHub_Bastien/cecidomyie-master/model_R/calibration_C2.Rdata")
plot_decompo_C2(c(0.059, 0.042, 0.533, 0.116, 1.928, 500, 7.517, 10.3), inflosCDE)
plot_decompo_C2(c(0.047, 0.389, 0.832, 0.036, 1.347, 522, 6.466, 11.5), inflosCDE)
plot_decompo_C2(c(0.014, 0.284, 0.813, 0.018, 0.220, 6523, 8.403, 13.8), inflosCDE)


### modele D
load("D:/Mes donnees/GitHub_Bastien/cecidomyie-master/model_R/calibration_D.Rdata")
plot_decompo_season(c(0.021, 0.105, 0.938, 0.916, 1.992, 516, 6.018, 0.004), inflosCDE)
#plot_decompo_season(c(0.003, 0.188, 0.996, 0.771, 0.249, 5944, 6.139, 0.06), inflosCDE)
plot_decompo_season(c(0.019, 0.131, 0.999, 0.743, 0.262, 508, 6.19, 0.06), inflosCDE)
plot_decompo_season(c(0.0023, 0.288, 0.997, 0.064, 0.248, 9850, 6.16, 0.0297), inflosCDE)

