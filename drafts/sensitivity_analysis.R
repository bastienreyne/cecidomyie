## Script qui réalise une analyse de sensibilité du modèle.

library(tidyverse)
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


N <- 10000
Matrice <- matrix(runif(N * 5 * 2), nrow = N)

mat_A <- Matrice[, 1:5]
mat_A[, 5] <- 1 + 49 * mat_A[, 5]

mat_B <- Matrice[, 6:10]
mat_B[, 5] <- 1 + 49 * mat_B[, 5]

mat_C1 <- cbind(mat_A[, 1], mat_B[, 2:5])
mat_C2 <- cbind(mat_B[, 1], mat_A[, 2], mat_B[, 3:5])
mat_C3 <- cbind(mat_B[, 1:2], mat_A[, 3], mat_B[, 4:5])
mat_C4 <- cbind(mat_B[, 1:3], mat_A[, 4], mat_B[, 5])
mat_C5 <- cbind(mat_B[, 1:4], mat_A[, 5])

y_A <- matrix(NA, nrow = N, ncol = 3)
y_B <- matrix(NA, nrow = N, ncol = 3)
y_C1 <- matrix(NA, nrow = N, ncol = 3)
y_C2 <- matrix(NA, nrow = N, ncol = 3)
y_C3 <- matrix(NA, nrow = N, ncol = 3)
y_C4 <- matrix(NA, nrow = N, ncol = 3)
y_C5 <- matrix(NA, nrow = N, ncol = 3)

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
    
    c(my_mae(larves_est[, 1], larves_observed[, 1]),
      my_mae(larves_est[, 2], larves_observed[, 2]),
      my_mae(larves_est[, 3], larves_observed[, 3]))
}


for (iter in 1:N) {
    y_A[iter, ] <-  obj(mat_A[iter, ])
    y_B[iter, ] <-  obj(mat_B[iter, ])
    y_C1[iter, ] <- obj(mat_C1[iter, ])
    y_C2[iter, ] <- obj(mat_C2[iter, ])
    y_C3[iter, ] <- obj(mat_C3[iter, ])
    y_C4[iter, ] <- obj(mat_C4[iter, ])
    y_C5[iter, ] <- obj(mat_C5[iter, ])
}


f02 <- colMeans(y_A)^2
denom_ER <- (y_A[, 1] %*% y_A[, 1]) / N - f02[1]
denom_PS <- (y_A[, 2] %*% y_A[, 2]) / N - f02[2]
denom_EH <- (y_A[, 3] %*% y_A[, 3]) / N - f02[3]


num_ER1 <- (y_A[, 1] %*% y_C1[, 1]) / N - f02[1]
num_ER2 <- (y_A[, 1] %*% y_C2[, 1]) / N - f02[1]
num_ER3 <- (y_A[, 1] %*% y_C3[, 1]) / N - f02[1]
num_ER4 <- (y_A[, 1] %*% y_C4[, 1]) / N - f02[1]
num_ER5 <- (y_A[, 1] %*% y_C5[, 1]) / N - f02[1]

num_PS1 <- (y_A[, 2] %*% y_C1[, 2]) / N - f02[2]
num_PS2 <- (y_A[, 2] %*% y_C2[, 2]) / N - f02[2]
num_PS3 <- (y_A[, 2] %*% y_C3[, 2]) / N - f02[2]
num_PS4 <- (y_A[, 2] %*% y_C4[, 2]) / N - f02[2]
num_PS5 <- (y_A[, 2] %*% y_C5[, 2]) / N - f02[2]

num_EH1 <- (y_A[, 3] %*% y_C1[, 3]) / N - f02[3]
num_EH2 <- (y_A[, 3] %*% y_C2[, 3]) / N - f02[3]
num_EH3 <- (y_A[, 3] %*% y_C3[, 3]) / N - f02[3]
num_EH4 <- (y_A[, 3] %*% y_C4[, 3]) / N - f02[3]
num_EH5 <- (y_A[, 3] %*% y_C5[, 3]) / N - f02[3]


num_ER1T <- (y_B[, 1] %*% y_C1[, 1]) / N - f02[1]
num_ER2T <- (y_B[, 1] %*% y_C2[, 1]) / N - f02[1]
num_ER3T <- (y_B[, 1] %*% y_C3[, 1]) / N - f02[1]
num_ER4T <- (y_B[, 1] %*% y_C4[, 1]) / N - f02[1]
num_ER5T <- (y_B[, 1] %*% y_C5[, 1]) / N - f02[1]

num_PS1T <- (y_B[, 2] %*% y_C1[, 2]) / N - f02[2]
num_PS2T <- (y_B[, 2] %*% y_C2[, 2]) / N - f02[2]
num_PS3T <- (y_B[, 2] %*% y_C3[, 2]) / N - f02[2]
num_PS4T <- (y_B[, 2] %*% y_C4[, 2]) / N - f02[2]
num_PS5T <- (y_B[, 2] %*% y_C5[, 2]) / N - f02[2]

num_EH1T <- (y_B[, 3] %*% y_C1[, 3]) / N - f02[3]
num_EH2T <- (y_B[, 3] %*% y_C2[, 3]) / N - f02[3]
num_EH3T <- (y_B[, 3] %*% y_C3[, 3]) / N - f02[3]
num_EH4T <- (y_B[, 3] %*% y_C4[, 3]) / N - f02[3]
num_EH5T <- (y_B[, 3] %*% y_C5[, 3]) / N - f02[3]


S_ER1 <- num_ER1 / denom_ER
S_ER2 <- num_ER2 / denom_ER
S_ER3 <- num_ER3 / denom_ER
S_ER4 <- num_ER4 / denom_ER
S_ER5 <- num_ER5 / denom_ER

S_PS1 <- num_PS1 / denom_PS
S_PS2 <- num_PS2 / denom_PS
S_PS3 <- num_PS3 / denom_PS
S_PS4 <- num_PS4 / denom_PS
S_PS5 <- num_PS5 / denom_PS

S_EH1 <- num_EH1 / denom_EH
S_EH2 <- num_EH2 / denom_EH
S_EH3 <- num_EH3 / denom_EH
S_EH4 <- num_EH4 / denom_EH
S_EH5 <- num_EH5 / denom_EH

ST_ER1 <- 1 - num_ER1T / denom_ER
ST_ER2 <- 1 - num_ER2T / denom_ER
ST_ER3 <- 1 - num_ER3T / denom_ER
ST_ER4 <- 1 - num_ER4T / denom_ER
ST_ER5 <- 1 - num_ER5T / denom_ER

ST_PS1 <- 1 - num_PS1T / denom_PS
ST_PS2 <- 1 - num_PS2T / denom_PS
ST_PS3 <- 1 - num_PS3T / denom_PS
ST_PS4 <- 1 - num_PS4T / denom_PS
ST_PS5 <- 1 - num_PS5T / denom_PS

ST_EH1 <- 1 - num_EH1T / denom_EH
ST_EH2 <- 1 - num_EH2T / denom_EH
ST_EH3 <- 1 - num_EH3T / denom_EH
ST_EH4 <- 1 - num_EH4T / denom_EH
ST_EH5 <- 1 - num_EH5T / denom_EH

to_plot <- data.frame(Paramètre = factor(rep(c("gamma", "pps", "muER",
                                               "muEH", "k"), times = 6),
                                         levels = c("gamma", "pps", "muER",
                                                    "muEH", "k")),
                      Valeur = c(S_ER1, S_ER2, S_ER3, S_ER4, S_ER5,
                                  ST_ER1, ST_ER2, ST_ER3, ST_ER4, ST_ER5,
                                  S_PS1, S_PS2, S_PS3, S_PS4, S_PS5,
                                  ST_PS1, ST_PS2, ST_PS3, ST_PS4, ST_PS5,
                                  S_EH1, S_EH2, S_EH3, S_EH4, S_EH5,
                                  ST_EH1, ST_EH2, ST_EH3, ST_EH4, ST_EH5),
                      Indice = rep(c("Effet principal", "Effet total"),
                                   each = 5, times = 3),
                      Sol = factor(rep(c("Enherbement ras",
                                         "Paillage synthétique",
                                         "Enherbement haut"), each = 10),
                                   levels = c("Enherbement ras",
                                              "Paillage synthétique",
                                              "Enherbement haut")))

to_plot %>% ggplot +
    aes(x = Paramètre, y = Valeur, fill = Indice) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_grid(. ~ Sol) +
    theme_bw() +
    theme(legend.title = element_blank(),
          legend.position = "bottom",
          axis.text.x = element_text(size = 12)) +
    scale_x_discrete(labels = c(expression(gamma), expression(p[m]),
                                expression(mu[ER]), expression(mu[EH]),
                                expression(k)))
