## Script qui réalise une analyse de sensibilité du modèle.
## Méthode dans Global Sensitivity Analysis : the Primer

library(tidyverse)
source("/home/bastien/cecidomyie/model_R/model_A.R")
source("/home/bastien/cecidomyie/model_R/objectif_A.R")

N <- 50000
Matrice <- matrix(runif(N * 7 * 2), nrow = N)

mat_A <- Matrice[, 1:7]
mat_A[, 5] <- 0.5 + 1.5 * mat_A[, 5]
mat_A[, 6] <- 500 + 20000 * mat_A[, 6]
mat_A[, 7] <- 1 + 11 * mat_A[, 7]

mat_B <- Matrice[, 8:14]
mat_B[, 5] <- 0.5 + 1.5 * mat_B[, 5]
mat_B[, 6] <- 500 + 20000 * mat_B[, 6]
mat_B[, 7] <- 1 + 10 * mat_B[, 7]

mat_C1 <- cbind(mat_A[, 1], mat_B[, 2:7])
mat_C2 <- cbind(mat_B[, 1], mat_A[, 2], mat_B[, 3:7])
mat_C3 <- cbind(mat_B[, 1:2], mat_A[, 3], mat_B[, 4:7])
mat_C4 <- cbind(mat_B[, 1:3], mat_A[, 4], mat_B[, 5:7])
mat_C5 <- cbind(mat_B[, 1:4], mat_A[, 5], mat_B[, 6:7])
mat_C6 <- cbind(mat_B[, 1:5], mat_A[, 6], mat_B[, 7])
mat_C7 <- cbind(mat_B[, 1:6], mat_A[, 7])

y_A <- matrix(NA, nrow = N, ncol = 3)
y_B <- matrix(NA, nrow = N, ncol = 3)
y_C1 <- matrix(NA, nrow = N, ncol = 3)
y_C2 <- matrix(NA, nrow = N, ncol = 3)
y_C3 <- matrix(NA, nrow = N, ncol = 3)
y_C4 <- matrix(NA, nrow = N, ncol = 3)
y_C5 <- matrix(NA, nrow = N, ncol = 3)
y_C6 <- matrix(NA, nrow = N, ncol = 3)
y_C7 <- matrix(NA, nrow = N, ncol = 3)


for (iter in 1:N) {
    y_A[iter, ] <-  obj0(mat_A[iter, ])
    y_B[iter, ] <-  obj0(mat_B[iter, ])
    y_C1[iter, ] <- obj0(mat_C1[iter, ])
    y_C2[iter, ] <- obj0(mat_C2[iter, ])
    y_C3[iter, ] <- obj0(mat_C3[iter, ])
    y_C4[iter, ] <- obj0(mat_C4[iter, ])
    y_C5[iter, ] <- obj0(mat_C5[iter, ])
    y_C6[iter, ] <- obj0(mat_C6[iter, ])
    y_C7[iter, ] <- obj0(mat_C7[iter, ])
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
num_ER6 <- (y_A[, 1] %*% y_C6[, 1]) / N - f02[1]
num_ER7 <- (y_A[, 1] %*% y_C7[, 1]) / N - f02[1]

num_PS1 <- (y_A[, 2] %*% y_C1[, 2]) / N - f02[2]
num_PS2 <- (y_A[, 2] %*% y_C2[, 2]) / N - f02[2]
num_PS3 <- (y_A[, 2] %*% y_C3[, 2]) / N - f02[2]
num_PS4 <- (y_A[, 2] %*% y_C4[, 2]) / N - f02[2]
num_PS5 <- (y_A[, 2] %*% y_C5[, 2]) / N - f02[2]
num_PS6 <- (y_A[, 2] %*% y_C6[, 2]) / N - f02[2]
num_PS7 <- (y_A[, 2] %*% y_C7[, 2]) / N - f02[2]

num_EH1 <- (y_A[, 3] %*% y_C1[, 3]) / N - f02[3]
num_EH2 <- (y_A[, 3] %*% y_C2[, 3]) / N - f02[3]
num_EH3 <- (y_A[, 3] %*% y_C3[, 3]) / N - f02[3]
num_EH4 <- (y_A[, 3] %*% y_C4[, 3]) / N - f02[3]
num_EH5 <- (y_A[, 3] %*% y_C5[, 3]) / N - f02[3]
num_EH6 <- (y_A[, 3] %*% y_C6[, 3]) / N - f02[3]
num_EH7 <- (y_A[, 3] %*% y_C7[, 3]) / N - f02[3]


num_ER1T <- (y_B[, 1] %*% y_C1[, 1]) / N - f02[1]
num_ER2T <- (y_B[, 1] %*% y_C2[, 1]) / N - f02[1]
num_ER3T <- (y_B[, 1] %*% y_C3[, 1]) / N - f02[1]
num_ER4T <- (y_B[, 1] %*% y_C4[, 1]) / N - f02[1]
num_ER5T <- (y_B[, 1] %*% y_C5[, 1]) / N - f02[1]
num_ER6T <- (y_B[, 1] %*% y_C6[, 1]) / N - f02[1]
num_ER7T <- (y_B[, 1] %*% y_C7[, 1]) / N - f02[1]

num_PS1T <- (y_B[, 2] %*% y_C1[, 2]) / N - f02[2]
num_PS2T <- (y_B[, 2] %*% y_C2[, 2]) / N - f02[2]
num_PS3T <- (y_B[, 2] %*% y_C3[, 2]) / N - f02[2]
num_PS4T <- (y_B[, 2] %*% y_C4[, 2]) / N - f02[2]
num_PS5T <- (y_B[, 2] %*% y_C5[, 2]) / N - f02[2]
num_PS6T <- (y_B[, 2] %*% y_C6[, 2]) / N - f02[2]
num_PS7T <- (y_B[, 2] %*% y_C7[, 2]) / N - f02[2]

num_EH1T <- (y_B[, 3] %*% y_C1[, 3]) / N - f02[3]
num_EH2T <- (y_B[, 3] %*% y_C2[, 3]) / N - f02[3]
num_EH3T <- (y_B[, 3] %*% y_C3[, 3]) / N - f02[3]
num_EH4T <- (y_B[, 3] %*% y_C4[, 3]) / N - f02[3]
num_EH5T <- (y_B[, 3] %*% y_C5[, 3]) / N - f02[3]
num_EH6T <- (y_B[, 3] %*% y_C6[, 3]) / N - f02[3]
num_EH7T <- (y_B[, 3] %*% y_C7[, 3]) / N - f02[3]


S_ER1 <- num_ER1 / denom_ER
S_ER2 <- num_ER2 / denom_ER
S_ER3 <- num_ER3 / denom_ER
S_ER4 <- num_ER4 / denom_ER
S_ER5 <- num_ER5 / denom_ER
S_ER6 <- num_ER6 / denom_ER
S_ER7 <- num_ER7 / denom_ER

S_PS1 <- num_PS1 / denom_PS
S_PS2 <- num_PS2 / denom_PS
S_PS3 <- num_PS3 / denom_PS
S_PS4 <- num_PS4 / denom_PS
S_PS5 <- num_PS5 / denom_PS
S_PS6 <- num_PS6 / denom_PS
S_PS7 <- num_PS7 / denom_PS

S_EH1 <- num_EH1 / denom_EH
S_EH2 <- num_EH2 / denom_EH
S_EH3 <- num_EH3 / denom_EH
S_EH4 <- num_EH4 / denom_EH
S_EH5 <- num_EH5 / denom_EH
S_EH6 <- num_EH6 / denom_EH
S_EH7 <- num_EH7 / denom_EH

ST_ER1 <- 1 - num_ER1T / denom_ER
ST_ER2 <- 1 - num_ER2T / denom_ER
ST_ER3 <- 1 - num_ER3T / denom_ER
ST_ER4 <- 1 - num_ER4T / denom_ER
ST_ER5 <- 1 - num_ER5T / denom_ER
ST_ER6 <- 1 - num_ER6T / denom_ER
ST_ER7 <- 1 - num_ER7T / denom_ER

ST_PS1 <- 1 - num_PS1T / denom_PS
ST_PS2 <- 1 - num_PS2T / denom_PS
ST_PS3 <- 1 - num_PS3T / denom_PS
ST_PS4 <- 1 - num_PS4T / denom_PS
ST_PS5 <- 1 - num_PS5T / denom_PS
ST_PS6 <- 1 - num_PS6T / denom_PS
ST_PS7 <- 1 - num_PS7T / denom_PS

ST_EH1 <- 1 - num_EH1T / denom_EH
ST_EH2 <- 1 - num_EH2T / denom_EH
ST_EH3 <- 1 - num_EH3T / denom_EH
ST_EH4 <- 1 - num_EH4T / denom_EH
ST_EH5 <- 1 - num_EH5T / denom_EH
ST_EH6 <- 1 - num_EH6T / denom_EH
ST_EH7 <- 1 - num_EH7T / denom_EH

to_plot <- data.frame(Paramètre = factor(rep(c("gamma", "pps", "muER",
                                               "muEH", "k", "stock", "eggs"), times = 6),
                                         levels = c("gamma", "pps", "muER",
                                                    "muEH", "k", "stock", "eggs")),
                      Valeur = c(S_ER1, S_ER2, S_ER3, S_ER4, S_ER5, S_ER6, S_ER7,
                                  ST_ER1, ST_ER2, ST_ER3, ST_ER4, ST_ER5, ST_ER6, ST_ER7,
                                  S_PS1, S_PS2, S_PS3, S_PS4, S_PS5, S_PS6, S_PS7,
                                  ST_PS1, ST_PS2, ST_PS3, ST_PS4, ST_PS5, ST_PS6, ST_PS7,
                                  S_EH1, S_EH2, S_EH3, S_EH4, S_EH5, S_EH6, S_EH7,
                                  ST_EH1, ST_EH2, ST_EH3, ST_EH4, ST_EH5, ST_EH6, ST_EH7),
                      Indice = rep(c("Effet principal  ", "Effet total"),
                                   each = 7, times = 3),
                      Sol = factor(rep(c("Enherbement ras",
                                         "Paillage synthétique",
                                         "Enherbement haut"), each = 14),
                                   levels = c("Enherbement ras",
                                              "Paillage synthétique",
                                              "Enherbement haut")))


# save(to_plot, file = "sobol_A.Rdata")


# Affichage ---------------------------------------------------------------

# load("/home/bastien/cecidomyie/model_R/sobol_A.Rdata")


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
                                expression(k), "stock", expression(E[0]*mu[l])))
