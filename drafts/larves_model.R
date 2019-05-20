## Script qui...

# Packages / data ---------------------------------------------------------

library(tidyverse)
library(mco)
data <- read.csv("../data/2017_piege.csv")

larves_ER <- data %>% filter(Sol == "ER") %>% pull(inflos)
larves_PS <- data %>% filter(Sol == "PS") %>% pull(inflos)
larves_EH <- data %>% filter(Sol == "EH") %>% pull(inflos)

# Corrélations ------------------------------------------------------------

## 12
cor(lag(larves_ER, 12)[-(1:12)], larves_ER[13:80], method = "spearman")
cor(lag(larves_PS, 12)[-(1:12)], larves_PS[13:80], method = "spearman")
cor(lag(larves_EH, 12)[-(1:12)], larves_EH[13:80], method = "spearman")

## 13
cor(lag(larves_ER, 13)[-(1:13)], larves_ER[14:80], method = "spearman")
cor(lag(larves_PS, 13)[-(1:13)], larves_PS[14:80], method = "spearman")
cor(lag(larves_EH, 13)[-(1:13)], larves_EH[14:80], method = "spearman")


# Modèle ------------------------------------------------------------------

my_mae <- function(x, y) {
    n_obs <- length(x)
    sum(abs(x - y)) / n_obs
}

objectif_larves <- function(x, larve) {
    alpha <- x[1:68]
    r <- x[69]
    mu <- x[70]
    exo <- x[71:138]
    larves_lag <- lag(larve, 12)[-(1:12)]
    larves_pred <- alpha * r * (mu * larves_lag + exo)
    
    my_mae(larves_pred, larve[13:80])
}

give <- function(x, larve) {
    alpha <- x[1:68]
    r <- x[69]
    mu <- x[70]
    exo <- x[71:138]
    larves_lag <- lag(larve, 12)[-(1:12)]
    larves_pred <- alpha * r * (mu * larves_lag + exo)
}

res_ER <- nsga2(objectif_larves, 138, 1, larves_ER,
                lower.bounds = rep(0, 138), upper.bounds = rep(5000, 138),
                popsize = 800, generations = 5000)

res_PS <- nsga2(objectif_larves, 138, 1, larves_PS,
                lower.bounds = rep(0, 138), upper.bounds = rep(5000, 138),
                popsize = 800, generations = 5000)

res_EH <- nsga2(objectif_larves, 138, 1, larves_EH,
                lower.bounds = rep(0, 138), upper.bounds = rep(5000, 138),
                popsize = 800, generations = 5000)


