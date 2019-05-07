## Script por calibrer les paramètres en utilisant les inflos attractives en entrée du modèle


# Packages / data ---------------------------------------------------------

library(tidyverse)
library(magrittr)
library(lubridate)
library(mco)
source("../model_R/model1.R")
simulated <- (read.csv("../data/attractive_simulated.csv") %>% as.matrix())[, 3:5]
attractiv <- (read.csv("../data/2017_attractives.csv") %>% as.matrix())[, 3:5]

data2017PS <- read_csv("b1.csv")
data2017ER <- read_csv("r1.csv")
data2017EH <- read_csv("h1.csv")
larves2017 <- cbind(ER = data2017ER$larves,
                    PS = data2017PS$larves,
                    EH = data2017EH$larves)
inflos2017 <- cbind(ER = data2017ER$inflos_vivantes,
                    PS = data2017PS$inflos_vivantes,
                    EH = data2017EH$inflos_vivantes)

date2017 <- as_date(data2017ER$date[1]:data2017ER$date[length(data2017EH$date)])
true_date <- read_csv2(file = "Data/2017_B1_bache.csv")$date
laps <- c(7, 7, 7, 8, 2, 5, 2, 4, 3, 5, 2, 5, 2, 5, 2, 4, 3, 4, 3)
true_index <- which(date2017 %in% true_date)

# Cost functions ----------------------------------------------------------

my_rmse <- function(x, y) {
    n_obs <- length(x)
    sqrt(sum((x - y)^2) / n_obs)
}

my_mae <- function(x, y) {
    n_obs <- length(x)
    sum(abs(x - y)) / n_obs
}

min_max <- function(x, y) {
    max(abs(x - y))
}

# Objective function ------------------------------------------------------

objectif <- function(x, my_function, inflo){
    
    larves_estimees <- dynamiques(x[1], x[2], x[3], x[4], x[5], inflo)
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

    larves_observed <- larves2017[true_index, ]
    return(c(my_function(larves_est[, 1], larves_observed[, 1]),
             my_function(larves_est[, 2], larves_observed[, 2]),
             my_function(larves_est[, 3], larves_observed[, 3])))
}

objectif2 <- function(x, my_function, inflo){
    
    larves_estimees <- dynamiques(x[1], x[2], x[3], x[4], x[5], inflo)
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
    
    larves_observed <- larves2017[true_index, ]
    return(c(my_function(larves_est[, 1], larves_observed[, 1]),
             my_function(larves_est[, 2], larves_observed[, 2]),
             my_function(larves_est[, 3], larves_observed[, 3]),
             my_function(sum(larves_est[, 1]), sum(larves_observed[, 1])),
             my_function(sum(larves_est[, 2]), sum(larves_observed[, 2])),
             my_function(sum(larves_est[, 3]), sum(larves_observed[, 3]))))
}

objectif3 <- function(x, my_function, inflo){
    
    larves_estimees <- dynamiques(x[1], x[2], x[3], x[4], x[5], inflo)
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
    
    larves_observed <- larves2017[true_index, ]
    return(c(my_function(cumsum(larves_est[, 1]), cumsum(larves_observed[, 1])),
             my_function(cumsum(larves_est[, 2]), cumsum(larves_observed[, 2])),
             my_function(cumsum(larves_est[, 3]), cumsum(larves_observed[, 3]))))
}

# Optimisation ------------------------------------------------------------


### FONCTION OBJECTIF CLASSIQUE
## Classique
res_c <- nsga2(objectif, 5, 3, my_mae, inflos2017,
               lower.bounds = rep(0,5),
               upper.bounds = c(10,1,1,1,10),
               popsize = 300, generations = 100)

ind_opt_c <- res_c$value %>% as_tibble %>%
    mutate(norm = abs(V1 + V2 + V3)) %$% 
    which.min(norm)

arg_opt_c <- res_c$par[ind_opt_c, ]

## Attractive
res_a <- nsga2(objectif, 5, 3, my_mae, attractiv,
             lower.bounds = rep(0,5),
             upper.bounds = c(10,1,1,1,10),
             popsize = 300, generations = 100)

ind_opt_a <- res_a$value %>% as_tibble %>%
    mutate(norm = abs(V1 + V2 + V3)) %$% 
    which.min(norm)

arg_opt_a <- res_a$par[ind_opt_a, ]

## Simulated attractive
res_s <- nsga2(objectif, 5, 3, my_mae, simulated,
             lower.bounds = rep(0,5),
             upper.bounds = c(10,1,1,1,10),
             popsize = 300, generations = 100)

ind_opt_s <- res_s$value %>% as_tibble %>%
    mutate(norm = abs(V1 + V2 + V3)) %$% 
    which.min(norm)

arg_opt_s <- res_s$par[ind_opt_s, ]


### FONCTION OBJECTIF AVEC CRITÈRE TOTAL LARVES
res_c2 <- nsga2(objectif2, 5, 6, my_mae, attractiv,
               lower.bounds = rep(0,5),
               upper.bounds = c(10,1,1,1,10),
               popsize = 300, generations = 100)

ind_opt_c2 <- res_c2$value %>% as_tibble %>%
    mutate(norm = abs(V1 + V2 + V3)) %$% 
    which.min(norm)

arg_opt_c2 <- res_c2$par[ind_opt_c2, ]

## Attractive
res_a2 <- nsga2(objectif2, 5, 6, my_mae, inflos2017,
               lower.bounds = rep(0,5),
               upper.bounds = c(10,1,1,1,10),
               popsize = 300, generations = 100)

ind_opt_a2 <- res_a2$value %>% as_tibble %>%
    mutate(norm = abs(V1 + V2 + V3)) %$% 
    which.min(norm)

arg_opt_a2 <- res_a2$par[ind_opt_a2, ]

## Simulated attractive
res_s2 <- nsga2(objectif2, 5, 6, my_mae, simulated,
               lower.bounds = rep(0,5),
               upper.bounds = c(10,1,1,1,10),
               popsize = 300, generations = 100)

ind_opt_s2 <- res_s2$value %>% as_tibble %>%
    mutate(norm = abs(V1 + V2 + V3)) %$% 
    which.min(norm)

arg_opt_s2 <- res_s2$par[ind_opt_s2, ]

### FONCTION OBJECTIF SOMME CUMULÉE
res_c3 <- nsga2(objectif3, 5, 3, my_mae, inflos2017,
                lower.bounds = rep(0,5),
                upper.bounds = c(10,1,1,1,10),
                popsize = 300, generations = 100)

ind_opt_c3 <- res_c3$value %>% as_tibble %>%
    mutate(norm = abs(V1 + V2 + V3)) %$% 
    which.min(norm)

arg_opt_c3 <- res_c3$par[ind_opt_c3, ]

## Attractive
res_a3 <- nsga2(objectif3, 5, 3, my_mae, attractiv,
                lower.bounds = rep(0,5),
                upper.bounds = c(10,1,1,1,10),
                popsize = 300, generations = 100)

ind_opt_a3 <- res_a3$value %>% as_tibble %>%
    mutate(norm = abs(V1 + V2 + V3)) %$% 
    which.min(norm)

arg_opt_a3 <- res_a3$par[ind_opt_a3, ]

## Simulated attractive
res_s3 <- nsga2(objectif3, 5, 3, my_mae, simulated,
                lower.bounds = rep(0,5),
                upper.bounds = c(10,1,1,1,10),
                popsize = 300, generations = 100)

ind_opt_s3 <- res_s3$value %>% as_tibble %>%
    mutate(norm = abs(V1 + V2 + V3)) %$% 
    which.min(norm)

arg_opt_s3 <- res_s3$par[ind_opt_s3, ]

# Plots -------------------------------------------------------------------

lta <- dynamiques(arg_opt_a3[1], arg_opt_a3[2], arg_opt_a3[3], arg_opt_a3[4], arg_opt_a3[5], attractiv)
lts <- dynamiques(arg_opt_s3[1], arg_opt_s3[2], arg_opt_s3[3], arg_opt_s3[4], arg_opt_s3[5], simulated)
ltc <- dynamiques(arg_opt_c3[1], arg_opt_c3[2], arg_opt_c3[3], arg_opt_c3[4], arg_opt_c3[5], inflos2017)
larves2017

to_plot_ER <- cbind(Date = date2017,
                    Observed = larves2017[, 1],
                    With_inflos_simuled = lts[, 1],
                    With_inflos_correct = lta[, 1],
                    Classic = ltc[, 1]) %>% 
    as_tibble %>% 
    mutate_at("Date", as_date) %>% 
    gather(Observed, With_inflos_simuled, With_inflos_correct, Classic, key = Toto, value = Nombre)

to_plot_ER %>% ggplot(aes(x = Date, y = Nombre, color = Toto)) +
    geom_point() +
    geom_line()


to_plot_PS <- cbind(Date = date2017,
                    Observed = larves2017[, 2],
                    With_inflos_simuled = lts[, 2],
                    With_inflos_correct = lta[, 2],
                    Classic = ltc[, 2]) %>% 
    as_tibble %>% 
    mutate_at("Date", as_date) %>% 
    gather(Observed, With_inflos_simuled, With_inflos_correct, Classic, key = Toto, value = Nombre)

to_plot_PS %>% ggplot(aes(x = Date, y = Nombre, color = Toto)) +
    geom_point() +
    geom_line()


to_plot_EH <- cbind(Date = date2017,
                    Observed = larves2017[, 3],
                    With_inflos_simuled = lts[, 3],
                    With_inflos_correct = lta[, 3],
                    Classic = ltc[, 3]) %>% 
    as_tibble %>% 
    mutate_at("Date", as_date) %>% 
    gather(Observed, With_inflos_simuled, With_inflos_correct, Classic, key = Toto, value = Nombre)

to_plot_EH %>% ggplot(aes(x = Date, y = Nombre, color = Toto)) +
    geom_point() +
    geom_line()
