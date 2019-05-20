## Script qui teste le paramètre de saisonnalité


# Packages / data ---------------------------------------------------------

library(tidyverse)
library(magrittr)
library(lubridate)
library(mco)
source("../model_R/model.R")
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
    
    larves_estimees <- dynamics3(x, inflo)
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


# Optimisation ------------------------------------------------------------

res <- nsga2(objectif, 7, 3, my_mae, inflos2017,
               lower.bounds = rep(0,7),
               upper.bounds = c(10,1,1,1,10, 1, 1),
               popsize = 300, generations = 100)

ind_opt <- res$value %>% as_tibble %>%
    mutate(norm = abs(V1 + V2 + V3)) %$% 
    which.min(norm)

arg_opt <- res$par[ind_opt, ]


# Plot --------------------------------------------------------------------

larves_est <- dynamics3(arg_opt, inflos2017)
larves_er <- cbind(date = date2017, obs = larves2017[, 1], est = larves_est[, 1]) %>% 
    as_tibble %>% 
    mutate_at("date", as_date) %>% 
    gather(obs, est, key = statut, value = nombre, factor_key = TRUE) %>% 
    mutate(Sol = factor("ER", levels = c("ER", "PS", "EH")))

larves_ps <- cbind(date = date2017, obs = larves2017[, 2], est = larves_est[, 2]) %>% 
    as_tibble %>% 
    mutate_at("date", as_date) %>% 
    gather(obs, est, key = statut, value = nombre, factor_key = TRUE) %>% 
    mutate(Sol = factor("PS", levels = c("ER", "PS", "EH")))

larves_eh <- cbind(date = date2017, obs = larves2017[, 3], est = larves_est[, 3]) %>% 
    as_tibble %>% 
    mutate_at("date", as_date) %>% 
    gather(obs, est, key = statut, value = nombre, factor_key = TRUE) %>% 
    mutate(Sol = factor("EH", levels = c("ER", "PS", "EH")))

to_plot <- bind_rows(larves_er, larves_ps, larves_eh)
to_plot %>% ggplot(aes(x = date, y = nombre, color = statut)) +
    geom_point() +
    geom_line(lwd = 0.75) +
    theme_bw() +
    facet_grid(. ~ Sol)
