
# Data / packages ---------------------------------------------------------

library(tidyverse)
library(magrittr)
library(lubridate)
dataPS <- read_csv2("Data/2017_B1_bache.csv")
dataER <- read_csv2("Data/2017_B1_enh.ras.csv")
dataEH <- read_csv2("Data/2017_B1_enh.haut.csv")

# Data format laurie ------------------------------------------------------

date_in <- dataPS$date %>% as_date
date_out <- date_in[1]:date_in[length(date_in)] %>% as_date

larvesPS <- approx(x = date_in, y = dataPS$larves, xout = date_out)$y
inflosPS <- approx(x = date_in, y = dataPS$inflos_vivantes, xout = date_out)$y

larvesER <- approx(x = date_in, y = dataER$larves, xout = date_out)$y
inflosER <- approx(x = date_in, y = dataER$inflos_vivantes, xout = date_out)$y

larvesEH <- approx(x = date_in, y = dataEH$larves, xout = date_out)$y
inflosEH <- approx(x = date_in, y = dataEH$inflos_vivantes, xout = date_out)$y

PSdata <- cbind(Date = date_out,
            Inflos = inflosPS,
            Larves = larvesPS) %>% 
    as_tibble %>% 
    mutate(Sol = "PS") %>% 
    mutate_at("Date", as_date)

ERdata <- cbind(Date = date_out,
            Inflos = inflosER,
            Larves = larvesER) %>% 
    as_tibble %>% 
    mutate(Sol = "PS") %>% 
    mutate_at("Date", as_date)

EHdata <- cbind(Date = date_out,
            Inflos = inflosEH,
            Larves = larvesEH) %>% 
    as_tibble %>% 
    mutate(Sol = "PS") %>% 
    mutate_at("Date", as_date)


# Cost function -----------------------------------------------------------

my_rmse <- function(x, y) {
    n_obs <- length(x)
    sqrt(sum((x - y)^2) / n_obs)
}


# Optimization ------------------------------------------------------------

source("model1.R")
library(mco)
inflos2017 <- cbind(ERdata$Inflos,
                    PSdata$Inflos,
                    EHdata$Inflos)
larves_obs <- cbind(ERdata$Larves,
                    PSdata$Larves,
                    EHdata$Larves)
colnames(larves_obs) <- c("ER", "PS", "EH")

objective <- function(arg, my_function) {
    larves_est <- dynamiques(arg[1], arg[2], arg[3], arg[4], arg[5], inflos2017, 2017)
    c(my_function(larves_est[, 1], larves_obs[, 1]),
      my_function(larves_est[, 2], larves_obs[, 2]),
      my_function(larves_est[, 3], larves_obs[, 3]))
}

res <- nsga2(objective, 5, 3, my_rmse,
             lower.bounds = rep(0,5),
             upper.bounds = c(10,1,1,1,10),
             popsize = 200, generations = 50)

ind_opt <- res$value %>% as_tibble %>%
    mutate(norm = abs(V1 + V2 + V3)) %$% 
    which.min(norm)

arg_opt <- res$par[ind_opt, ]
args_laurie <- c(0.59, 0.41, 0.98, 0.44, 0.84)
args_found <- c(1.497993e-01, 1.655082e-01, 9.760211e-01, 1.692474e-05, 3.081956e-01)
# plot --------------------------------------------------------------------

plotted <- function(arg) {
    estimed <- dynamiques(arg[1], arg[2], arg[3], arg[4], arg[5], inflos2017 %>% as.matrix, 2017)
    colnames(estimed) <- c("ER", "PS", "EH")
    estimed %<>% as_tibble() %>% 
        mutate(Statut = "Estimed")
    observed <- larves_obs %>% as_tibble() %>% 
        mutate(Statut = "Observed")
    date <- date_out
    to_plot <- rbind(observed, estimed)
    to_plot <- cbind(Date = rep(date, 2),
                     to_plot) %>%
        as_tibble() %>% 
        gather(ER, PS, EH, key = Sol, value = Nombre) %>% 
        mutate_at("Sol", as_factor)
    to_plot %>% ggplot(aes(x = Date, y = Nombre, color = Statut)) +
        geom_point() +
        geom_line() +
        facet_grid(Sol ~ .)
}
