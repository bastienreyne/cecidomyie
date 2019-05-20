## Script qui recence les divers aopprts aux modèles et leurs effets

# Packages / data ---------------------------------------------------------

library(tidyverse)
library(magrittr)
library(lubridate)
library(mco)
source("../model_R/model.R")
simulated <- (read.csv("../data/attractive_simulated.csv") %>% as.matrix())[, 3:5]
attractiv <- (read.csv("../data/2017_attractives.csv") %>% as.matrix())[, 3:5]
bursts <- read.csv("../data/2017_bursts_simulated.csv")[, -1] %>% as.matrix()
load("Itsdmin.Rdata")
sdmin <- cbind(Itsdmin_ER, Itsdmin_PS, Itsdmin_EH)

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


# Objectif ----------------------------------------------------------------

objectif <- function(x, my_function, inflos){
    
    larves_estimees <- dynamics2(x, inflos)
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

objectif2 <- function(x, my_function, inflos){
    
    larves_estimees <- dynamics2(x, inflos)
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

objectif3 <- function(x, my_function, bursts){
    
    larves_estimees <- dynamics(x, bursts)
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

objectif4 <- function(x, my_function, bursts) {
    
    larves_estimees <- dynamics(x, bursts)
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

# Reference ---------------------------------------------------------------

res_ref <- nsga2(objectif, 5, 3, my_mae, inflos2017,
                lower.bounds = rep(0,5),
                upper.bounds = c(10,1,1,1,10),
                popsize = 100, generations = 50)

ind_opt_ref <- res_ref$value %>% as_tibble %>%
    mutate(norm = abs(V1 + V2 + V3)) %$% 
    which.min(norm)

arg_opt_ref <- res_ref$par[ind_opt_ref, ]

lref <- dynamics2(arg_opt_ref, inflos2017)

to_plot_ER <- cbind(Date = date2017,
                    Observées = larves2017[, 1],
                    Référence = lref[, 1]) %>% 
    as_tibble %>% 
    mutate_at("Date", as_date) %>% 
    gather(Observées, Référence, key = Toto, value = Nombre) %>% 
    mutate(Sol = factor("Enherbement ras", levels = c("Enherbement ras",
                                                      "Paillage synthétique",
                                                      "Enherbement haut")))
to_plot_PS <- cbind(Date = date2017,
                    Observées = larves2017[, 2],
                    Référence = lref[, 2]) %>% 
    as_tibble %>% 
    mutate_at("Date", as_date) %>% 
    gather(Observées, Référence, key = Toto, value = Nombre) %>% 
    mutate(Sol = factor("Paillage synthétique", levels = c("Enherbement ras",
                                                      "Paillage synthétique",
                                                      "Enherbement haut")))

to_plot_EH <- cbind(Date = date2017,
                    Observées = larves2017[, 3],
                    Référence = lref[, 3]) %>% 
    as_tibble %>% 
    mutate_at("Date", as_date) %>% 
    gather(Observées, Référence, key = Toto, value = Nombre) %>% 
    mutate(Sol = factor("Enherbement haut", levels = c("Enherbement ras",
                                                      "Paillage synthétique",
                                                      "Enherbement haut")))

to_plot <- bind_rows(to_plot_ER, to_plot_PS, to_plot_EH)

to_plot %>% ggplot(aes(x = Date, y = Nombre, color = Toto)) +
    geom_point() +
    geom_line() +
    theme(legend.title = element_blank()) +
    facet_wrap(Sol ~ .) +
    ylab("Nombre de larves")


# 6 critères --------------------------------------------------------------

res_crit <- nsga2(objectif2, 5, 6, my_mae, inflos2017,
                 lower.bounds = rep(0,5),
                 upper.bounds = c(10,1,1,1,10),
                 popsize = 100, generations = 50)

ind_opt_crit <- res_crit$value %>% as_tibble %>%
    mutate(norm = abs(V1 + V2 + V3 + V4 + V5 + V6)) %$% 
    which.min(norm)

arg_opt_crit <- res_crit$par[ind_opt_crit, ]

lcrit <- dynamics2(arg_opt_crit, inflos2017)

to_plot_ER <- cbind(Date = date2017,
                    Observées = larves2017[, 1],
                    Référence = lref[, 1],
                    `Critères totaux` = lcrit[, 1]) %>% 
    as_tibble %>% 
    mutate_at("Date", as_date) %>% 
    gather(Observées, Référence, `Critères totaux`, key = Toto, value = Nombre) %>% 
    mutate(Sol = factor("Enherbement ras", levels = c("Enherbement ras",
                                                      "Paillage synthétique",
                                                      "Enherbement haut")))
to_plot_PS <- cbind(Date = date2017,
                    Observées = larves2017[, 2],
                    Référence = lref[, 2],
                    `Critères totaux` = lcrit[, 2]) %>% 
    as_tibble %>% 
    mutate_at("Date", as_date) %>% 
    gather(Observées, Référence, `Critères totaux`, key = Toto, value = Nombre) %>% 
    mutate(Sol = factor("Paillage synthétique", levels = c("Enherbement ras",
                                                           "Paillage synthétique",
                                                           "Enherbement haut")))

to_plot_EH <- cbind(Date = date2017,
                    Observées = larves2017[, 3],
                    Référence = lref[, 3],
                    `Critères totaux` = lcrit[, 3]) %>% 
    as_tibble %>% 
    mutate_at("Date", as_date) %>% 
    gather(Observées, Référence, `Critères totaux`, key = Toto, value = Nombre) %>% 
    mutate(Sol = factor("Enherbement haut", levels = c("Enherbement ras",
                                                       "Paillage synthétique",
                                                       "Enherbement haut")))

to_plot <- bind_rows(to_plot_ER, to_plot_PS, to_plot_EH)

to_plot %>% ggplot(aes(x = Date, y = Nombre, color = Toto)) +
    geom_point() +
    geom_line() +
    theme(legend.title = element_blank()) +
    facet_wrap(Sol ~ .) +
    ylab("Nombre de larves")


# Attractives dataset 1 ---------------------------------------------------

res_a1 <- nsga2(objectif, 5, 3, my_mae, attractiv,
                 lower.bounds = rep(0,5),
                 upper.bounds = c(10,1,1,1,10),
                 popsize = 100, generations = 50)

ind_opt_a1 <- res_a1$value %>% as_tibble %>%
    mutate(norm = abs(V1 + V2 + V3)) %$% 
    which.min(norm)

arg_opt_a1 <- res_a1$par[ind_opt_a1, ]

la1 <- dynamics2(arg_opt_a1, attractiv)

to_plot_ER <- cbind(Date = date2017,
                    Observées = larves2017[, 1],
                    Référence = lref[, 1],
                    `Inflos attractives` = la1[, 1]) %>% 
    as_tibble %>% 
    mutate_at("Date", as_date) %>% 
    gather(Observées, Référence, `Inflos attractives`, key = Toto, value = Nombre) %>% 
    mutate(Sol = factor("Enherbement ras", levels = c("Enherbement ras",
                                                      "Paillage synthétique",
                                                      "Enherbement haut")))
to_plot_PS <- cbind(Date = date2017,
                    Observées = larves2017[, 2],
                    Référence = lref[, 2],
                    `Inflos attractives` = la1[, 2]) %>% 
    as_tibble %>% 
    mutate_at("Date", as_date) %>% 
    gather(Observées, Référence, `Inflos attractives`, key = Toto, value = Nombre) %>% 
    mutate(Sol = factor("Paillage synthétique", levels = c("Enherbement ras",
                                                           "Paillage synthétique",
                                                           "Enherbement haut")))

to_plot_EH <- cbind(Date = date2017,
                    Observées = larves2017[, 3],
                    Référence = lref[, 3],
                    `Inflos attractives` = la1[, 3]) %>% 
    as_tibble %>% 
    mutate_at("Date", as_date) %>% 
    gather(Observées, Référence, `Inflos attractives`, key = Toto, value = Nombre) %>% 
    mutate(Sol = factor("Enherbement haut", levels = c("Enherbement ras",
                                                       "Paillage synthétique",
                                                       "Enherbement haut")))

to_plot <- bind_rows(to_plot_ER, to_plot_PS, to_plot_EH)

to_plot %>% ggplot(aes(x = Date, y = Nombre, color = Toto)) +
    geom_point() +
    geom_line() +
    theme(legend.title = element_blank()) +
    facet_wrap(Sol ~ .) +
    ylab("Nombre de larves")


# Attractives simu --------------------------------------------------------


res_as <- nsga2(objectif, 5, 3, my_mae, simulated,
                lower.bounds = rep(0,5),
                upper.bounds = c(10,1,1,1,10),
                popsize = 100, generations = 50)

ind_opt_as <- res_as$value %>% as_tibble %>%
    mutate(norm = abs(V1 + V2 + V3)) %$% 
    which.min(norm)

arg_opt_as <- res_as$par[ind_opt_as, ]

las <- dynamics2(arg_opt_as, simulated)

to_plot_ER <- cbind(Date = date2017,
                    Observées = larves2017[, 1],
                    Référence = lref[, 1],
                    `Inflos attractives` = las[, 1]) %>% 
    as_tibble %>% 
    mutate_at("Date", as_date) %>% 
    gather(Observées, Référence, `Inflos attractives`, key = Toto, value = Nombre) %>% 
    mutate(Sol = factor("Enherbement ras", levels = c("Enherbement ras",
                                                      "Paillage synthétique",
                                                      "Enherbement haut")))
to_plot_PS <- cbind(Date = date2017,
                    Observées = larves2017[, 2],
                    Référence = lref[, 2],
                    `Inflos attractives` = las[, 2]) %>% 
    as_tibble %>% 
    mutate_at("Date", as_date) %>% 
    gather(Observées, Référence, `Inflos attractives`, key = Toto, value = Nombre) %>% 
    mutate(Sol = factor("Paillage synthétique", levels = c("Enherbement ras",
                                                           "Paillage synthétique",
                                                           "Enherbement haut")))

to_plot_EH <- cbind(Date = date2017,
                    Observées = larves2017[, 3],
                    Référence = lref[, 3],
                    `Inflos attractives` = las[, 3]) %>% 
    as_tibble %>% 
    mutate_at("Date", as_date) %>% 
    gather(Observées, Référence, `Inflos attractives`, key = Toto, value = Nombre) %>% 
    mutate(Sol = factor("Enherbement haut", levels = c("Enherbement ras",
                                                       "Paillage synthétique",
                                                       "Enherbement haut")))

to_plot <- bind_rows(to_plot_ER, to_plot_PS, to_plot_EH)

to_plot %>% ggplot(aes(x = Date, y = Nombre, color = Toto)) +
    geom_point() +
    geom_line() +
    theme(legend.title = element_blank()) +
    facet_wrap(Sol ~ .) +
    ylab("Nombre de larves")


# Répartition uniforme ----------------------------------------------------


res_uni <- nsga2(objectif, 5, 3, my_mae, inflos2017,
                 lower.bounds = rep(0,5),
                 upper.bounds = c(10,1,1,1,10),
                 popsize = 100, generations = 50)

ind_opt_uni <- res_uni$value %>% as_tibble %>%
    mutate(norm = abs(V1 + V2 + V3)) %$% 
    which.min(norm)

arg_opt_uni <- res_uni$par[ind_opt_uni, ]

luni <- dynamics2(arg_opt_uni, inflos2017)

to_plot_ER <- cbind(Date = date2017,
                    Observées = larves2017[, 1],
                    Référence = lref[, 1],
                    `Répartition uniforme` = luni[, 1]) %>% 
    as_tibble %>% 
    mutate_at("Date", as_date) %>% 
    gather(Observées, Référence, `Répartition uniforme`, key = Toto, value = Nombre) %>% 
    mutate(Sol = factor("Enherbement ras", levels = c("Enherbement ras",
                                                      "Paillage synthétique",
                                                      "Enherbement haut")))
to_plot_PS <- cbind(Date = date2017,
                    Observées = larves2017[, 2],
                    Référence = lref[, 2],
                    `Répartition uniforme` = luni[, 2]) %>% 
    as_tibble %>% 
    mutate_at("Date", as_date) %>% 
    gather(Observées, Référence, `Répartition uniforme`, key = Toto, value = Nombre) %>% 
    mutate(Sol = factor("Paillage synthétique", levels = c("Enherbement ras",
                                                           "Paillage synthétique",
                                                           "Enherbement haut")))

to_plot_EH <- cbind(Date = date2017,
                    Observées = larves2017[, 3],
                    Référence = lref[, 3],
                    `Répartition uniforme` = luni[, 3]) %>% 
    as_tibble %>% 
    mutate_at("Date", as_date) %>% 
    gather(Observées, Référence, `Répartition uniforme`, key = Toto, value = Nombre) %>% 
    mutate(Sol = factor("Enherbement haut", levels = c("Enherbement ras",
                                                       "Paillage synthétique",
                                                       "Enherbement haut")))

to_plot <- bind_rows(to_plot_ER, to_plot_PS, to_plot_EH)

to_plot %>% ggplot(aes(x = Date, y = Nombre, color = Toto)) +
    geom_point() +
    geom_line() +
    theme(legend.title = element_blank()) +
    facet_wrap(Sol ~ .) +
    ylab("Nombre de larves")


# Répartion cloche --------------------------------------------------------

res_clo <- nsga2(objectif, 5, 3, my_mae, inflos2017,
                 lower.bounds = rep(0,5),
                 upper.bounds = c(10,1,1,1,10),
                 popsize = 100, generations = 50)

ind_opt_clo <- res_clo$value %>% as_tibble %>%
    mutate(norm = abs(V1 + V2 + V3)) %$% 
    which.min(norm)

arg_opt_clo <- res_clo$par[ind_opt_clo, ]

lclo <- dynamics2(arg_opt_clo, inflos2017)

to_plot_ER <- cbind(Date = date2017,
                    Observées = larves2017[, 1],
                    Référence = lref[, 1],
                    `Répartition cloche` = lclo[, 1]) %>% 
    as_tibble %>% 
    mutate_at("Date", as_date) %>% 
    gather(Observées, Référence, `Répartition cloche`, key = Toto, value = Nombre) %>% 
    mutate(Sol = factor("Enherbement ras", levels = c("Enherbement ras",
                                                      "Paillage synthétique",
                                                      "Enherbement haut")))
to_plot_PS <- cbind(Date = date2017,
                    Observées = larves2017[, 2],
                    Référence = lref[, 2],
                    `Répartition cloche` = lclo[, 2]) %>% 
    as_tibble %>% 
    mutate_at("Date", as_date) %>% 
    gather(Observées, Référence, `Répartition cloche`, key = Toto, value = Nombre) %>% 
    mutate(Sol = factor("Paillage synthétique", levels = c("Enherbement ras",
                                                           "Paillage synthétique",
                                                           "Enherbement haut")))

to_plot_EH <- cbind(Date = date2017,
                    Observées = larves2017[, 3],
                    Référence = lref[, 3],
                    `Répartition cloche` = lclo[, 3]) %>% 
    as_tibble %>% 
    mutate_at("Date", as_date) %>% 
    gather(Observées, Référence, `Répartition cloche`, key = Toto, value = Nombre) %>% 
    mutate(Sol = factor("Enherbement haut", levels = c("Enherbement ras",
                                                       "Paillage synthétique",
                                                       "Enherbement haut")))

to_plot <- bind_rows(to_plot_ER, to_plot_PS, to_plot_EH)

to_plot %>% ggplot(aes(x = Date, y = Nombre, color = Toto)) +
    geom_point() +
    geom_line() +
    theme(legend.title = element_blank()) +
    facet_wrap(Sol ~ .) +
    ylab("Nombre de larves")


# Delta_T -----------------------------------------------------------------

res_dt <- nsga2(objectif3, 6, 3, my_mae, bursts,
                 lower.bounds = c(rep(0,5), 7),
                 upper.bounds = c(10,1,1,1,10, 12),
                 popsize = 100, generations = 50)

ind_opt_dt <- res_dt$value %>% as_tibble %>%
    mutate(norm = abs(V1 + V2 + V3)) %$% 
    which.min(norm)

arg_opt_dt <- res_dt$par[ind_opt_dt, ]

ldt <- dynamics(arg_opt_dt, bursts)

to_plot_ER <- cbind(Date = date2017,
                    Observées = larves2017[, 1],
                    Référence = lref[, 1],
                    Delta_t = ldt[, 1]) %>% 
    as_tibble %>% 
    mutate_at("Date", as_date) %>% 
    gather(Observées, Référence, Delta_t, key = Toto, value = Nombre) %>% 
    mutate(Sol = factor("Enherbement ras", levels = c("Enherbement ras",
                                                      "Paillage synthétique",
                                                      "Enherbement haut")))
to_plot_PS <- cbind(Date = date2017,
                    Observées = larves2017[, 2],
                    Référence = lref[, 2],
                    Delta_t = ldt[, 2]) %>% 
    as_tibble %>% 
    mutate_at("Date", as_date) %>% 
    gather(Observées, Référence, Delta_t, key = Toto, value = Nombre) %>% 
    mutate(Sol = factor("Paillage synthétique", levels = c("Enherbement ras",
                                                           "Paillage synthétique",
                                                           "Enherbement haut")))

to_plot_EH <- cbind(Date = date2017,
                    Observées = larves2017[, 3],
                    Référence = lref[, 3],
                    Delta_t = ldt[, 3]) %>% 
    as_tibble %>% 
    mutate_at("Date", as_date) %>% 
    gather(Observées, Référence, Delta_t, key = Toto, value = Nombre) %>% 
    mutate(Sol = factor("Enherbement haut", levels = c("Enherbement ras",
                                                       "Paillage synthétique",
                                                       "Enherbement haut")))

to_plot <- bind_rows(to_plot_ER, to_plot_PS, to_plot_EH)

to_plot %>% ggplot(aes(x = Date, y = Nombre, color = Toto)) +
    geom_point() +
    geom_line() +
    theme(legend.title = element_blank()) +
    facet_wrap(Sol ~ .) +
    ylab("Nombre de larves")



# Everything --------------------------------------------------------------

res_tout <- nsga2(objectif2, 5, 6, my_mae, attractiv,
                  lower.bounds = rep(0,5),
                  upper.bounds = c(10,1,1,1,10),
                  popsize = 100, generations = 50)

ind_opt_tout <- res_tout$value %>% as_tibble %>%
    mutate(norm = abs(V1 + V2 + V3 + V4 + V5 + V6)) %$% 
    which.min(norm)

arg_opt_tout <- res_tout$par[ind_opt_tout, ]

ltout <- dynamics2(arg_opt_tout, attractiv)

to_plot_ER <- cbind(Date = date2017,
                    Observées = larves2017[, 1],
                    Référence = lref[, 1],
                    Tout = ltout[, 1]) %>% 
    as_tibble %>% 
    mutate_at("Date", as_date) %>% 
    gather(Observées, Référence, Tout, key = Toto, value = Nombre) %>% 
    mutate(Sol = factor("Enherbement ras", levels = c("Enherbement ras",
                                                      "Paillage synthétique",
                                                      "Enherbement haut")))
to_plot_PS <- cbind(Date = date2017,
                    Observées = larves2017[, 2],
                    Référence = lref[, 2],
                    Tout = ltout[, 2]) %>% 
    as_tibble %>% 
    mutate_at("Date", as_date) %>% 
    gather(Observées, Référence, Tout, key = Toto, value = Nombre) %>% 
    mutate(Sol = factor("Paillage synthétique", levels = c("Enherbement ras",
                                                           "Paillage synthétique",
                                                           "Enherbement haut")))

to_plot_EH <- cbind(Date = date2017,
                    Observées = larves2017[, 3],
                    Référence = lref[, 3],
                    Tout = ltout[, 3]) %>% 
    as_tibble %>% 
    mutate_at("Date", as_date) %>% 
    gather(Observées, Référence, Tout, key = Toto, value = Nombre) %>% 
    mutate(Sol = factor("Enherbement haut", levels = c("Enherbement ras",
                                                       "Paillage synthétique",
                                                       "Enherbement haut")))

to_plot <- bind_rows(to_plot_ER, to_plot_PS, to_plot_EH)

to_plot %>% ggplot(aes(x = Date, y = Nombre, color = Toto)) +
    geom_point() +
    geom_line() +
    theme(legend.title = element_blank()) +
    facet_wrap(Sol ~ .) +
    ylab("Nombre de larves")


# Everything 2 ------------------------------------------------------------

res_tout2 <- nsga2(objectif4, 6, 6, my_mae, bursts,
                  lower.bounds = c(rep(0,5), 7),
                  upper.bounds = c(10,1,1,1,10, 12),
                  popsize = 100, generations = 50)

ind_opt_tout2 <- res_tout2$value %>% as_tibble %>%
    mutate(norm = abs(V1 + V2 + V3 + V4 + V5 + V6)) %$% 
    which.min(norm)

arg_opt_tout2 <- res_tout2$par[ind_opt_tout2, ]

ltout2 <- dynamics(arg_opt_tout2, bursts)

to_plot_ER <- cbind(Date = date2017,
                    Observées = larves2017[, 1],
                    Référence = lref[, 1],
                    Tout = ltout2[, 1]) %>% 
    as_tibble %>% 
    mutate_at("Date", as_date) %>% 
    gather(Observées, Référence, Tout, key = Toto, value = Nombre) %>% 
    mutate(Sol = factor("Enherbement ras", levels = c("Enherbement ras",
                                                      "Paillage synthétique",
                                                      "Enherbement haut")))
to_plot_PS <- cbind(Date = date2017,
                    Observées = larves2017[, 2],
                    Référence = lref[, 2],
                    Tout = ltout2[, 2]) %>% 
    as_tibble %>% 
    mutate_at("Date", as_date) %>% 
    gather(Observées, Référence, Tout, key = Toto, value = Nombre) %>% 
    mutate(Sol = factor("Paillage synthétique", levels = c("Enherbement ras",
                                                           "Paillage synthétique",
                                                           "Enherbement haut")))

to_plot_EH <- cbind(Date = date2017,
                    Observées = larves2017[, 3],
                    Référence = lref[, 3],
                    Tout = ltout2[, 3]) %>% 
    as_tibble %>% 
    mutate_at("Date", as_date) %>% 
    gather(Observées, Référence, Tout, key = Toto, value = Nombre) %>% 
    mutate(Sol = factor("Enherbement haut", levels = c("Enherbement ras",
                                                       "Paillage synthétique",
                                                       "Enherbement haut")))

to_plot <- bind_rows(to_plot_ER, to_plot_PS, to_plot_EH)

to_plot %>% ggplot(aes(x = Date, y = Nombre, color = Toto)) +
    geom_point() +
    geom_line() +
    theme_bw() +
    theme(legend.title = element_blank()) +
    facet_wrap(Sol ~ .) +
    ylab("Nombre de larves")


# Attractives sdmin -------------------------------------------------------

res_asd <- nsga2(objectif, 5, 3, my_mae, sdmin,
                lower.bounds = rep(0,5),
                upper.bounds = c(10,1,1,1,10),
                popsize = 100, generations = 50)

ind_opt_asd <- res_asd$value %>% as_tibble %>%
    mutate(norm = abs(V1 + V2 + V3)) %$% 
    which.min(norm)

arg_opt_asd <- res_asd$par[ind_opt_asd, ]

lasd <- dynamics2(arg_opt_asd, simulated)

to_plot_ER <- cbind(Date = date2017,
                    Observées = larves2017[, 1],
                    Référence = lref[, 1],
                    `Inflos attractives` = lasd[, 1]) %>% 
    as_tibble %>% 
    mutate_at("Date", as_date) %>% 
    gather(Observées, Référence, `Inflos attractives`, key = Toto, value = Nombre) %>% 
    mutate(Sol = factor("Enherbement ras", levels = c("Enherbement ras",
                                                      "Paillage synthétique",
                                                      "Enherbement haut")))
to_plot_PS <- cbind(Date = date2017,
                    Observées = larves2017[, 2],
                    Référence = lref[, 2],
                    `Inflos attractives` = lasd[, 2]) %>% 
    as_tibble %>% 
    mutate_at("Date", as_date) %>% 
    gather(Observées, Référence, `Inflos attractives`, key = Toto, value = Nombre) %>% 
    mutate(Sol = factor("Paillage synthétique", levels = c("Enherbement ras",
                                                           "Paillage synthétique",
                                                           "Enherbement haut")))

to_plot_EH <- cbind(Date = date2017,
                    Observées = larves2017[, 3],
                    Référence = lref[, 3],
                    `Inflos attractives` = lasd[, 3]) %>% 
    as_tibble %>% 
    mutate_at("Date", as_date) %>% 
    gather(Observées, Référence, `Inflos attractives`, key = Toto, value = Nombre) %>% 
    mutate(Sol = factor("Enherbement haut", levels = c("Enherbement ras",
                                                       "Paillage synthétique",
                                                       "Enherbement haut")))

to_plot <- bind_rows(to_plot_ER, to_plot_PS, to_plot_EH)

to_plot %>% ggplot(aes(x = Date, y = Nombre, color = Toto)) +
    geom_point() +
    geom_line() +
    theme_bw() +
    theme(legend.title = element_blank()) +
    facet_wrap(Sol ~ .) +
    ylab("Nombre de larves")
