## Script por calibrer les paramètres en utilisant les inflos attractives en entrée du modèle


# Packages / data ---------------------------------------------------------

library(tidyverse)
library(magrittr)
library(lubridate)
library(mco)
source("../model_R/model.R")
simulated <- (read.csv("../data/attractive_simulated.csv") %>% as.matrix())[, 3:5]
attractiv <- (read.csv("../data/2017_attractives.csv") %>% as.matrix())[, 3:5]
bursts <- read.csv("../data/2017_bursts_simulated.csv")[, -1] %>% as.matrix()

data2017PS <- read_csv("b1.csv")
data2017ER <- read_csv("r1.csv")
data2017EH <- read_csv("h1.csv")
larves2017 <- cbind(ER = data2017ER$larves,
                    PS = data2017PS$larves,
                    EH = data2017EH$larves)
inflos2017 <- cbind(ER = data2017ER$inflos_vivantes,
                    PS = data2017PS$inflos_vivantes,
                    EH = data2017EH$inflos_vivantes)

# data <- read.csv(file = "../data/2017_piege_bloc2.csv")
# inflos2017 <- cbind(data %>% filter(Sol == "ER") %>% pull(inflos),
#                     data %>% filter(Sol == "PS") %>% pull(inflos),
#                     data %>% filter(Sol == "EH") %>% pull(inflos))
# larves2017 <- cbind(data %>% filter(Sol == "ER") %>% pull(larves),
#                     data %>% filter(Sol == "PS") %>% pull(larves),
#                     data %>% filter(Sol == "EH") %>% pull(larves))

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

objectif <- function(x, my_function, burst){
    
    larves_estimees <- dynamics(x, burst)
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

objectif2 <- function(x, my_function, burst){
    
    larves_estimees <- dynamics(x, burst)
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
    
    larves_estimees <- dynamics(x, inflo)
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

# 
# ### FONCTION OBJECTIF CLASSIQUE
# ## Classique
# res_c <- nsga2(objectif, 5, 3, my_mae, inflos2017,
#                lower.bounds = rep(0,5),
#                upper.bounds = c(10,1,1,1,10),
#                popsize = 100, generations = 50)
# 
# ind_opt_c <- res_c$value %>% as_tibble %>%
#     mutate(norm = abs(V1 + V2 + V3)) %$% 
#     which.min(norm)
# 
# arg_opt_c <- res_c$par[ind_opt_c, ]
# 
# ## Attractive
# res_a <- nsga2(objectif, 5, 3, my_mae, attractiv,
#              lower.bounds = rep(0,5),
#              upper.bounds = c(10,1,1,1,10),
#              popsize = 100, generations = 50)
# 
# ind_opt_a <- res_a$value %>% as_tibble %>%
#     mutate(norm = abs(V1 + V2 + V3)) %$% 
#     which.min(norm)
# 
# arg_opt_a <- res_a$par[ind_opt_a, ]
# 
# ## Simulated attractive
# res_s <- nsga2(objectif, 5, 3, my_mae, simulated,
#              lower.bounds = rep(0,5),
#              upper.bounds = c(10,1,1,1,10),
#              popsize = 100, generations = 50)
# 
# ind_opt_s <- res_s$value %>% as_tibble %>%
#     mutate(norm = abs(V1 + V2 + V3)) %$% 
#     which.min(norm)
# 
# arg_opt_s <- res_s$par[ind_opt_s, ]
# 

### FONCTION OBJECTIF AVEC CRITÈRE TOTAL LARVES
res_c2 <- nsga2(objectif2, 6, 6, my_mae, bursts,
               lower.bounds = rep(0,6),
               upper.bounds = c(10,1,1,1,10, 15),
               popsize = 100, generations = 50)

ind_opt_c2 <- res_c2$value %>% as_tibble %>%
    mutate(norm = abs(V1 + V2 + V3)) %$% 
    which.min(norm)

arg_opt_cp <- res_c2$par[ind_opt_c2, ]
# 
# ## Attractive
# res_a2 <- nsga2(objectif2, 5, 6, my_mae, inflos2017,
#                lower.bounds = rep(0,5),
#                upper.bounds = c(10,1,1,1,10),
#                popsize = 100, generations = 50)
# 
# ind_opt_a2 <- res_a2$value %>% as_tibble %>%
#     mutate(norm = abs(V1 + V2 + V3)) %$% 
#     which.min(norm)
# 
# arg_opt_ap <- res_a2$par[ind_opt_a2, ]
# 
# ## Simulated attractive
# res_s2 <- nsga2(objectif2, 5, 6, my_mae, simulated,
#                lower.bounds = rep(0,5),
#                upper.bounds = c(10,1,1,1,10),
#                popsize = 100, generations = 50)
# 
# ind_opt_s2 <- res_s2$value %>% as_tibble %>%
#     mutate(norm = abs(V1 + V2 + V3)) %$% 
#     which.min(norm)
# 
# arg_opt_sp <- res_s2$par[ind_opt_s2, ]

### FONCTION OBJECTIF SOMME CUMULÉE
# res_c3 <- nsga2(objectif3, 5, 3, my_mae, inflos2017,
#                 lower.bounds = rep(0,5),
#                 upper.bounds = c(10,1,1,1,10),
#                 popsize = 100, generations = 50)
# 
# ind_opt_c3 <- res_c3$value %>% as_tibble %>%
#     mutate(norm = abs(V1 + V2 + V3)) %$% 
#     which.min(norm)
# 
# arg_opt_c3 <- res_c3$par[ind_opt_c3, ]
# 
# ## Attractive
# res_a3 <- nsga2(objectif3, 5, 3, my_mae, attractiv,
#                 lower.bounds = rep(0,5),
#                 upper.bounds = c(10,1,1,1,10),
#                 popsize = 100, generations = 50)
# 
# ind_opt_a3 <- res_a3$value %>% as_tibble %>%
#     mutate(norm = abs(V1 + V2 + V3)) %$% 
#     which.min(norm)
# 
# arg_opt_a3 <- res_a3$par[ind_opt_a3, ]
# 
# ## Simulated attractive
# res_s3 <- nsga2(objectif3, 5, 3, my_mae, simulated,
#                 lower.bounds = rep(0,5),
#                 upper.bounds = c(10,1,1,1,10),
#                 popsize = 100, generations = 50)
# 
# ind_opt_s3 <- res_s3$value %>% as_tibble %>%
#     mutate(norm = abs(V1 + V2 + V3)) %$% 
#     which.min(norm)
# 
# arg_opt_s3 <- res_s3$par[ind_opt_s3, ]

# Plots -------------------------------------------------------------------

lta <- dynamics(arg_opt_a1, attractiv)
lts <- dynamics(arg_opt_s1, simulated)
ltc <- dynamics(arg_opt_cp, inflos2017)
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
    geom_line() +
    labs(title = "Enherbement ras") +
    theme(legend.title = element_blank()) +
    scale_color_viridis_d() #+
    # geom_line(data = inflos2017 %>% as.data.frame(), mapping = aes(x = 1:80,  y = ER, color = ER))


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
    geom_line() +
    labs(title = "Paillage synthétique") +
    theme(legend.title = element_blank()) +
    scale_color_viridis_d()


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
    geom_line() +
    labs(title = "Enherbement haut") +
    theme(legend.title = element_blank()) +
    scale_color_viridis_d()


# Jeux de paramètres ------------------------------------------------------


## Répartition uniforme

# Sans critère total
arg_opt_a1 <- c(0.01801364, 0.00505962, 0.89031292, 0.92941099, 0.09046370)
arg_opt_c1 <- c(0.03007238, 0.52525497, 0.95068100, 0.00281502, 0.08674610)
arg_opt_s1 <- c(0.0419984431, 0.7516226984, 0.9219886520, 0.0001561709, 2.2840908119)

# Avec critère total
arg_opt_a2 <- c(0.03256335, 0.84715132, 0.78080009, 0.02503531, 3.74888200)
arg_opt_c2 <- c(0.01746046, 0.00246315, 0.51124275, 0.98737859, 0.09538188)
arg_opt_s2 <- c(0.0505325596, 0.4427848730, 0.8639499922, 0.0003090748, 0.1318831381)


## Eggs à 300

# Sans critere total
arg_opt_a3 <- c(0.00679058, 0.01229802, 0.49301781, 0.73270029, 0.04850367)
arg_opt_c3 <- c(0.010884917, 0.452663316, 0.713563879, 0.003251705, 0.039357014)
arg_opt_s3 <- c(0.0173154535, 0.1783031288, 0.9993313330, 0.0007271481, 0.0691272213)

# Avec critere total
arg_opt_a4 <- c(0.0233208019, 0.6425556800, 0.2538324242, 0.0005997354, 0.8682752322)
arg_opt_c4 <- c(0.008412532, 0.111776015, 0.285695100, 0.566067070, 0.045928917)
arg_opt_s4 <- c(8.61461001, 0.79078531, 0.69406992, 0.07274787, 0.03390829)


## Proba pupaison à 1

# Sans critère total
arg_opt_a5 <- c(0.02932499, 0.03978466, 0.60390726, 0.37261187, 0.08746912)
arg_opt_c5 <- c(0.02850888, 0.75757426, 0.66803412, 0.02378983, 0.29171042)
arg_opt_s5 <- c(0.033661797, 0.370267280, 0.960677240, 0.003260751, 0.116919413)

# Avec critere total
arg_opt_a6 <- c(0.020797563, 0.959811435, 0.759315400, 0.002879797, 7.680256993)
arg_opt_c6 <- c(0.016144619, 0.001365966, 0.760825919, 0.936291489, 0.099359910)
arg_opt_s6 <- c(0.06010847, 0.89004149, 0.38156574, 0.04056860, 9.69565258)


## Proba pupaison à 0.5

# Sans critère total
arg_opt_a7 <- c(0.025982348, 0.008423767, 0.999091790, 0.897250015, 0.099737184)
arg_opt_c7 <- c(0.0380917997, 0.7880245997, 0.9969081266, 0.0004875849, 0.4817037792)
arg_opt_s7 <- c(0.0517900625, 0.6454180550, 0.9554480565, 0.0005802959, 3.4994471209)

# Avec critere total
arg_opt_a8 <- c(0.02617125, 0.46496818, 0.95065874, 0.02614878, 0.05764266)
arg_opt_c8 <- c(0.03122670, 0.13197789, 0.26399546, 0.79400434, 0.08940868)
arg_opt_s8 <- c(0.068887231, 0.246141382, 0.385183306, 0.008148933, 8.740040966)


## Duree pupaion à 6

# Sans critere total
arg_opt_a9 <- c(0.02098081, 0.00576337, 0.78445146, 0.82230381, 0.11021389)
arg_opt_c9 <- c(0.0334146247, 0.7971851417, 0.8184765251, 0.0004132678, 4.0526946829)
arg_opt_s9 <- c(0.0459845844, 0.4923276480, 0.8434925497, 0.0005568149, 0.1147187239)

# Avec critere total
arg_opt_a10 <- c(0.03209081, 0.99362842, 0.99297217, 0.07447543, 7.03321210)
arg_opt_c10 <- c(0.024703332, 0.009795314, 0.681265524, 0.696271988, 0.111912428)
arg_opt_s10 <- c(0.05387376, 0.58427221, 0.67736999, 0.01086949, 5.06078461)