## Script pour tester une éventuelle amélioration du modèle grâce aux inflos modifiées.

# Packages / data ---------------------------------------------------------

library(tidyverse)
library(magrittr)
library(lubridate)
library(mco)
library(gridExtra)
source(file = "../model_R/model1.R")
# source("model1.R")
inflos_corrected <- (read.csv(file = "../data/corrected.csv") %>% as.matrix)[, 2:4]
inflos_simuled <- (read.csv(file = "../data/simu_python.csv") %>% as.matrix)[, 2:4]

date2017 <- read.csv("b1.csv")$date %>% as_date
true_date_2017 <- read_csv2(file = "Data/2017_B1_bache.csv")$date
laps2017 <- c(7, 7, 7, 8, 2, 5, 2, 4, 3, 5, 2, 5, 2, 5, 2, 4, 3, 4, 3)
true_index2017 <- which(date2017 %in% true_date_2017)

bloc1 <- read_csv("../data/2017_piege.csv")
larves <- cbind(bloc1 %>% filter(Sol == "ER") %>% pull(larves),
                bloc1 %>% filter(Sol == "PS") %>% pull(larves),
                bloc1 %>% filter(Sol == "EH") %>% pull(larves))

inflos <- cbind(bloc1 %>% filter(Sol == "ER") %>% pull(inflos),
                bloc1 %>% filter(Sol == "PS") %>% pull(inflos),
                bloc1 %>% filter(Sol == "EH") %>% pull(inflos))


# Calibration -------------------------------------------------------------

my_mae <- function(x, y) {
    n_obs <- length(x)
    sum(abs(x - y)) / n_obs
}

objectif <- function(x, my_function, inflo){
        
    larves_estimees <- dynamiques(x[1], x[2], x[3], x[4], x[5], inflo %>% as.matrix)
    larvesER <- larves_estimees[, 1]
    larvesPS <- larves_estimees[, 2]
    larvesEH <- larves_estimees[, 3]
    
    larves_est <- matrix(NA, nrow = length(laps2017), ncol = 3)
    for (i in 1:length(laps2017)) {
        indices <- (true_index2017[i] - laps2017[i] + 1):true_index2017[i]
        larves_est[i, ] <- c(mean(larvesER[indices]),
                             mean(larvesPS[indices]),
                             mean(larvesEH[indices]))
    }
    # browser()
    larves_observed <- larves[true_index2017, ]
    return(c(my_function(cumsum(larves_est[, 1]), cumsum(larves_observed[, 1])),
             my_function(cumsum(larves_est[, 2]), cumsum(larves_observed[, 2])),
             my_function(cumsum(larves_est[, 3]), cumsum(larves_observed[, 3]))))
}

## Corrigées
res <- nsga2(objectif, 5, 3, my_mae, inflos_corrected,
             lower.bounds = rep(0,5),
             upper.bounds = c(10,1,1,1,10),
             popsize = 200, generations = 50)

ind_opt <- res$value %>% as_tibble %>%
    mutate(norm = abs(V1 + V2 + V3)) %$% 
    which.min(norm)

arg_opt <- res$par[ind_opt, ]

## Simulées
res2 <- nsga2(objectif, 5, 3, my_mae, inflos_simuled,
              lower.bounds = rep(0,5),
              upper.bounds = c(10,1,1,1,10),
              popsize = 200, generations = 50)

ind_opt2 <- res2$value %>% as_tibble %>%
    mutate(norm = abs(V1 + V2 + V3)) %$% 
    which.min(norm)

arg_opt2 <- res2$par[ind_opt, ]

## Normales
res3 <- nsga2(objectif, 5, 3, my_mae, inflos_simuled,
              lower.bounds = rep(0,5),
              upper.bounds = c(10,1,1,1,10),
              popsize = 200, generations = 50)

ind_opt3 <- res3$value %>% as_tibble %>%
    mutate(norm = abs(V1 + V2 + V3)) %$% 
    which.min(norm)

arg_opt3 <- res3$par[ind_opt, ]

# Plots -------------------------------------------------------------------

larves_decalees <- dynamiques(arg_opt[1], arg_opt[2],
                              arg_opt[3], arg_opt[4],
                              arg_opt[5], inflos_corrected)

larves_simulees <- dynamiques(arg_opt2[1], arg_opt2[2],
                              arg_opt2[3], arg_opt2[4],
                              arg_opt2[5], inflos_simuled)

larves_base <- dynamiques(arg_opt3[1], arg_opt3[2],
                          arg_opt3[3], arg_opt3[4],
                          arg_opt3[5], inflos_simuled)

larvesER <- cbind(Date = date2017,
                  Observées = larves[, 1],
                  Comme_dab = larves_base[, 1],
                  Avec_inflos_corrigées = larves_decalees[, 1],
                  Avec_inflos_simulées = larves_simulees[, 1], 
                  Inflos = inflos[, 1],
                  Inflos_corrigées = inflos_corrected[, 1],
                  Inflos_simulées = inflos_simuled[, 1]) %>% 
    as_tibble() %>% 
    gather(Observées, Comme_dab, Avec_inflos_corrigées,
           Avec_inflos_simulées, key = Larves, value = Nombre) %>% 
    gather(Inflos, Inflos_corrigées, Inflos_simulées, key = Inflos, value = Number)

larvesPS <- cbind(Date = date2017,
                  Observées = larves[, 2],
                  Comme_dab = larves_base[, 2],
                  Avec_inflos_corrigées = larves_decalees[, 2],
                  Avec_inflos_simulées = larves_simulees[, 2],
                  Inflos = inflos[, 2],
                  Inflos_corrigées = inflos_corrected[, 2],
                  Inflos_simulées = inflos_simuled[, 2]) %>% 
    as_tibble() %>% 
    gather(Observées, Comme_dab, Avec_inflos_corrigées,
           Avec_inflos_simulées, key = Larves, value = Nombre) %>% 
    gather(Inflos, Inflos_corrigées, Inflos_simulées, key = Inflos, value = Number)

larvesEH <- cbind(Date = date2017,
                  Observées = larves[, 3],
                  Comme_dab = larves_base[, 3],
                  Avec_inflos_corrigées = larves_decalees[, 3],
                  Avec_inflos_simulées = larves_simulees[, 3],
                  Inflos = inflos[, 3],
                  Inflos_corrigées = inflos_corrected[, 3],
                  Inflos_simulées = inflos_simuled[, 3]) %>% 
    as_tibble() %>% 
    gather(Observées, Comme_dab, Avec_inflos_corrigées,
           Avec_inflos_simulées, key = Larves, value = Nombre) %>% 
    gather(Inflos, Inflos_corrigées, Inflos_simulées, key = Inflos, value = Number)

to_plot_ER <- cbind(Date = date2017,
                    ITC = inflos_corrected[, 1])

lplot_ER <- larvesER %>% ggplot(aes(x = Date, y = Nombre, color = Larves)) +
    geom_point() +
    geom_line(linetype = "dashed") +
    # geom_line(aes(y = Number, color = Inflos)) +
    # scale_color_viridis_d() +
    ylim(c(0, 3000)) +
    labs(title = "Enherbement ras") +
    ylab("Nombre de larves") +
    xlab("Date") +
    theme(legend.title = element_blank(), legend.text = element_text(size = 14),
          axis.title = element_text(size = 14), axis.text = element_text(size = 13), 
          legend.position="bottom") 

lplot_PS <- larvesPS %>% ggplot(aes(x = Date, y = Nombre, color = Larves)) +
    geom_point() +
    geom_line(linetype = "dashed") +
    # geom_line(aes(y = Number, color = Inflos)) +
    ylim(c(0, 3000)) +
    labs(title = "Paillage synthétique") +
    ylab("Nombre de larves") +
    xlab("Date") +
    theme(legend.title = element_blank(), legend.text = element_text(size = 14),
          axis.title = element_text(size = 14), axis.text = element_text(size = 13),
          legend.position="bottom") 


lplot_EH <- larvesEH %>% ggplot(aes(x = Date, y = Nombre, color = Larves)) +
    geom_point() +
    geom_line(linetype = "dashed") +
    # geom_line(aes(y = Number, color = Inflos)) +
    ylim(c(0, 3000)) +
    labs(title = "Enherbement haut") +
    ylab("Nombre de larves") +
    xlab("Date") +
    theme(legend.title = element_blank(), legend.text = element_text(size = 14),
          axis.title = element_text(size = 14), axis.text = element_text(size = 13),
          legend.position="bottom") 


grid.arrange(plot_ER, plot_PS, plot_EH, nrow = 3)
