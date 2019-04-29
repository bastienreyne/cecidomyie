
# Data / packages ---------------------------------------------------------

library(tidyverse)
library(magrittr)
library(mco)
source("model1.R")
load("piege2018.Rdata")
data2017PS <- read_csv("b1.csv")
data2017ER <- read_csv("r1.csv")
data2017EH <- read_csv("h1.csv")

data2017PS2 <- read_csv("b2.csv")
data2017ER2 <- read_csv("r2.csv")
data2017EH2 <- read_csv("h2.csv")

date2018 <- unique(bloc$Date)
inflos2018 <- data.frame(cbind(ER = bloc %>% filter(Sol == "ER") %>% pull(Inflos),
                               PS = bloc %>% filter(Sol == "PS") %>% pull(Inflos),
                               EH = bloc %>% filter(Sol == "EH") %>% pull(Inflos)))
larves2018 <- data.frame(cbind(ER = bloc %>% filter(Sol == "ER") %>% pull(Larves),
                               PS = bloc %>% filter(Sol == "PS") %>% pull(Larves),
                               EH = bloc %>% filter(Sol == "EH") %>% pull(Larves)))
date2017 <- as_date(data2017ER$date[1]:data2017ER$date[length(data2017EH$date)])
inflos2017 <- data.frame(cbind(ER = data2017ER$inflos_vivantes,
                               PS = data2017PS$inflos_vivantes,
                               EH = data2017EH$inflos_vivantes))
larves2017 <- data.frame(cbind(ER = data2017ER$larves,
                               PS = data2017PS$larves,
                               EH = data2017EH$larves))

inflos2017_b2 <- data.frame(cbind(ER = data2017ER2$inflos_vivantes,
                                  PS = data2017PS2$inflos_vivantes,
                                  EH = data2017EH2$inflos_vivantes))
larves2017_b2 <- data.frame(cbind(ER = data2017ER2$larves,
                                  PS = data2017PS2$larves,
                                  EH = data2017EH2$larves))

true_date_2017 <- read_csv2(file = "Data/2017_B1_bache.csv")$date
laps2017 <- c(7, 7, 7, 8, 2, 5, 2, 4, 3, 5, 2, 5, 2, 5, 2, 4, 3, 4, 3)
true_index2017 <- which(date2017 %in% true_date_2017)

true_date_2018 <- c("2018-07-31", "2018-08-07", "2018-08-10", "2018-08-14", "2018-08-17", "2018-08-21", "2018-08-24", "2018-08-28", "2018-08-31", "2018-09-04", "2018-09-07", "2018-09-11", "2018-09-14", "2018-09-18", "2018-09-21") %>% as_date
laps2018 <- c(7, 3, 4, 3, 4, 3, 4, 3, 4, 3, 4, 3, 4, 3)
true_index2018 <- which(date2018 %in% true_date_2018)

# Cost function -----------------------------------------------------------

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

objectif <- function(x, my_function, annee = 2017, bloc = 1){
    if (annee == 2017) {
        inflo <- inflos2017 %>% as.matrix()
        larve <- larves2017
        laps <- laps2017
        true_index <- true_index2017
        if (bloc == 2) {
            inflo <- inflos2017_b2 %>% as.matrix()
            larve <- larves2017_b2
        }
    } else {
        inflo <- inflos2018 %>% as.matrix()
        laps <- laps2018
        true_index <- true_index2018
        larve <- larves2018
    }
    
    larves_estimees <- dynamiques(x[1], x[2], x[3], x[4], x[5], inflo, annee)
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
    # browser()
    larves_observed <- larve[true_index, ]
    return(c(my_function(larves_est[, 1], larves_observed[, 1]),
             my_function(larves_est[, 2], larves_observed[, 2]),
             my_function(larves_est[, 3], larves_observed[, 3])))
}

# Optimisation ------------------------------------------------------------

res <- nsga2(objectif, 5, 3, min_max, 2017,
             lower.bounds = rep(0,5),
             upper.bounds = c(10,1,1,1,10),
             popsize = 200, generations = 50)

ind_opt <- res$value %>% as_tibble %>%
    mutate(norm = abs(V1 + V2 + V3)) %$% 
    which.min(norm)

arg_opt <- res$par[ind_opt, ]


# plots -------------------------------------------------------------------
args_found <- c(0.0440855271, 0.5635581664, 0.4651371745, 0.0001322221, 0.2229496708)
plot_dynamics <- function(args, annee, bloc = 1) {
    if (annee == 2017) {
        inflo <- inflos2017 %>% as.matrix()
        larve <- larves2017
        date <- date2017
        nb.jours <- 80
        if (bloc == 2) {
            inflo <- inflos2017_b2 %>% as.matrix()
            larve <- larves2017_b2
        }
    } 
    else {
        inflo <- inflos2018 %>% as.matrix()
        larve <- larves2018
        date <- date2018
        nb.jours <- length(date2018)
    }
    
    estimed <- dynamiques(args[1], args[2], args[3], args[4], args[5], inflos = inflo, an = annee)
    estimed %<>% as.data.frame
    colnames(estimed) <- c("ER", "PS", "EH")
    estimed %<>% gather(ER, PS, EH, key = Sol, value = Larves) %>% mutate(Status = "Estimed")
    observed <- larve %>% gather(ER, PS, EH, key = Sol, value = Larves) %>% mutate(Status = "Observed")
    inflos <- inflo %>% as_tibble %>% gather(ER, PS, EH, key = Sol, value = Larves) %>%
        mutate(Status = "Inflos")
    
    to_plot <- rbind(estimed, observed, inflos) %>% cbind(Date = rep(date, 9)) %>% as.data.frame()
    to_plot$Sol_f <- factor(to_plot$Sol, levels = c("ER", "PS", "EH"))
    to_plot %>% ggplot(aes(x = Date, y = Larves, color = Status)) +
        geom_point() +
        geom_line() +
        facet_grid(Sol_f ~ .)
    
}

plot_decompo <- function(args, annee, bloc = 1) {
    if (annee == 2017) {
        inflo <- inflos2017 %>% as.matrix()
        larve <- larves2017
        date <- date2017
        nb.jours <- 80
        if (bloc == 2) {
            inflo <- inflos2017_b2 %>% as.matrix()
            larve <- larves2017_b2
        }
    } 
    else {
        inflo <- inflos2018 %>% as.matrix()
        larve <- larves2018
        date <- date2018
        nb.jours <- length(date2018)
    }
    
    
    decomposed <- dynamiques3(args[1], args[2], args[3], args[4], args[5], inflos = inflo, an = annee)
    exoER <- decomposed[[1]] %>% as_tibble %>% cbind(date, obs = larve[, 1], inflos = inflo[, 1]) %>% 
        gather(Exogene, Stay, Voisines, key = Provenance, value = Nombre)
    exoPS <- decomposed[[2]] %>% as_tibble %>% cbind(date, obs = larve[, 2], inflos = inflo[, 2]) %>% 
        gather(Exogene, Stay, Voisines, key = Provenance, value = Nombre)
    exoEH <- decomposed[[3]] %>% as_tibble %>% cbind(date, obs = larve[, 3], inflos = inflo[, 3]) %>% 
        gather(Exogene, Stay, Voisines, key = Provenance, value = Nombre)
    
    # browser()
    plot1 <- exoER %>% ggplot(aes(x = date, y = Nombre, color = Provenance)) + 
        geom_line() +
        geom_line(aes(x = date, y = Total), col = "red") +
        geom_line(aes(x = date, y = obs), col = "black") +
        geom_line(aes(x = date, y = inflos), col = "purple") +
        ylim(c(0, 6000))
    
    plot2 <- exoPS %>% ggplot(aes(x = date, y = Nombre, color = Provenance)) + 
        geom_line() +
        geom_line(aes(x = date, y = Total), col = "red") +
        geom_line(aes(x = date, y = obs), col = "black") +
        geom_line(aes(x = date, y = inflos), col = "purple") +
        ylim(c(0, 6000))
    
    plot3 <- exoEH %>% ggplot(aes(x = date, y = Nombre, color = Provenance)) + 
        geom_line() +
        geom_line(aes(x = date, y = Total), col = "red") + 
        geom_line(aes(x = date, y = obs), col = "black") +
        geom_line(aes(x = date, y = inflos), col = "purple") +
        ylim(c(0, 6000))
    
    grid.arrange(plot1, plot2, plot3, nrow = 3)
    
}
