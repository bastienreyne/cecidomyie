
# Packages / data ---------------------------------------------------------

library(tidyverse)
library(magrittr)
library(lubridate)
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

bloc %>% ggplot(aes(x = Date, y = Inflos)) +
    geom_line() +
    geom_point(aes(y = Larves)) +
    geom_segment(aes(xend = Date, y = Larves, yend = 0)) +
    facet_grid(Sol ~ .)



# Cost function -----------------------------------------------------------

my_mae <- function(x, y) {
    n <- length(x)
    return(sum(abs(x - y)) / n)
}

my_rmse <- function(x, y){
    n <- length(x)
    ans <- sqrt(sum((x - y)^2) / n)
    return(ans)
}

min_max <- function(x, y) {
    max(abs(x - y))
}


# Optimisation ------------------------------------------------------------

objectif <- function(x, my_function, annee, bloc = 1){
    if (annee == 2017) {
        inflo <- inflos2017 %>% as.matrix()
        larve <- larves2017
        if (bloc == 2) {
            inflo <- inflos2017_b2 %>% as.matrix()
            larve <- larves2017_b2
        }
    } else {
        inflo <- inflos2018 %>% as.matrix()
        larve <- larves2018
    }
    
    larves_estimees <- dynamiques(x[1], x[2], x[3], x[4], x[5], inflo, annee)
    larvesER <- larves_estimees[, 1]
    larvesPS <- larves_estimees[, 2]
    larvesEH <- larves_estimees[, 3]
    
    return(c(my_function(larvesER, larve[, 1]),
             my_function(larvesPS, larve[, 2]),
             my_function(larvesEH, larve[, 3])))
}

res <- nsga2(objectif, 5, 3, min_max, 2017, 1, lower.bounds = rep(0,5),
             upper.bounds = c(1000, 1, 1, 1, 1000), popsize = 200,
             generations = 100)

toto <- res$value %>% as_tibble %>% mutate(norm1 = V1 + V2 + V3)
ind_opt <- which.min(toto$norm1)[1]
params_opt <- res$par[ind_opt, ]
params_opt

# Plot --------------------------------------------------------------------


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
    
    
    decomposed <- dynamiques3(args[1], args[2], args[3], args[4], args[5], inflos = inflo, an = annee)
    exoER <- decomposed[[1]] %>% as_tibble %>% cbind(date) %>% 
        gather(Exogene, Stay, Voisines, key = Provenance, value = Nombre)
    exoPS <- decomposed[[2]] %>% as_tibble %>% cbind(date) %>% 
        gather(Exogene, Stay, Voisines, key = Provenance, value = Nombre)
    exoEH <- decomposed[[3]] %>% as_tibble %>% cbind(date) %>% 
        gather(Exogene, Stay, Voisines, key = Provenance, value = Nombre)
    
    plot1 <- exoER %>% ggplot(aes(x = date, y = Nombre, color = Provenance)) + 
        geom_line() +
        geom_line(aes(x = date, y = Total))
    
    plot2 <- exoPS %>% ggplot(aes(x = date, y = Nombre, color = Provenance)) + 
        geom_line() +
        geom_line(aes(x = date, y = Total))
    
    plot3 <- exoEH %>% ggplot(aes(x = date, y = Nombre, color = Provenance)) + 
        geom_line() +
        geom_line(aes(x = date, y = Total))
    
    grid.arrange(plot1, plot2, plot3, nrow = 3)

}

params_laurie <- c(0.59, 0.41, 0.98, 0.44, 0.84)
params_mae2017 <- c(2.487529e-02, 4.465644e-01, 9.997824e-01, 8.793165e-05, 1.060483e-01)
params_mae2018 <- c(19.53019298, 0.87941299, 0.39420751, 0.98798969, 0.01177052)
params_rmse2018 <- c(652.31060875, 0.84053631, 0.45666491, 0.01986522, 0.02327967)
params_rmse2017 <- c(2.708157e-02, 4.312741e-01, 9.181171e-01, 6.561647e-06, 1.108418e-01)
params_rmse2 <- c(2.959347e-02, 8.707611e-01, 9.639234e-01, 2.338834e-05, 1.831456e+01)
params_mae2 <- c(0.035736417, 0.212285647, 0.956419566, 0.006380339, 0.079693108)
params_mae_b2 <- c(1.465108e+02, 9.999492e-01, 2.051613e-04, 3.606617e-01, 3.548404e-02)

# Both years --------------------------------------------------------------

objectif2 <- function(x, my_function) {
    
    r2017 <- objectif(x, my_function, 2017)
    r2018 <- objectif(x, my_function, 2018)
    c(r2017, r2018)
    # larves_estimees <- dynamiques(x[1], x[2], x[3], x[4], x[5], inflos2017, 2017)
    # larvesER <- larves_estimees[, 1]
    # larvesPS <- larves_estimees[, 2]
    # larvesEH <- larves_estimees[, 3]
    # 
    # larves_estimees2018 <- dynamiques(x[1], x[2], x[3], x[4], x[5], inflos2018, 2018)
    # larvesER2018 <- larves_estimees2018[, 1]
    # larvesPS2018 <- larves_estimees2018[, 2]
    # larvesEH2018 <- larves_estimees2018[, 3]
    # 
    # return(c(my_function(larvesER, larves2017[, 1]),
    #          my_function(larvesPS, larves2017[, 2]),
    #          my_function(larvesEH, larves2017[, 3]),
    #          my_function(larvesER2018, larves2018[, 1]),
    #          my_function(larvesPS2018, larves2018[, 2]),
    #          my_function(larvesEH2018, larves2018[, 3])))
}

res <- nsga2(objectif2, 5, 6, my_mae, lower.bounds = rep(0,5),
             upper.bounds = c(1000, 1, 1, 1, 1000), popsize = 200,
             generations = 100)

toto <- res$value %>% as_tibble %>% mutate(norm1 = V1 + V2 + V3)
ind_opt <- which.min(toto$norm1)[1]
params_opt <- res$par[ind_opt, ]
params_opt



plot_dynamics2 <- function(args, annee, bloc = 1) {
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
    
    estimed <- dynamiques2(args[1], args[2], args[3], args[4], args[5], inflos = inflo, an = annee)
    estimed %<>% as.data.frame
    colnames(estimed) <- c("ER", "PS", "EH")
    # browser()
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
