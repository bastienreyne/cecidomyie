## Script pour corriger les dynamiques du dataset 1 pour le bloc 1

# Packages ----------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(mco)

dataset2 <- read_csv("2017_piege.csv", col_names = TRUE)
date_dataset2 <- unique(dataset2$date)
inflos2 <- cbind(dataset2 %>% filter(Sol == "ER") %>% pull(inflos),
                 dataset2 %>% filter(Sol == "PS") %>% pull(inflos),
                 dataset2 %>% filter(Sol == "EH") %>% pull(inflos))

dataset1_inflos <- read_csv("2017_inflos_dataset1_bloc1.csv")
date_dataset1 <- dataset1_inflos$date
inflos1 <- cbind(dataset1_inflos %>% pull(inflosER),
                 dataset1_inflos %>% pull(inflosPS),
                 dataset1_inflos %>% pull(inflosEH))

dataset1_bursts_deads <- read_csv("2017_bursts_death_bloc1.csv")


# Fonction objectifs ------------------------------------------------------

first_date <- which(date_dataset1 %in% date_dataset2)[1]

my_rmse <- function(x, y) {
    n_obs <- length(x)
    sqrt(sum((x - y)^2) / n_obs ) 
}

## Correction ER
inflos_rawER <- inflos1[first_date:nrow(inflos1), 1]
ecartER <- inflos_rawER[49] - inflos_rawER[50]

obj_ds1_ER <- function(x) {
    
    poids <- x[1:50]
    morts <- poids * ecartER / sum(poids)
    
    inflos_new <- inflos_rawER
    inflos_new[1:49] <- inflos_new[1:49] - cumsum(morts)[1:49]
    
    my_rmse(x[51] * inflos_new, inflos2[1:77, 1])
}

## Correction PS
inflos_rawPS <- inflos1[first_date:nrow(inflos1), 2]
ecartPS <- inflos_rawPS[49] - inflos_rawPS[50]

obj_ds1_PS <- function(x) {
    
    poids <- x[1:50]
    morts <- poids * ecartPS / sum(poids)
    
    inflos_new <- inflos_rawPS
    inflos_new[1:49] <- inflos_new[1:49] - cumsum(morts)[1:49]
    
    my_rmse(x[51] * inflos_new, inflos2[1:77, 2])
}

## Correction EH
inflos_rawEH <- inflos1[first_date:nrow(inflos1), 3]
ecartEH <- inflos_rawEH[49] - inflos_rawEH[50]

obj_ds1_EH <- function(x) {
    
    poids <- x[1:50]
    morts <- poids * ecartEH / sum(poids)
    
    inflos_new <- inflos_rawEH
    inflos_new[1:49] <- inflos_new[1:49] - cumsum(morts)[1:49]
    
    my_rmse(x[51] * inflos_new, inflos2[1:77, 3])
}


# Et c'est parti ! --------------------------------------------------------

resER <- nsga2(obj_ds1_ER, 51, 1,
               lower.bounds = rep(0, 51),
               upper.bounds = c(rep(100, 50), 6000),
               popsize = 20,
               generations = 500)

resPS <- nsga2(obj_ds1_PS, 51, 1,
               lower.bounds = rep(0, 51),
               upper.bounds = c(rep(100, 50), 6000),
               popsize = 20,
               generations = 500)

resEH <- nsga2(obj_ds1_EH, 51, 1,
               lower.bounds = rep(0, 51),
               upper.bounds = c(rep(100, 50), 6000),
               popsize = 20,
               generations = 500)

## ER
my_x_ER <- resER$par[1, 1:50]
my_alpha_ER <- resER$par[1, 51]

deads_ER <- my_x_ER * ecartER / sum(my_x_ER)

inflos_ER <- inflos_rawER
inflos_ER[1:49] <- inflos_ER[1:49] - cumsum(deads_ER)[1:49]
inflos_ER <- inflos_ER * my_alpha_ER

## PS
my_x_PS <- resPS$par[1, 1:50]
my_alpha_PS <- resPS$par[1, 51]

deads_PS <- my_x_PS * ecartPS / sum(my_x_PS)

inflos_PS <- inflos_rawPS
inflos_PS[1:49] <- inflos_PS[1:49] - cumsum(deads_PS)[1:49]
inflos_PS <- inflos_PS * my_alpha_PS

## EH
my_x_EH <- resEH$par[1, 1:50]
my_alpha_EH <- resEH$par[1, 51]

deads_EH <- my_x_EH * ecartEH / sum(my_x_EH)

inflos_EH <- inflos_rawEH
inflos_EH[1:49] <- inflos_EH[1:49] - cumsum(deads_EH)[1:49]
inflos_EH <- inflos_EH * my_alpha_EH


inflos_corrected <- cbind(inflos_ER, inflos_PS, inflos_EH)

# write.csv(inflos_corrected, file = "2017_inflos_corrected_bloc1.csv")

inflos1_ech <- cbind(inflos_rawER * my_alpha_ER,
                     inflos_rawPS * my_alpha_PS,
                     inflos_rawEH * my_alpha_EH)

# write.csv(inflos1_ech, "2017_inflos1_echelle_dataset1_bloc1.csv")


# Débourrements à l'échelle -----------------------------------------------


bursts_echelle <- data.frame(date = date_dataset1,
                             burstER = my_alpha_ER * dataset1_bursts_deads$burstER,
                             burstPS = my_alpha_PS * dataset1_bursts_deads$burstPS,
                             burstEH = my_alpha_EH * dataset1_bursts_deads$burstEH)

# write.csv(bursts_echelle, file = "2017_bursts_echelle_bloc1.csv")
