## Script qui simule des dates de débourrements pour coller au mieux aux dynamiques observées

# Packages / data ---------------------------------------------------------

library(tidyverse)
library(magrittr)
library(mco)

data <- read_csv("2017_piege.csv")

inflos_ER <- data %>% filter(Sol == "ER") %>% pull(inflos)
inflos_PS <- data %>% filter(Sol == "PS") %>% pull(inflos)
inflos_EH <- data %>% filter(Sol == "EH") %>% pull(inflos)
inflos <- data.frame(inflos_ER, inflos_PS, inflos_EH)

floraison <- read_csv("2017_floraison_dates.csv") %>% 
    mutate(lifespan = as.numeric(death - birth))

floraison_ER <- floraison %>% filter(Traitm == "ER") %>% pull(lifespan) %>% na.omit
mean_ER <- floraison_ER %>% mean
sd_ER <- floraison_ER %>% sd

floraison_PS <- floraison %>% filter(Traitm == "PS") %>% pull(lifespan) %>% na.omit
mean_PS <- floraison_PS %>% mean
sd_PS <- floraison_PS %>% sd

floraison_EH <- floraison %>% filter(Traitm == "EH") %>% pull(lifespan) %>% na.omit
mean_EH <- floraison_EH %>% mean
sd_EH <- floraison_EH %>% sd

moyenne <- data.frame(mean_ER, mean_PS, mean_EH)
stddev <- data.frame(sd_ER, sd_PS, sd_EH)

# Fonctions inflos --------------------------------------------------------

estimation_inflos <- function(bursts, sousbloc) {
    
    mean_hat <- moyenne %>% select(ends_with(sousbloc)) %>% pull
    sd_hat <- stddev %>% select(ends_with(sousbloc)) %>% pull
    # browser()
    inflo <- bursts
    FdR <- 1 - pnorm(1:50, mean_hat, sd_hat)
    for (t in 2:length(inflos)) {
        inflo[t] <- inflo[t] + sum(bursts[1:(t-1)] * (1 - FdR[1:(t-1)]))
    }
    
    inflo
}


# Optimisation des bursts -------------------------------------------------

objectif <- function(x, sousbloc) {
    # browser()
    to_closen <- inflos %>% select(ends_with(sousbloc)) %>% pull
    simulated <- estimation_inflos(x, sousbloc)
    n_obs <- to_closen %>% length
    
    sum(abs(simulated - to_closen)) / n_obs
}

res_ER <- nsga2(objectif, idim = 80, odim = 1, "ER",
                lower.bounds = rep(10, 80), upper.bounds = rep(6000, 80))

res_PS <- nsga2(objectif, idim = 80, odim = 1, "PS",
                lower.bounds = rep(10, 80), upper.bounds = rep(6000, 80))

res_ER <- nsga2(objectif, idim = 80, odim = 1, "EH",
                lower.bounds = rep(10, 80), upper.bounds = rep(6000, 80))

