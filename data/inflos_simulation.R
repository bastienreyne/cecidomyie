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
    mutate(lifespan = as.numeric(death - birth)) %>% 
    mutate_at("Traitm", as.factor)

pairwise.wilcox.test(floraison$lifespan, floraison$Traitm, p.adjust.method = "holm")
## Différence significatives entre les sous-blocs

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
    FdR <- pnorm(1:50, mean_hat, sd_hat)
    
    # browser()
    inflo <- bursts
    for (t in 1:80) {
        for (j in 1:50) {
            if (t - j >= 1 & t-j <= 50){
                oi <- bursts[t-j] * (1 - FdR[j])
                inflo[t] <- inflo[t] + oi
            }
        }
    }
    inflo
}

estimation_inflos2 <- function(bursts, sousbloc) {
    
    mean_hat <- moyenne %>% select(ends_with(sousbloc)) %>% pull
    sd_hat <- stddev %>% select(ends_with(sousbloc)) %>% pull
    FdR <- pnorm(1:50, mean_hat, sd_hat)
    
    inflo <- bursts
    # browser()
    for (t in 2:80) {
        for (j in (t-1):max(1, t - 50)) {
            oi <- bursts[t-j] * (1 - FdR[j])
            inflo[t] <- inflo[t] + oi
            # if (t >= 53)
            #     browser()
        }
    }
    
    inflo
}


estimation_inflos3 <- function(bursts, sousbloc) {
    
    mean_hat <- moyenne %>% select(ends_with(sousbloc)) %>% pull
    sd_hat <- stddev %>% select(ends_with(sousbloc)) %>% pull
    FdR <- pnorm(1:50, mean_hat, sd_hat)
    
    # browser()
    inflo <- rep(NA, 80)
    inflo[1] <- bursts[1]
    for (t in 2:80) {
        inflo[t] <- bursts[t] + sum(bursts[(t-1):max(1, t - 50)] * (1 - FdR[1:min(50, t-1)]))
    }
    
    inflo
}




# Optimisation des bursts -------------------------------------------------

objectif <- function(x, sousbloc) {
    # browser()
    to_closen <- inflos %>% select(ends_with(sousbloc)) %>% pull
    simulated <- estimation_inflos3(x, sousbloc)
    n_obs <- to_closen %>% length
    
    sum(abs(simulated - to_closen)) / n_obs
}

res_ER <- optim(rep(0, 80), objectif, gr = NULL, "ER", lower = 0, upper = 6000, method = "BFGS")
# res_ER <- nsga2(objectif, 80, 1, "ER", 
#                 lower.bounds = 0, upper.bounds = 6000)
bursts_ER <- res_ER$par

res_PS <- optim(rep(0, 80), objectif, gr = NULL, "PS", lower = 0, upper = 6000, method = "L-BFGS-B")
bursts_PS <- res_PS$par

res_EH <- optim(rep(0, 80), objectif, gr = NULL, "EH", lower = 0, upper = 6000, method = "L-BFGS-B")
bursts_EH <- res_EH$par

bursts <- cbind(bursts_ER, bursts_PS, bursts_EH)
# write.csv(bursts, file = "2017_bursts_simulated")

# Inflos simulated --------------------------------------------------------

inflos_simu_ER <- estimation_inflos3(bursts_ER, "ER")
inflos_simu_PS <- estimation_inflos3(bursts_PS, "PS")
inflos_simu_EH <- estimation_inflos3(bursts_EH, "EH")

plot(inflos_simu_ER)
lines(inflos_ER)

plot(inflos_simu_PS)
lines(inflos_PS)

plot(inflos_simu_EH)
lines(inflos_EH)
