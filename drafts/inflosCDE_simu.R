## Script pour comparer les inflos CDE simulées à partir des débourrements observés avec celles  simulées avec
## les débourrement simulés

# Packages / data ---------------------------------------------------------

library(tidyverse)
library(magrittr)
load("../data/Btc1.Rdata") ## Débourrements observés mis à l'échelle
load("../data/Bts.Rdata") ## Débourrements simulés      
load("../data/date2017.Rdata")
day <- 1:80
mu <- 29  
sigma <- 14  
Fnorm <- cumsum(dnorm(c(1:50), mu, sigma))

# Simulation (débourrements simulés) --------------------------------------

I_fun2 <- function(fB, dA = 16) {
    nI <- rep(NA, 80)
    nI[1] <- fB[1]
    for (t in day[-1]) {   
        temp <- 0
        for (j in 1:min(dA, t-1)) {
            temp <- temp + fB[t-j] * (1 - Fnorm[j]) 
        } 
        nI[t] <- fB[t] + temp
    }
    nI
}

inflosCDE16j_sim_ER <- I_fun2(Bts_ER)
inflosCDE16j_sim_PS <- I_fun2(Bts_PS)
inflosCDE16j_sim_EH <- I_fun2(Bts_EH)

# Simulation (débourrements observés) -------------------------------------

inflosCDE16j_obs_ER <- I_fun2(Btc1_ER)
inflosCDE16j_obs_PS <- I_fun2(Btc1_PS)
inflosCDE16j_obs_EH <- I_fun2(Btc1_EH)

# Comparaison -------------------------------------------------------------

df_to_plot <- data.frame(date = rep(date2017, 4),
                    inflos = c(inflosCDE16j_obs_ER,
                               inflosCDE16j_obs_PS,
                               #inflosCDE16j_obs_EH,
                               inflosCDE16j_sim_ER,
                               inflosCDE16j_sim_PS),
                               #inflosCDE16j_sim_EH),
                    burst = rep(c("obs", "sim"), each = 160),
                    sol = factor(rep(c("Enherbement ras", "Paillage synthétique"),
                                     each = 80, times = 2),
                                 levels = c("Enherbement ras", "Paillage synthétique",
                                            "Enherbement haut")))

df_to_plot %>%
    ggplot +
    aes(x = date, y = inflos, color = burst) +
    theme_bw() +
    geom_line(lwd = 0.75) +
    facet_grid(. ~ sol) +
    theme(legend.position = "bottom") +
    xlab("Date") +
    ylab("Inflorescences C/D/E (16 jours)") +
    scale_color_discrete(labels = c("Observés", "Simulés"), name = "Débourrements")

# Save data ---------------------------------------------------------------

inflosCDE <- cbind(inflosCDE16j_obs_ER, inflosCDE16j_obs_PS, inflosCDE16j_sim_EH)
inflosCDEsim <- cbind(inflosCDE16j_sim_ER, inflosCDE16j_sim_PS, inflosCDE16j_sim_EH)

# save(inflosCDE, inflosCDEsim, file = "inflosCDE.Rdata")
