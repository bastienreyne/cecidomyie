
bursts <- read.csv("../data/2017_bursts_simulated.csv", row.names = 1) %>% as.matrix

# Simulation --------------------------------------------------------------

mu <- 29
sigma <- 14
CDF_norm <- pnorm(1:50, mu, sigma)

inflosCDE <- function(burst, dA = 16) {
    inflos <- rep(NA, length(burst))
    inflos[1] <- burst[1]
    days <- 1:(length(burst)+3) ## Pour arriver au 80 jours avec les burst du dataset 1
    for (t in days[-1]) {
        temp <- 0
        for (j in 1:min(dA, t-1)) {
            temp <- temp + burst[t-j] * (1 - CDF_norm[j]) 
        } 
        inflos[t] <- burst[t] + temp
    }
    inflos
}


inflosCDE_ER <- inflosCDE(bursts[, 1])
inflosCDE_PS <- inflosCDE(bursts[, 2])
inflosCDE_EH <- inflosCDE(bursts[, 3])

inflosCDE_bloc1 <- cbind(inflosCDE_ER[1:80],
                         inflosCDE_PS[1:80],
                         inflosCDE_EH[1:80])

# Comparasion with alive --------------------------------------------------

inflos_CDE_obs <- rbind(inflosCDE_bloc1, inflos_obs) %>% 
    as.data.frame()
colnames(inflos_CDE_obs) <- c("Enherbement ras", "Paillage synthétique", "Enherbement haut")
inflos_CDE_obs <- inflos_CDE_obs %>% 
    mutate(Statut = factor(rep(c("CDE", "alive"), each = 80),
                           levels = c("CDE", "alive"))) %>% 
    mutate(Date = rep(date2017, 2)) %>% 
    gather("Enherbement ras", "Paillage synthétique", "Enherbement haut",
           key = sol, value = inflos, factor_key = TRUE) 
inflos_CDE_obs %>% 
    ggplot +
    aes(x = Date, y = inflos, color = Statut) +
    geom_line(lwd = 0.75) +
    theme_bw() +
    facet_grid(. ~ sol) +
    scale_color_discrete(labels = c("Attractives", "Vivantes")) +
    labs(color = element_text("Inflorescences")) +
    ylab("Nombre d'inflorescences") +
    theme(legend.position = "bottom")
        