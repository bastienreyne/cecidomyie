## Script illustrant la différence entre le nouveau p_m et l'ancien

# Packages / data ---------------------------------------------------------

library(tidyverse)
library(magrittr)
library(mco)
library(gridExtra)
source("../model_R/model.R")
donnees <- read.csv("../data/2017_piege.csv")

inflos_ER <- donnees %>% filter(Sol == "ER") %>% pull(inflos)
inflos_PS <- donnees %>% filter(Sol == "PS") %>% pull(inflos)
inflos_EH <- donnees %>% filter(Sol == "EH") %>% pull(inflos)
inflos <- cbind(inflos_ER, inflos_PS, inflos_EH)

larves_ER <- donnees %>% filter(Sol == "ER") %>% pull(larves)
larves_PS <- donnees %>% filter(Sol == "PS") %>% pull(larves)
larves_EH <- donnees %>% filter(Sol == "EH") %>% pull(larves)
larves <- cbind(larves_ER, larves_PS, larves_EH)

load("../data/date2017.Rdata")
laps <- c(7, 7, 7, 8, 2, 5, 2, 4, 3, 5, 2, 5, 2, 5, 2, 4, 3, 4, 3)
true_index <- which(date2017 %in% true_date2017)
# Optimisation ------------------------------------------------------------

my_mae <- function(x, y) {
    n_obs <- length(x)
    sum(abs(x - y)) / n_obs
}

obj1 <- function(x, inflos) {
    larves_estimees <- dynamics(x, inflos)
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

    larves_observed <- larves[true_index, ]

    c(my_mae(larves_est[, 1], larves_observed[, 1]),
      my_mae(larves_est[, 2], larves_observed[, 2]),
      my_mae(larves_est[, 3], larves_observed[, 3]))
}

obj2 <- function(x, inflos) {
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

    larves_observed <- larves[true_index, ]

    c(my_mae(larves_est[, 1], larves_observed[, 1]),
      my_mae(larves_est[, 2], larves_observed[, 2]),
      my_mae(larves_est[, 3], larves_observed[, 3]))
}

# obj3 <- function(x, inflos) {
#   larves_estimees <- dynamics(x, inflos)
#   larvesER <- larves_estimees[, 1]
#   larvesPS <- larves_estimees[, 2]
#   larvesEH <- larves_estimees[, 3]
# 
# 
#   c(my_mae(larvesER, larves_ER),
#     my_mae(larvesPS, larves_PS),
#     my_mae(larvesEH, larves_EH))
# }
# 
# obj4 <- function(x, inflos) {
#   larves_estimees <- dynamics4(x, inflos)
#   larvesER <- larves_estimees[, 1]
#   larvesPS <- larves_estimees[, 2]
#   larvesEH <- larves_estimees[, 3]
# 
# 
#   c(my_mae(larvesER, larves_ER),
#     my_mae(larvesPS, larves_PS),
#     my_mae(larvesEH, larves_EH))
# }


res1 <- nsga2(obj1, 5, 3, inflos,
              lower.bounds = c(0, 0, 0, 0, 1),
              upper.bounds = c(1, 1, 1, 1, 70),
              popsize = 200, generations = 200)


res2 <- nsga2(obj2, 5, 3, inflos,
              lower.bounds = c(0, 0, 0, 0, 1),
              upper.bounds = c(1, 1, 1, 1, 70),
              popsize = 200, generations = 200)

# res3 <- nsga2(obj3, 5, 3, inflos,
#               lower.bounds = c(0, 0, 0, 0, 1),
#               upper.bounds = c(1, 1, 1, 1, 70),
#               popsize = 200, generations = 100)
# 
# res4 <- nsga2(obj4, 5, 3, inflos,
#               lower.bounds = rep(0, 5),
#               upper.bounds = c(1, 1, 1, 1, 70),
#               popsize = 200, generations = 100)

ind1 <- res1$value %>% as_tibble %>%
    mutate(norm = abs(V1 + V2 + V3)) %$%
    which.min(norm)

arg1 <- res1$par[ind1, ]

ind2 <- res2$value %>% as_tibble %>%
    mutate(norm = abs(V1 + V2 + V3)) %$%
    which.min(norm)

arg2 <- res2$par[ind2, ]

# ind3 <- res3$value %>% as_tibble %>%
#   mutate(norm = abs(V1 + V2 + V3)) %$%
#   which.min(norm)
# 
# arg3 <- res3$par[ind3, ]
# 
# ind4 <- res4$value %>% as_tibble %>%
#   mutate(norm = abs(V1 + V2 + V3)) %$%
#   which.min(norm)
# 
# arg4 <- res4$par[ind4, ]
# 

# Plots -------------------------------------------------------------------
  
ref <- dynamics(arg1, inflos)
new <- dynamics2(arg2, inflos)
# ref2 <- dynamics2(arg3, inflos)
# new2 <- dynamics4(arg4, inflos)

df_er <- data.frame(Date = date2017,
                    Observées = larves_ER,
                    Référence = ref[, 1],
                    Nouveau_pm = new[, 1])

er <- df_er %>% 
  gather(Observées, Référence, Nouveau_pm,
         key = statut, value = Nombre, factor_key = TRUE) %>% 
  ggplot() +
  aes(x = Date, y = Nombre, color = statut) +
  geom_line() +
  geom_point() +
  scale_color_viridis_d(end = 0.80) +
  theme_bw() +
  theme(legend.title = element_blank(), legend.position = "bottom") +
  ggtitle("Enherbement ras")

df_ps <- data.frame(Date = date2017,
                    Observées = larves_PS,
                    Référence = ref[, 2],
                    Nouveau_pm = new[, 2])

ps <- df_ps %>%
  gather(Observées, Référence, Nouveau_pm,
         key = statut, value = Nombre, factor_key = TRUE) %>% 
  ggplot() +
  aes(x = Date, y = Nombre, color = statut) +
  geom_line() +
  geom_point() +
  scale_color_viridis_d(end = 0.80) +
  theme_bw() +
  theme(legend.title = element_blank(), legend.position = "bottom") +
  ggtitle("Paillage synthétique")

df_eh <- data.frame(Date = date2017,
                    Observées = larves_EH,
                    Référence = ref[, 3],
                    Nouveau_pm = new[, 3])

eh <- df_eh %>%
  gather(Observées, Référence, Nouveau_pm,
         key = statut, value = Nombre, factor_key = TRUE) %>% 
  ggplot() +
  aes(x = Date, y = Nombre, color = statut) +
  geom_line() +
  geom_point() +
  scale_color_viridis_d(end = 0.80) +
  theme_bw() +
  theme(legend.title = element_blank(), legend.position = "bottom") +
  ggtitle("Enherbement haut")

grid.arrange(er, ps, eh, ncol = 3)

# Comparaison décomposition -----------------------------------------------

dec1 <- decomposition(arg1, inflos)

er2 <- data.frame(Date = date2017,
                  Observées = larves[, 1],
                  Référence = dec1[[1]][, 1],
                  Exogène = dec1[[2]][, 1],
                  Endogène = dec1[[3]][, 1],
                  Side = dec1[[4]][, 1]) %>% 
  gather(Exogène, Endogène, Side, key = provenance,
         value = Nombre, factor_key = TRUE) %>%
  ggplot +
  aes(x = Date) +
  geom_line(aes(y = Observées, color = "Observées")) +
  geom_point(aes(y = Observées, color = "Observées")) +
  geom_line(aes(y = Référence, color = "Référence")) +
  geom_point(aes(y = Référence, color = "Référence")) +
  geom_area(aes(y = Nombre, fill = provenance), alpha = 0.5) +
  theme_bw() +
  theme(legend.title = element_blank(), legend.position = "none") +
  ggtitle("\n\nEnherbement ras") +
  ylab("Nombre de larves") +
  xlab("Date\n\n\n\n\n\n") +
  scale_color_manual(values = c("black", "green4")) +
  guides(fill = guide_legend(ncol = 1),
         color = guide_legend(ncol = 1))

ps2 <- data.frame(Date = date2017,
                  Observées = larves[, 2],
                  Référence = dec1[[1]][, 2],
                  Exogène = dec1[[2]][, 2],
                  Endogène = dec1[[3]][, 2],
                  Side = dec1[[4]][, 2]) %>% 
  gather(Exogène, Endogène, Side, key = provenance,
         value = Nombre, factor_key = TRUE) %>%
  ggplot +
  aes(x = Date) +
  geom_line(aes(y = Observées, color = "Observées")) +
  geom_point(aes(y = Observées, color = "Observées")) +
  geom_line(aes(y = Référence, color = "Référence")) +
  geom_point(aes(y = Référence, color = "Référence")) +
  geom_area(aes(y = Nombre, fill = provenance), alpha = 0.5) +
  theme_bw() +
  theme(legend.title = element_blank(), legend.position = "bottom") +
  ggtitle("Estimation de référence\n\nPaillage synthétique") +
  ylab("Nombre de larves") +
  scale_color_manual(values = c("black", "green4")) +
  guides(fill = guide_legend(ncol = 1),
         color = guide_legend(ncol = 1))

eh2 <- data.frame(Date = date2017,
                  Observées = larves[, 3],
                  Référence = dec1[[1]][, 3],
                  Exogène = dec1[[2]][, 3],
                  Endogène = dec1[[3]][, 3],
                  Side = dec1[[4]][, 3]) %>% 
  gather(Exogène, Endogène, Side, key = provenance,
         value = Nombre, factor_key = TRUE) %>%
  ggplot +
  aes(x = Date) +
  geom_line(aes(y = Observées, color = "Observées")) +
  geom_point(aes(y = Observées, color = "Observées")) +
  geom_line(aes(y = Référence, color = "Référence")) +
  geom_point(aes(y = Référence, color = "Référence")) +
  geom_area(aes(y = Nombre, fill = provenance), alpha = 0.5) +
  theme_bw() +
  theme(legend.title = element_blank(), legend.position = "none") +
  ggtitle("\n\nEnherbement haut") +
  ylab("Nombre de larves") +
  xlab("Date\n\n\n\n\n\n") +
  scale_color_manual(values = c("black", "green4")) +
  guides(fill = guide_legend(ncol = 1),
         color = guide_legend(ncol = 1))

grid.arrange(er2, ps2, eh2, ncol = 3)


dec2 <- decomposition2(arg2, inflos)

er3 <- data.frame(Date = date2017,
                  Observées = larves[, 1],
                  Référence = dec2[[1]][, 1],
                  Exogène = dec2[[2]][, 1],
                  Endogène = dec2[[3]][, 1],
                  Side = dec2[[4]][, 1]) %>% 
  gather(Exogène, Endogène, Side, key = provenance,
         value = Nombre, factor_key = TRUE) %>%
  ggplot +
  aes(x = Date) +
  geom_line(aes(y = Observées, color = "Observées")) +
  geom_point(aes(y = Observées, color = "Observées")) +
  geom_line(aes(y = Référence, color = "Référence")) +
  geom_point(aes(y = Référence, color = "Référence")) +
  geom_area(aes(y = Nombre, fill = provenance), alpha = 0.5) +
  theme_bw() +
  theme(legend.title = element_blank(), legend.position = "none") +
  ggtitle("\n\nEnherbement ras") +
  ylab("Nombre de larves") +
  xlab("Date\n\n\n\n\n\n") +
  scale_color_manual(values = c("black", "green4")) 

ps3 <- data.frame(Date = date2017,
                  Observées = larves[, 2],
                  Référence = dec2[[1]][, 2],
                  Exogène = dec2[[2]][, 2],
                  Endogène = dec2[[3]][, 2],
                  Side = dec2[[4]][, 2]) %>% 
  gather(Exogène, Endogène, Side, key = provenance,
         value = Nombre, factor_key = TRUE) %>%
  ggplot +
  aes(x = Date) +
  geom_line(aes(y = Observées, color = "Observées")) +
  geom_point(aes(y = Observées, color = "Observées")) +
  geom_line(aes(y = Référence, color = "Référence")) +
  geom_point(aes(y = Référence, color = "Référence")) +
  geom_area(aes(y = Nombre, fill = provenance), alpha = 0.5) +
  theme_bw() +
  theme(legend.title = element_blank(), legend.position = "bottom") +
  ggtitle("Nouvelle estimation \n \n Paillage synthétique") +
  ylab("Nombre de larves") +
  scale_color_manual(values = c("black", "green4")) +
  guides(fill = guide_legend(ncol = 1),
         color = guide_legend(ncol = 1))

eh3 <- data.frame(Date = date2017,
                  Observées = larves[, 3],
                  Référence = dec2[[1]][, 3],
                  Exogène = dec2[[2]][, 3],
                  Endogène = dec2[[3]][, 3],
                  Side = dec2[[4]][, 3]) %>% 
  gather(Exogène, Endogène, Side, key = provenance,
         value = Nombre, factor_key = TRUE) %>%
  ggplot +
  aes(x = Date) +
  geom_line(aes(y = Observées, color = "Observées")) +
  geom_point(aes(y = Observées, color = "Observées")) +
  geom_line(aes(y = Référence, color = "Référence")) +
  geom_point(aes(y = Référence, color = "Référence")) +
  geom_area(aes(y = Nombre, fill = provenance), alpha = 0.5) +
  theme_bw() +
  theme(legend.title = element_blank(), legend.position = "none") +
  ggtitle("\n\nEnherbement haut") +
  ylab("Nombre de larves") +
  xlab("Date\n\n\n\n\n\n") +
  scale_color_manual(values = c("black", "green4"))

grid.arrange(er3, ps3, eh3, ncol = 3)

# Nouvelle référence ------------------------------------------------------

inflos_simulated <- read.csv("../data/attractive_simulated.csv")[3:5] %>% as.matrix()
set.seed(1848)
res_ref <- nsga2(obj2, 5, 3, inflos_simulated,
                 lower.bounds = c(0, 0, 0, 0, 1),
                 upper.bounds = c(1, 1, 1, 1, 70),
                 popsize = 200, generations = 200)

indref <- res_ref$value %>% as_tibble %>%
  mutate(norm = abs(V1 + V2 + V3)) %$%
  which.min(norm)

argref <- res_ref$par[indref, ]

estimation <- dynamics2(argref, inflos_simulated)
p1 <- data.frame(Date = date2017,
                 Observées = larves[, 1],
                 Estimées = estimation[, 1],
                 Sol = factor("Enherbement ras", 
                              levels = c("Enherbement ras", "Paillage synthétique", "Enherbement haut")))
p2 <- data.frame(Date = date2017,
                 Observées = larves[, 2],
                 Estimées = estimation[, 2],
                 Sol = factor("Paillage synthétique",
                              levels = c("Enherbement ras", "Paillage synthétique", "Enherbement haut")))
p3 <- data.frame(Date = date2017,
                 Observées = larves[, 3],
                 Estimées = estimation[, 3],
                 Sol = factor("Enherbement haut",
                              levels = c("Enherbement ras", "Paillage synthétique", "Enherbement haut")))
to_plot <- bind_rows(p1, p2, p3)
to_plot %>%
  gather(Observées, Estimées, key = statut, value = Nombre, factor_key = TRUE) %>% 
  ggplot() +
  aes(x = Date, y = Nombre, color = statut) +
  geom_point() +
  geom_line() +
  ylab("Nombre de larves") +
  theme_bw() +
  theme(legend.title = element_blank(), legend.position = "bottom") +
  facet_grid(. ~ Sol) +
  scale_color_manual(values = c("black", "green4"))



# Décompo ref -------------------------------------------------------------

decref <- decomposition2(c(0.073, 0.373, 0.223, 0, 10.788), inflos_simulated)

erref <- data.frame(Date = date2017,
                  Observées = larves[, 1],
                  Référence = decref[[1]][, 1],
                  Exogène = decref[[2]][, 1],
                  Endogène = decref[[3]][, 1],
                  Side = decref[[4]][, 1]) %>% 
  gather(Exogène, Endogène, Side, key = provenance,
         value = Nombre, factor_key = TRUE) %>%
  ggplot +
  aes(x = Date) +
  geom_line(aes(y = Observées, color = "Observées")) +
  geom_point(aes(y = Observées, color = "Observées")) +
  geom_line(aes(y = Référence, color = "Référence")) +
  geom_point(aes(y = Référence, color = "Référence")) +
  geom_area(aes(y = Nombre, fill = provenance), alpha = 0.5) +
  theme_bw() +
  theme(legend.title = element_blank(), legend.position = "none") +
  ggtitle("Enherbement ras") +
  ylab("Nombre de larves") +
  xlab("Date\n\n\n\n\n\n") +
  scale_color_manual(values = c("black", "green4")) 

psref <- data.frame(Date = date2017,
                  Observées = larves[, 2],
                  Référence = decref[[1]][, 2],
                  Exogène = decref[[2]][, 2],
                  Endogène = decref[[3]][, 2],
                  Side = decref[[4]][, 2]) %>% 
  gather(Exogène, Endogène, Side, key = provenance,
         value = Nombre, factor_key = TRUE) %>%
  ggplot +
  aes(x = Date) +
  geom_line(aes(y = Observées, color = "Observées")) +
  geom_point(aes(y = Observées, color = "Observées")) +
  geom_line(aes(y = Référence, color = "Référence")) +
  geom_point(aes(y = Référence, color = "Référence")) +
  geom_area(aes(y = Nombre, fill = provenance), alpha = 0.5) +
  theme_bw() +
  theme(legend.title = element_blank(), legend.position = "bottom") +
  ggtitle("Paillage synthétique") +
  ylab("Nombre de larves") +
  scale_color_manual(values = c("black", "green4")) +
  guides(fill = guide_legend(ncol = 1),
         color = guide_legend(ncol = 1))

ehref <- data.frame(Date = date2017,
                  Observées = larves[, 3],
                  Référence = decref[[1]][, 3],
                  Exogène = decref[[2]][, 3],
                  Endogène = decref[[3]][, 3],
                  Side = decref[[4]][, 3]) %>% 
  gather(Exogène, Endogène, Side, key = provenance,
         value = Nombre, factor_key = TRUE) %>%
  ggplot +
  aes(x = Date) +
  geom_line(aes(y = Observées, color = "Observées")) +
  geom_point(aes(y = Observées, color = "Observées")) +
  geom_line(aes(y = Référence, color = "Référence")) +
  geom_point(aes(y = Référence, color = "Référence")) +
  geom_area(aes(y = Nombre, fill = provenance), alpha = 0.5) +
  theme_bw() +
  theme(legend.title = element_blank(), legend.position = "none") +
  ggtitle("Enherbement haut") +
  ylab("Nombre de larves") +
  xlab("Date\n\n\n\n\n\n") +
  scale_color_manual(values = c("black", "green4"))

grid.arrange(erref, psref, ehref, ncol = 3)

