
# packages / data ---------------------------------------------------------

library(tidyverse)
library(magrittr)
library(mco)
library(emoa)
source("../model_R/model.R")
source("../model_R/objectif.R")

# NSGA-II -----------------------------------------------------------------

n_repet <- 5
taille_pop <- 100
pareto_front4 <- matrix(NA, nrow = n_repet * taille_pop, ncol = 3)
parameters4 <- matrix(NA, nrow = n_repet * taille_pop, ncol = 5)

constraint_function <- function(x) {
    larves_estimees <- dynamics2(x, inflos_simulated)
    larvesPS <- larves_estimees[, 2]
    
    max(larvesPS) - 1500
}

for (iter in 1:n_repet) {
    tmp <- nsga2(obj, 5, 3,
                 lower.bounds = c(0, 0, 0, 0, 1),
                 upper.bounds = c(1, 1, 1, 1, 70),
                 constraints = constraint_function,
                 popsize = taille_pop, generations = 100)
    pareto_front4[((iter - 1) * taille_pop + 1):((iter - 1) * taille_pop + taille_pop), ] <- 
        tmp$value
    parameters4[((iter - 1) * taille_pop + 1):((iter - 1) * taille_pop + taille_pop), ] <- tmp$par
}

cor(pareto_front2[, 1], pareto_front2[, 2])
cor(pareto_front2[, 1], pareto_front2[, 3])
cor(pareto_front2[, 2], pareto_front2[, 3])


# Boxplot -----------------------------------------------------------------

colnames(pareto_front3) <- c("ER", "EH", "Critère")
pareto_front2 %>% 
    as.data.frame %>% 
    gather(ER, PS, EH, key = Sol, value = NRMSE, factor_key = TRUE) %>% 
    ggplot +
    aes(x = Sol, y = NRMSE/100) +
    geom_boxplot() +
    ylab("NRMSE") +
    xlab("Sous-bloc") +
    theme_bw()

# Pareto ------------------------------------------------------------------

pareto_front3[!is_dominated(t(pareto_front3)), ] %>% 
    as.data.frame %>% 
    mutate(Distance = sqrt(ER^2 + EH^2 + Critère^2)) %$%
    which.min(Distance)
params <- parameters3[!is_dominated(t(pareto_front3)), ]
argdist <- params[261, ]
argmed <- params[156, ]
arg3s <- params[201, ]
argdistmax <- params[190, ]
argpasmal <- params[2, ] # 0.04658174 0.78545289 0.49709368 0.87663688 3.71755689
new_front <- pareto_front3[!is_dominated(t(pareto_front3)), ]

# Plots -------------------------------------------------------------------

argref <- c(0.046, 0.919, 0.856, 0.440, 30.131)

arg7 <- good_sol[5, ]

estimation <- dynamics2(arg7, inflos_simulated)
proportion <- decomposition2(arg7, inflos_simulated)

ploter <- data.frame(Date = date2017,
                     Sol = factor("Enherbement ras", levels = c("Enherbement ras",
                                                                "Paillage synthétique",
                                                                "Enherbement haut")),
                     method = factor("Modification", levels = c("Référence", "Modification")),
                     Observation = larves1,
                     Estimation = estimation[, 1],
                     Endogène = proportion[[3]][, 1],
                     Side = proportion[[4]][, 1],
                     Exogène = proportion[[2]][, 1]) %>% 
    gather(Exogène, Side, Endogène, key = prov, value = prop, factor_key = TRUE)

plotps <- data.frame(Date = date2017,
                     Sol = factor("Paillage synthétique", levels = c("Enherbement ras",
                                                                     "Paillage synthétique",
                                                                     "Enherbement haut")),
                     method = factor("Modification", levels = c("Référence", "Modification")),
                     Observation = larves2,
                     Estimation = estimation[, 2],
                     Endogène = proportion[[3]][, 2],
                     Side = proportion[[4]][, 2],
                     Exogène = proportion[[2]][, 2]) %>% 
    gather(Exogène, Side, Endogène, key = prov, value = prop, factor_key = TRUE)

ploteh <- data.frame(Date = date2017,
                     Sol = factor("Enherbement haut", levels = c("Enherbement ras",
                                                                 "Paillage synthétique",
                                                                 "Enherbement haut")),
                     method = factor("Modification", levels = c("Référence", "Modification")),
                     Observation = larves3,
                     Estimation = estimation[, 3],
                     Endogène = proportion[[3]][, 3],
                     Side = proportion[[4]][, 3],
                     Exogène = proportion[[2]][, 3]) %>% 
    gather(Exogène, Side, Endogène, key = prov, value = prop, factor_key = TRUE)


to_plot <- bind_rows(ploter, plotps, ploteh)
to_plot %>% ggplot +
          aes(x = Date) +
          geom_area(aes(y = prop, fill = prov), alpha = 0.5) +
          geom_line(aes(y = Observation, color = "Observation"), lwd = 0.75) +
          geom_line(aes(y = Estimation, color = "Estimation"), lwd = 0.75) +
          geom_point(aes(y = Observation, color = "Observation")) +
          geom_point(aes(y = Estimation, color = "Estimation")) +
          theme_bw() +
          facet_grid(. ~ Sol) +
          scale_color_manual(values = c("green4", "black")) +
          theme(legend.title = element_blank(), legend.position = "bottom") +
          ylab("Nombre de larves s'éjectant")




# Plot ER vs EH col = max endo --------------------------------------------

new_front %>%
    as.data.frame() %>% 
    ggplot +
    aes(x = ER, y = EH, color  = Critère) +
    geom_point() +
    theme_bw() +
    theme(aspect.ratio = 1) +
    xlim(c(25, 85)) + 
    ylim(c(25, 85))



# Calib x 5 ---------------------------------------------------------------

res1 <- nsga2(obj, 5, 3,
              lower.bounds = c(0, 0, 0, 0, 1),
              upper.bounds = c(1, 1, 1, 1, 70), 
              popsize = 300, generations = 200)

res2 <- nsga2(obj, 5, 3,
              lower.bounds = c(0, 0, 0, 0, 1),
              upper.bounds = c(1, 1, 1, 1, 70), 
              popsize = 300, generations = 200)

res3 <- nsga2(obj, 5, 3,
              lower.bounds = c(0, 0, 0, 0, 1),
              upper.bounds = c(1, 1, 1, 1, 70), 
              popsize = 300, generations = 200)

res4 <- nsga2(obj, 5, 3,
              lower.bounds = c(0, 0, 0, 0, 1),
              upper.bounds = c(1, 1, 1, 1, 70), 
              popsize = 300, generations = 200)

res5 <- nsga2(obj, 5, 3,
              lower.bounds = c(0, 0, 0, 0, 1),
              upper.bounds = c(1, 1, 1, 1, 70), 
              popsize = 300, generations = 200)

res6 <- nsga2(obj, 5, 3,
              lower.bounds = c(0, 0, 0, 0, 1),
              upper.bounds = c(1, 1, 1, 1, 70), 
              popsize = 300, generations = 200)

res7 <- nsga2(obj, 5, 3,
              lower.bounds = c(0, 0, 0, 0, 1),
              upper.bounds = c(1, 1, 1, 1, 70), 
              popsize = 300, generations = 200)

res8 <- nsga2(obj, 5, 3,
              lower.bounds = c(0, 0, 0, 0, 1),
              upper.bounds = c(1, 1, 1, 1, 70), 
              popsize = 300, generations = 200)

res9 <- nsga2(obj, 5, 3,
              lower.bounds = c(0, 0, 0, 0, 1),
              upper.bounds = c(1, 1, 1, 1, 70), 
              popsize = 300, generations = 200)

res10 <- nsga2(obj, 5, 3,
              lower.bounds = c(0, 0, 0, 0, 1),
              upper.bounds = c(1, 1, 1, 1, 70), 
              popsize = 300, generations = 200)

res11 <- nsga2(obj, 5, 3,
              lower.bounds = c(0, 0, 0, 0, 1),
              upper.bounds = c(1, 1, 1, 1, 70), 
              popsize = 300, generations = 200)

res12 <- nsga2(obj, 5, 3,
              lower.bounds = c(0, 0, 0, 0, 1),
              upper.bounds = c(1, 1, 1, 1, 70), 
              popsize = 300, generations = 200)

res13 <- nsga2(obj, 5, 3,
              lower.bounds = c(0, 0, 0, 0, 1),
              upper.bounds = c(1, 1, 1, 1, 70), 
              popsize = 300, generations = 200)

res14 <- nsga2(obj, 5, 3,
              lower.bounds = c(0, 0, 0, 0, 1),
              upper.bounds = c(1, 1, 1, 1, 70), 
              popsize = 300, generations = 200)

res15 <- nsga2(obj, 5, 3,
              lower.bounds = c(0, 0, 0, 0, 1),
              upper.bounds = c(1, 1, 1, 1, 70), 
              popsize = 300, generations = 200)

save(res1, res2, res3, res4, res5, res6, res7,
     res8, res9, res10, res11, res12, res13, res14,
     res15, file = "to_PLS2.Rdata")