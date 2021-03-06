## Script avec des fonctions qui plottent


# packages / data ---------------------------------------------------------

library(tidyverse)
data_piege <- read.csv("2017_piege.csv")
larves_obs <- cbind(data_piege %>% filter(Sol == "ER") %>% pull(larves),
                    data_piege %>% filter(Sol == "PS") %>% pull(larves),
                    data_piege %>% filter(Sol == "EH") %>% pull(larves))

# Plots function ----------------------------------------------------------

plot_dynamics <- function(args, inflos) {
    obs <- larves_obs
    est <- dynamics(args, inflos)
    
    er <- data.frame(date = date2017, obs = obs[, 1], est = est[, 1]) %>%
        mutate(Sol = factor("Enherbement ras",
                            levels = c("Enherbement ras", "Paillage synthétique", "Enherbement haut"))) %>% 
        gather(obs, est, key = statut, value = nombre, factor_key = TRUE)
    
    ps <- data.frame(date = date2017, obs = obs[, 2], est = est[, 2]) %>%
        mutate(Sol = factor("Paillage synthétique", 
                            levels = c("Enherbement ras", "Paillage synthétique", "Enherbement haut"))) %>% 
        gather(obs, est, key = statut, value = nombre, factor_key = TRUE)
    
    eh <- data.frame(date = date2017, obs = obs[, 3], est = est[, 3]) %>%
        mutate(Sol = factor("Enherbement haut",
                            levels = c("Enherbement ras", "Paillage synthétique", "Enherbement haut"))) %>% 
        gather(obs, est, key = statut, value = nombre, factor_key = TRUE)
    
    to_plot <- bind_rows(er, ps, eh)
    to_plot %>% ggplot +
        aes(x = date, y = nombre, color = statut) +
        geom_point() +
        geom_line() +
        facet_grid(. ~ Sol) +
        theme_bw() +
        theme(legend.title = element_blank()) +
        scale_color_discrete(labels = c("Observation", "Estimation")) +
        xlab("Date") +
        ylab("Nombre de larves")
}

plot_decompo <- function(args, inflos) {
    obs <- larves_obs
    estimations <- decomposition(args, inflos)
    
    ploter <- data.frame(Date = date2017,
                         Sol = factor("Enherbement ras", levels = c("Enherbement ras",
                                                                    "Paillage synthétique",
                                                                    "Enherbement haut")),
                         Observation = obs[, 1],
                         Estimation = estimations[[1]][, 1],
                         Pupaison = estimations[[2]][, 1],
                         Diapause = estimations[[3]][, 1],
                         Voisinage = estimations[[4]][, 1],
                         Exogène = estimations[[5]][, 1]) %>% 
        gather(Exogène, Voisinage, Diapause, Pupaison,
               key = prov, value = prop, factor_key = TRUE)
    
    plotps <- data.frame(Date = date2017,
                         Sol = factor("Paillage synthétique", levels = c("Enherbement ras",
                                                                         "Paillage synthétique",
                                                                         "Enherbement haut")),
                         Observation = obs[, 2],
                         Estimation = estimations[[1]][, 2],
                         Pupaison = estimations[[2]][, 2],
                         Diapause = estimations[[3]][, 2],
                         Voisinage = estimations[[4]][, 2],
                         Exogène = estimations[[5]][, 2]) %>% 
        gather(Exogène, Voisinage, Diapause, Pupaison,
               key = prov, value = prop, factor_key = TRUE)
    
    ploteh <- data.frame(Date = date2017,
                         Sol = factor("Enherbement haut", levels = c("Enherbement ras",
                                                                     "Paillage synthétique",
                                                                     "Enherbement haut")),
                         Observation = obs[, 3],
                         Estimation = estimations[[1]][, 3],
                         Pupaison = estimations[[2]][, 3],
                         Diapause = estimations[[3]][, 3],
                         Voisinage = estimations[[4]][, 3],
                         Exogène = estimations[[5]][, 3]) %>% 
        gather(Exogène, Voisinage, Diapause, Pupaison,
               key = prov, value = prop, factor_key = TRUE)
    
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
        theme(legend.title = element_blank(), legend.position = "bottom") +
        scale_color_manual(values = c("green4", "black")) +
        ylab("Nombre de larves")
    
}

