data.frame(date = rep(date2017, 6),
           inflos = c(inflos, inflos_simu),
           sol = factor(rep(c("Enherbement ras", "Paillage synthétique", "Enherbement haut"), 
                            each = 80, times = 2),
                        levels = c("Enherbement ras", "Paillage synthétique", "Enherbement haut")),
           statut = rep(c("Observées", "Simulées"), each = 240)) %>% 
    ggplot() +
    aes(x = date, y = inflos, color = statut) +
    geom_line(lwd = 0.75) +
    theme_bw() +
    facet_grid(.~sol) +
    ylab("Nombre d'inflorescences vivantes") +
    xlab("") +
    theme(legend.title = element_blank())
