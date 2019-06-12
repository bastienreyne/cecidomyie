

It1c <- read.csv("../data/2017_corrected.csv")[3:5] %>% as.matrix()
piege <- read.csv("../data/2017_piege.csv")
It2 <- cbind(piege %>% filter(Sol == "ER") %>% pull(inflos),
             piege %>% filter(Sol == "PS") %>% pull(inflos),
             piege %>% filter(Sol == "EH") %>% pull(inflos))
Its <- read.csv("../data/inflos_simulated.csv")[3:5] %>% as.matrix()

toto <- rbind(It1c, It2, Its) %>% 
    as_tibble() %>% 
    mutate(Date = rep(date2017, 3)) %>% 
    mutate(sourc = factor(rep(c("It1c", "It2", "Its"), each = 80),
                          levels = c("It1c", "It2", "Its"))) %>% 
    gather(inflos_ER, inflos_PS, inflos_EH, key = Sol, 
           value = Nombre, factor_key = TRUE) %>% 
    ggplot +
    aes(x = Date, y = Nombre, color = sourc) +
    geom_line(lwd = 0.75) +
    theme_bw() +
    facet_grid(. ~ Sol, 
               labeller = labeller(Sol = c(inflos_ER = "Enherbement ras",
                                           inflos_PS = "Paillage synthétique",
                                           inflos_EH = "Enherbement haut"))) +
    ylab("Nombre d'inflorescences vivantes") +
    theme(legend.title = element_blank(), 
          legend.text = element_text(size = 13)) +
    scale_color_discrete(labels = c(expression(I[t]^{"1,c"}),
                                    expression(I[t]^{"2"}),
                                    expression(I[t]^{"s"})))

