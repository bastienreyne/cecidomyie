
library(tidyverse)
load("date2017.Rdata")
inflos1 <- read_csv("2017_inflos_dataset1_bloc2.csv") %>% filter(date >= "2017-07-19")
inflos2 <- cbind(dataset2 %>% filter(Sol == "ER") %>% pull(inflos),
                 dataset2 %>% filter(Sol == "PS") %>% pull(inflos),
                 dataset2 %>% filter(Sol == "EH") %>% pull(inflos))
inflos1c <- read_csv("2017_inflos_corrected_bloc2.csv")


df <- data.frame(Date = rep(date2017[1:77], 9), 
                 Inflorescences = c(inflos1$inflosER * my_alpha_ER,
                                    inflos1$inflosPS * my_alpha_PS,
                                    inflos1$inflosEH * my_alpha_EH,
                                    inflos1c$inflos_ER,
                                    inflos1c$inflos_PS,
                                    inflos1c$inflos_EH,
                                    inflos2[1:77, 1],
                                    inflos2[1:77, 2],
                                    inflos2[1:77, 3]),
                 Sol = factor(rep(c("Enherbement ras",
                                    "Paillage synthétique",
                                    "Enherbement haut"),
                                  each = 77, times = 3),
                              levels = c("Enherbement ras",
                                         "Enherbement haut",
                                         "Paillage synthétique")),
                 Dataset = factor(rep(c("It1", "Itc1", "It2"), each = 231),
                                  levels = c("It1", "Itc1", "It2")))

df %>% ggplot +
    aes(x = Date, y = Inflorescences, color = Dataset) +
    geom_line(lwd = 0.75) +
    theme_bw() +
    facet_grid(. ~ Sol) +
    ylab("Nombre d'inflorescences vivantes") +
    theme(legend.title = element_blank(), legend.position = "bottom", 
          legend.text = element_text(size = 10)) +
    scale_color_discrete(labels = c("Dataset 1   ", "Dataset 1 corrigé   ", "Dataset 2")) +
    xlab(element_blank())
