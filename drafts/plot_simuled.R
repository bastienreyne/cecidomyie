# Packages / data ---------------------------------------------------------

library(tidyverse)
library(magrittr)
library(lubridate)
library(mco)
library(gridExtra)
source(file = "../model_R/model1.R")
# source("model1.R")
inflos_corrected <- (read.csv(file = "../data/corrected.csv") %>% as.matrix)[, 2:4]
inflos_simuled <- (read.csv(file = "../data/simu_python.csv") %>% as.matrix)[, 2:4]

date2017 <- read.csv("b1.csv")$date %>% as_date
true_date_2017 <- read_csv2(file = "Data/2017_B1_bache.csv")$date
laps2017 <- c(7, 7, 7, 8, 2, 5, 2, 4, 3, 5, 2, 5, 2, 5, 2, 4, 3, 4, 3)
true_index2017 <- which(date2017 %in% true_date_2017)

bloc1 <- read_csv("../data/2017_piege.csv")
larves <- cbind(bloc1 %>% filter(Sol == "ER") %>% pull(larves),
                bloc1 %>% filter(Sol == "PS") %>% pull(larves),
                bloc1 %>% filter(Sol == "EH") %>% pull(larves))

inflos <- cbind(bloc1 %>% filter(Sol == "ER") %>% pull(inflos),
                bloc1 %>% filter(Sol == "PS") %>% pull(inflos),
                bloc1 %>% filter(Sol == "EH") %>% pull(inflos))


simuled_ER <- cbind(Date = date2017, 
                    IT1 = inflos[, 1],
                    ITS = inflos_simuled[, 1]) %>% 
    as_tibble %>% 
    mutate_at("Date", as_date) %>% 
    gather(ITS, IT1, key = Inflos, value = Nombre) %>% 
    ggplot(aes(x = Date, y = Nombre, color = Inflos)) +
    geom_line(lwd = 0.75) +
    scale_color_discrete(labels = c(expression(I[t]^{1}), 
                                    expression(I[t]^{S}))) +
    theme(legend.title = element_blank(), legend.text = element_text(size = 14),
          axis.title = element_text(size = 14), axis.text = element_text(size = 13)) +
    labs(title = "Enherbement ras") +
    ylab("Nombre d'inflos") +
    xlab("Date") +
    ylim(c(0, 6001))

simuled_PS <- cbind(Date = date2017, 
                    IT1 = inflos[, 2],
                    ITS = inflos_simuled[, 2]) %>% 
    as_tibble %>% 
    mutate_at("Date", as_date) %>% 
    gather(ITS, IT1, key = Inflos, value = Nombre) %>% 
    ggplot(aes(x = Date, y = Nombre, color = Inflos)) +
    geom_line(lwd = 0.75) +
    scale_color_discrete(labels = c(expression(I[t]^{1}), 
                                    expression(I[t]^{S}))) +
    theme(legend.title = element_blank(), legend.text = element_text(size = 14),
          axis.title = element_text(size = 14), axis.text = element_text(size = 13)) +
    labs(title = "Paillage synthétique") +
    ylab("Nombre d'inflos") +
    xlab("Date") +
    ylim(c(0, 6001))

simuled_EH <- cbind(Date = date2017, 
                    IT1 = inflos[, 3],
                    ITS = inflos_simuled[, 3]) %>% 
    as_tibble %>% 
    mutate_at("Date", as_date) %>% 
    gather(ITS, IT1, key = Inflos, value = Nombre) %>% 
    ggplot(aes(x = Date, y = Nombre, color = Inflos)) +
    geom_line(lwd = 0.75) +
    scale_color_discrete(labels = c(expression(I[t]^{1}), 
                                    expression(I[t]^{S}))) +
    theme(legend.title = element_blank(), legend.text = element_text(size = 14),
          axis.title = element_text(size = 14), axis.text = element_text(size = 13)) +
    labs(title = "Enherbement haut") +
    ylab("Nombre d'inflos") +
    xlab("Date") +
    ylim(c(0, 6001))
