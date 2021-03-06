## Script qui plotte les inflos

# Data / packages ---------------------------------------------------------

library(tidyverse)
library(magrittr)
library(lubridate)
library(viridisLite)
load("date2017.Rdata")
load("It1.Rdata")
load("Itc1.Rdata")
load("It2.Rdata")
load("Ita1.Rdata")
its <- read.csv("inflos_simulated.csv")[3:5] %>% as.matrix
itas <- read.csv("attractive_simulated.csv")[3:5] %>% as.matrix
load("Btc1.Rdata")
load("Bts.Rdata")
load("Itsdmin.Rdata")

# Plot ds1 ----------------------------------------------------------------

It_ER <- cbind(date2017, DS2 = It2_ER, DS1 = It1_ER, DS1C = Itc1_ER) %>%
    as_tibble %>% 
    mutate(Sol = factor("Enherbement ras",
                        levels = c("Enherbement ras", 
                                   "Paillage synthétique",
                                   "Enherbement haut"))) %>% 
    mutate_at("date2017", as_date) %>% 
    gather(DS2, DS1, DS1C, key = ds, value = Nombre, factor_key = TRUE)

It_PS <- cbind(date2017, DS2 = It2_PS, DS1 = It1_PS, DS1C = Itc1_PS) %>% 
    as_tibble %>% 
    mutate(Sol = factor("Paillage synthétique",
                        levels = c("Enherbement ras", 
                                   "Paillage synthétique",
                                   "Enherbement haut"))) %>% 
    mutate_at("date2017", as_date) %>% 
    gather(DS2, DS1, DS1C, key = ds, value = Nombre, factor_key = TRUE)

It_EH <- cbind(date2017, DS2 = It2_EH, DS1 = It1_EH, DS1C = Itc1_EH) %>% 
    as_tibble %>% 
    mutate(Sol = factor("Enherbement haut",
                        levels = c("Enherbement ras", 
                                   "Paillage synthétique",
                                   "Enherbement haut"))) %>% 
    mutate_at("date2017", as_date) %>% 
    gather(DS2, DS1, DS1C, key = ds, value = Nombre, factor_key = TRUE)


to_plot1 <- bind_rows(It_ER, It_PS, It_EH)

plot1 <- to_plot1 %>% ggplot(aes(x = date2017, y = Nombre, color = ds)) +
    geom_line(lwd = 0.75) +
    facet_grid(. ~ Sol) +
    xlab("Date") +
    ylab("Nombre d'inflorescences") +
    theme(legend.title = element_blank(), legend.text = element_text(size = 14)) +
    scale_color_manual(values = c("green4", "yellow", "orange"),
                       labels = c(expression(I[t]^{2}),
                                     expression(I[t]^{1}),
                                     expression(I[t]^{"c,1"})))


# plot ds1 comp a/c -------------------------------------------------------

It_ER2 <- cbind(date2017, DS1C = Itc1_ER, DS1A = Ita1_ER) %>%
    as_tibble %>% 
    mutate(Sol = factor("Enherbement ras",
                        levels = c("Enherbement ras", 
                                   "Paillage synthétique",
                                   "Enherbement haut"))) %>% 
    mutate_at("date2017", as_date) %>% 
    gather(DS1C, DS1A, key = ds, value = Nombre, factor_key = TRUE)

It_PS2 <- cbind(date2017, DS1C = Itc1_PS, DS1A = Ita1_PS) %>% 
    as_tibble %>% 
    mutate(Sol = factor("Paillage synthétique",
                        levels = c("Enherbement ras", 
                                   "Paillage synthétique",
                                   "Enherbement haut"))) %>% 
    mutate_at("date2017", as_date) %>% 
    gather(DS1C, DS1A, key = ds, value = Nombre, factor_key = TRUE)

It_EH2 <- cbind(date2017, DS1C = Itc1_EH, DS1A = Ita1_EH) %>% 
    as_tibble %>% 
    mutate(Sol = factor("Enherbement haut",
                        levels = c("Enherbement ras", 
                                   "Paillage synthétique",
                                   "Enherbement haut"))) %>% 
    mutate_at("date2017", as_date) %>% 
    gather(DS1C, DS1A, key = ds, value = Nombre, factor_key = TRUE)


to_plot2 <- bind_rows(It_ER2, It_PS2, It_EH2)

plot2 <- to_plot2 %>% ggplot(aes(x = date2017, y = Nombre, color = ds)) +
    geom_line(lwd = 0.75) +
    facet_grid(. ~ Sol) +
    xlab("Date") +
    ylab("Nombre d'inflorescences") +
    theme(legend.title = element_blank(), legend.text = element_text(size = 14)) +
    scale_color_manual(values = c("orange", "red"),
                       labels = c(expression(I[t]^{"c,1"}),
                                  expression(I[t]^{"a,1"})))

# Comparaison inflos simulation -------------------------------------------

It_ER3 <- cbind(date2017, DS2 = It2_ER, ITS = its[, 1], ITAS = itas[, 1]) %>%
    as_tibble %>% 
    mutate(Sol = factor("Enherbement ras",
                        levels = c("Enherbement ras", 
                                   "Paillage synthétique",
                                   "Enherbement haut"))) %>% 
    mutate_at("date2017", as_date) %>% 
    gather(DS2, ITS, ITAS, key = ds, value = Nombre, factor_key = TRUE)

It_PS3 <- cbind(date2017, DS2 = It2_PS, ITS = its[, 2], ITAS = itas[, 2]) %>% 
    as_tibble %>% 
    mutate(Sol = factor("Paillage synthétique",
                        levels = c("Enherbement ras", 
                                   "Paillage synthétique",
                                   "Enherbement haut"))) %>% 
    mutate_at("date2017", as_date) %>% 
    gather(DS2, ITS, ITAS, key = ds, value = Nombre, factor_key = TRUE)

It_EH3 <- cbind(date2017, DS2 = It2_EH, ITS = its[, 3], ITAS = itas[, 3]) %>% 
    as_tibble %>% 
    mutate(Sol = factor("Enherbement haut",
                        levels = c("Enherbement ras", 
                                   "Paillage synthétique",
                                   "Enherbement haut"))) %>% 
    mutate_at("date2017", as_date) %>% 
    gather(DS2, ITS, ITAS, key = ds, value = Nombre, factor_key = TRUE)


to_plot3 <- bind_rows(It_ER3, It_PS3, It_EH3)

plot3 <- to_plot3 %>% ggplot(aes(x = date2017, y = Nombre, color = ds)) +
    geom_line(lwd = 0.75) +
    facet_grid(. ~ Sol) +
    xlab("Date") +
    ylab("Nombre d'inflorescences") +
    theme(legend.title = element_blank(), legend.text = element_text(size = 14)) +
    scale_color_manual(values = c("green4", "skyblue", "blue"),
                       labels = c(expression(I[t]^{2}),
                                  expression(I[t]^{s}),
                                  expression(I[t]^{"a,s"})))


# Comparaison des 2 approches ---------------------------------------------


It_ER4 <- cbind(date2017, DS2 = It2_ER, ITS = its[, 1], DS1C = Itc1_ER) %>%
    as_tibble %>% 
    mutate(Sol = factor("Enherbement ras",
                        levels = c("Enherbement ras", 
                                   "Paillage synthétique",
                                   "Enherbement haut"))) %>% 
    mutate_at("date2017", as_date) %>% 
    gather(DS2, ITS, DS1C, key = ds, value = Nombre, factor_key = TRUE)

It_PS4 <- cbind(date2017, DS2 = It2_PS, ITS = its[, 2], DS1C = Itc1_PS) %>% 
    as_tibble %>% 
    mutate(Sol = factor("Paillage synthétique",
                        levels = c("Enherbement ras", 
                                   "Paillage synthétique",
                                   "Enherbement haut"))) %>% 
    mutate_at("date2017", as_date) %>% 
    gather(DS2, ITS, DS1C, key = ds, value = Nombre, factor_key = TRUE)

It_EH4 <- cbind(date2017, DS2 = It2_EH, ITS = its[, 3], DS1C = Itc1_EH) %>% 
    as_tibble %>% 
    mutate(Sol = factor("Enherbement haut",
                        levels = c("Enherbement ras", 
                                   "Paillage synthétique",
                                   "Enherbement haut"))) %>% 
    mutate_at("date2017", as_date) %>% 
    gather(DS2, ITS, DS1C, key = ds, value = Nombre, factor_key = TRUE)


to_plot4 <- bind_rows(It_ER4, It_PS4, It_EH4)

plot4 <- to_plot4 %>% ggplot(aes(x = date2017, y = Nombre, color = ds)) +
    geom_line(lwd = 0.75) +
    facet_grid(. ~ Sol) +
    xlab("Date") +
    ylab("Nombre d'inflorescences") +
    theme(legend.title = element_blank(), legend.text = element_text(size = 14)) +
    scale_color_manual(values = c("green4", "skyblue", "orange"),
                       labels = c(expression(I[t]^{2}),
                                  expression(I[t]^{s}),
                                  expression(I[t]^{"c,1"})))


# Comp bursts cumsum ------------------------------------------------------

It_ER5 <- cbind(date2017, DS1C = cumsum(Btc1_ER), Simu = cumsum(Bts_ER)) %>%
    as_tibble %>% 
    mutate(Sol = factor("Enherbement ras",
                        levels = c("Enherbement ras", 
                                   "Paillage synthétique",
                                   "Enherbement haut"))) %>% 
    mutate_at("date2017", as_date) %>% 
    gather(DS1C, Simu, key = ds, value = Nombre, factor_key = TRUE)

It_PS5 <- cbind(date2017, DS1C = cumsum(Btc1_PS), Simu = cumsum(Bts_PS)) %>% 
    as_tibble %>% 
    mutate(Sol = factor("Paillage synthétique",
                        levels = c("Enherbement ras", 
                                   "Paillage synthétique",
                                   "Enherbement haut"))) %>% 
    mutate_at("date2017", as_date) %>% 
    gather(DS1C, Simu, key = ds, value = Nombre, factor_key = TRUE)

It_EH5 <- cbind(date2017, DS1C = cumsum(Btc1_EH), Simu = cumsum(Bts_EH)) %>% 
    as_tibble %>% 
    mutate(Sol = factor("Enherbement haut",
                        levels = c("Enherbement ras", 
                                   "Paillage synthétique",
                                   "Enherbement haut"))) %>% 
    mutate_at("date2017", as_date) %>% 
    gather(DS1C, Simu, key = ds, value = Nombre, factor_key = TRUE)


to_plot5 <- bind_rows(It_ER5, It_PS5, It_EH5)

plot5 <- to_plot5 %>% ggplot(aes(x = date2017, y = Nombre, color = ds)) +
    geom_line(lwd = 0.75) +
    facet_grid(. ~ Sol) +
    xlab("Date") +
    ylab("Somme cumulée des débourrements") +
    theme(legend.title = element_blank(), legend.text = element_text(size = 14)) +
    scale_color_manual(values = c("orange", "skyblue"),
                       labels = c(expression(B[t]^{"c,1"}),
                                  expression(B[t]^{s})))
