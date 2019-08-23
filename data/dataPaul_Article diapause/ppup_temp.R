## Script qui pour ajuster p_pup ~ temp

# packages /data ----------------------------------------------------------

library(tidyverse)
library(lubridate)

larves <- read.table("data2.txt", header = T, sep = "\t")
temp <- read.table("data2_meteo.txt", header = T, sep = "\t")


# mise en forme -----------------------------------------------------------

nb_larves <- aggregate(larves$nblarv, list(larves$datprelev), sum)
df_tmp <- data.frame(date = larves$datprelev, pup = larves$toteme) %>% na.omit
nb_pup <- aggregate(df_tmp$pup, list(df_tmp$date), sum)

## Gardons uniquement les dates où l'on connaît le nombre de pupes
good_dates <- nb_larves$Group.1 %in% nb_pup$Group.1

pct_pup <- nb_pup$x / nb_larves[good_dates, ]$x

to_plot <- data.frame(date = nb_pup$Group.1 %>% dmy,
                      pct = pct_pup)
to_plot %>% ggplot +
    aes(x = date, y = pct) +
    geom_point() +
    theme_bw()

moving_mean <- function(x) {
    n_x <- length(x)
    ans <- rep(NA, n_x)
    ans[2:(n_x - 1)] <- x[1:(n_x-2)] + x[2:(n_x - 1)] + x[3:n_x]
    ans[1] <- 2 * x[1] + x[2]
    ans[n_x] <- x[(n_x - 1)] + 2 * x[n_x]
    
    ans / 3
}

temperature <- moving_mean(temp$tempm)
good_ind <- which(temp$date %in% nb_pup$Group.1)
good_temp <- temperature[good_ind]
good_temp[which(is.na(good_temp))] <- temp$tempm[good_ind][which(is.na(good_temp))]

good_ordre <- nb_pup$Group.1 %>% dmy %>% order()
good_pct <- pct_pup[good_ordre]

# trouver relation p_pup ~ temp -------------------------------------------

fit <- lm(good_pct ~ good_temp)

# plot --------------------------------------------------------------------

to_plot2 <- data.frame(pup = good_pct, temp = good_temp)
to_plot2 %>% ggplot +
    aes(x = temp, y = pup) +
    geom_point() +
    geom_abline(intercept = fit$coefficients[1], slope = fit$coefficients[2]) +
    theme_bw() +
    ylab("Taux de pupaison") +
    xlab("Température")


# ajustement sur 15j ------------------------------------------------------

temp15j <- rep(NA, length(good_ind))
for (i in 1:length(good_ind)) {
    temp15j[i] <- mean(temperature[(good_ind[i] - 7):(good_ind[i] + 7)], na.rm = TRUE)
}

fit2 <- lm(good_pct ~ temp15j)


# Comp des 3 --------------------------------------------------------------

args <- c(0.02, 0.8, 1, 1, 54.119)
cst <- dynamics2(args, inflos_attractive %>% as.matrix)
p1j <- dynamics21j(args, inflos_attractive %>% as.matrix)
p15j <- dynamics215j(args, inflos_attractive %>% as.matrix)


er <- data.frame(Date = date2017, obs = larves[, 1],
                 est1 = cst[, 1], est2 = p1j[, 1], est3 = p15j[, 1],
                 Sol = factor("Enherbement ras", levels = c("Enherbement ras", 
                                                            "Paillage synthétique", 
                                                            "Enherbement haut"))) %>% 
    gather(est1, est2, est3, key = est, value = nombre)

ps <- data.frame(Date = date2017, obs = larves[, 2],
                 est1 = cst[, 2], est2 = p1j[, 2], est3 = p15j[, 2],
                 Sol = factor("Paillage synthétique", levels = c("Enherbement ras", 
                                                            "Paillage synthétique", 
                                                            "Enherbement haut"))) %>% 
    gather(est1, est2, est3, key = est, value = nombre)

eh <- data.frame(Date = date2017, obs = larves[, 3],
                 est1 = cst[, 3], est2 = p1j[, 3], est3 = p15j[, 3],
                 Sol = factor("Enherbement haut", levels = c("Enherbement ras", 
                                                            "Paillage synthétique", 
                                                            "Enherbement haut"))) %>% 
    gather(est1, est2, est3, key = est, value = nombre)

plot_pup <- bind_rows(er, ps, eh)
plot_pup %>% ggplot +
    aes(x = Date) +
    geom_line(aes(y = obs)) +
    geom_point(aes(y = obs)) +
    geom_line(aes(y = nombre, color = est)) +
    geom_point(aes(y = nombre, color = est)) +
    facet_grid(Sol ~ .) +
    theme_bw() +
    ylab("Nombre de larves s'éjectant") +
    theme(legend.title = element_blank(), legend.position = "bottom") +
    scale_color_discrete(labels = c("0.77\t", "Température quotidienne\t", "Température sur la quinzaine"))
