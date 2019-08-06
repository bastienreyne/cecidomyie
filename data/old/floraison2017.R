## Script pour prendre les inflorescences du fichier floraison et les corriger pour les faire
## coincider avec les inflos du fichier piege.

# Packages / data ---------------------------------------------------------

library(tidyverse)
library(magrittr)
library(lubridate)

inflos_piege <- read_csv("2017_piege.csv") %>% select(date, inflos, Sol)
inflos_floraison <- read_csv2("raw/floraison0.csv")

inflos_floraison %<>% mutate_at(c("ApdateC", "ApdateM",
                                  c(paste0("Lat",1:5,"dateC"),
                                    paste0("Lat", 1:5, "dateM"))), dmy) %>% 
    filter(Annee == 2017) %>% 
    filter(Bloc == 1)

inflos_floraison$Traitm[which(inflos_floraison$Traitm == "Sn")] <- "ER"
inflos_floraison$Traitm[which(inflos_floraison$Traitm == "B")] <- "PS"
inflos_floraison$Traitm[which(inflos_floraison$Traitm == "E")] <- "EH"

# Mise en forme inflo floraison -------------------------------------------

inflos_apic <- inflos_floraison %>% filter(!is.na(ApdateC)) %>%
    select(Traitm, ApdateC, ApdateM) %>% 
    rename(birth = ApdateC, death = ApdateM)

inflos_lat1 <- inflos_floraison %>% filter(!is.na(Lat1dateC)) %>%
    select(Traitm, Lat1dateC, Lat1dateM) %>%
    rename(birth = Lat1dateC, death = Lat1dateM)

inflos_lat2 <- inflos_floraison %>% filter(!is.na(Lat2dateC)) %>%
    select(Traitm, Lat2dateC, Lat2dateM) %>%
    rename(birth = Lat2dateC, death = Lat2dateM)

inflos_lat3 <- inflos_floraison %>% filter(!is.na(Lat3dateC)) %>%
    select(Traitm, Lat3dateC, Lat3dateM) %>%
    rename(birth = Lat3dateC, death = Lat3dateM)

inflos_lat4 <- inflos_floraison %>% filter(!is.na(Lat4dateC)) %>%
    select(Traitm, Lat4dateC, Lat4dateM) %>%
    rename(birth = Lat4dateC, death = Lat4dateM)

inflos_lat5 <- inflos_floraison %>% filter(!is.na(Lat5dateC)) %>%
    select(Traitm, Lat5dateC, Lat5dateM) %>%
    rename(birth = Lat5dateC, death = Lat5dateM)

floraison2017 <- rbind(inflos_apic, inflos_lat1, inflos_lat2,
                       inflos_lat3, inflos_lat4, inflos_lat5) %>% 
    arrange(birth)
floraison2017$birth <- floraison2017$birth - 365
floraison2017$death <- floraison2017$death - 365
isna <- floraison2017$death %>% is.na %>% which
floraison2017$death[isna] <- floraison2017$birth[isna] + 50
# write.csv(floraison2017, file = "2017_floraison_dates.csv")

floraison2017_ER <- floraison2017 %>% filter(Traitm == "ER")
floraison2017_PS <- floraison2017 %>% filter(Traitm == "PS")
floraison2017_EH <- floraison2017 %>% filter(Traitm == "EH")

date <- floraison2017 %$% unique(c(birth, death)) %>% na.omit
date <- min(date):max(date) %>% as_date


inflos_floraison_ER <- rep(NA, length(date))
inflos_floraison_PS <- rep(NA, length(date))
inflos_floraison_EH <- rep(NA, length(date))
inflos_floraison_all <- rep(NA, length(date))
inflos_deads_ER <- rep(NA, length(date))
inflos_deads_PS <- rep(NA, length(date))
inflos_deads_EH <- rep(NA, length(date))
inflos_deads_all <- rep(NA, length(date))
for (day in 1:length(date)) {
    inflos_floraison_ER[day] <- floraison2017_ER %$% which(birth <= date[day] & death > date[day]) %>%
        length + floraison2017_ER %$% which(birth <= date[day] & is.na(death)) %>% length
    inflos_floraison_PS[day] <- floraison2017_PS %$% which(birth <= date[day] & death > date[day]) %>%
        length + floraison2017_PS %$% which(birth <= date[day] & is.na(death)) %>% length
    inflos_floraison_EH[day] <- floraison2017_EH %$% which(birth <= date[day] & death > date[day]) %>%
        length + floraison2017_EH %$% which(birth <= date[day] & is.na(death)) %>% length
    inflos_floraison_all[day] <- floraison2017 %$% which(birth <= date[day] & death > date[day]) %>%
        length + floraison2017 %$% which(birth <= date[day] & is.na(death)) %>% length
    inflos_deads_ER[day] <- floraison2017_ER %$% which(death == date[day]) %>% length
    inflos_deads_PS[day] <- floraison2017_PS %$% which(death == date[day]) %>% length
    inflos_deads_EH[day] <- floraison2017_EH %$% which(death == date[day]) %>% length
    inflos_deads_all[day] <- floraison2017 %$% which(death == date[day]) %>% length
}

floraison_ER <- cbind(date, inflos = inflos_floraison_ER, deads = inflos_deads_ER) %>% 
    as_tibble %>% 
    mutate_at("date", as_date) %>%
    filter(date >= "2017-07-19") %>%
    mutate(Sol = "ER")

floraison_PS <- cbind(date, inflos = inflos_floraison_PS, deads = inflos_deads_PS) %>%
    as_tibble %>% 
    mutate_at("date", as_date) %>%
    filter(date >= "2017-07-19") %>% 
    mutate(Sol = "PS")

floraison_EH <- cbind(date, inflos = inflos_floraison_EH, deads = inflos_deads_EH) %>%
    as_tibble %>% 
    mutate_at("date", as_date) %>%
    filter(date >= "2017-07-19") %>%
    mutate(Sol = "EH")

floraison_all <- cbind(date, inflos = inflos_floraison_all, deads = inflos_deads_all) %>%
    as_tibble %>% 
    mutate_at("date", as_date) %>%
    filter(date >= "2017-07-19") %>%
    mutate(Sol = "all")


floraison <- rbind(floraison_ER, floraison_PS, floraison_EH, floraison_all)

# write.csv(floraison, file = "2017_floraison_not_corrected.csv")

# Correction --------------------------------------------------------------

floraison_ER_to_correct <- cbind(date, inflos = inflos_floraison_ER, deads = inflos_deads_ER) %>% 
    as_tibble %>% 
    mutate_at("date", as_date) %>%
    filter(date >= "2017-07-19") %>%
    filter(date <= "2017-10-06") %>% 
    mutate(Sol = "ER")

floraison_PS_to_correct <- cbind(date, inflos = inflos_floraison_PS, deads = inflos_deads_PS) %>%
    as_tibble %>% 
    mutate_at("date", as_date) %>%
    filter(date >= "2017-07-19") %>% 
    filter(date <= "2017-10-06") %>% 
    mutate(Sol = "PS")

floraison_EH_to_correct <- cbind(date, inflos = inflos_floraison_EH, deads = inflos_deads_EH) %>%
    as_tibble %>% 
    mutate_at("date", as_date) %>%
    filter(date >= "2017-07-19") %>%
    filter(date <= "2017-10-06") %>% 
    mutate(Sol = "EH")

index <- floraison_ER %$% which(date == "2017-09-05" | date == "2017-09-06")
ecart_ER <- (floraison_ER[49, 2] - floraison_ER[50, 2]) %>%
    as.numeric()

ecart_PS <- (floraison_PS[49, 2] - floraison_PS[50, 2]) %>%
    as.numeric()

ecart_EH <- (floraison_EH[49, 2] - floraison_EH[50, 2]) %>% 
    as.numeric()

date2017 <- floraison_ER_to_correct$date

inflos_piege_ER <- inflos_piege %>%
    filter(Sol == "ER") %>%
    pull(inflos)

inflos_piege_PS <- inflos_piege %>%
    filter(Sol == "PS") %>%
    pull(inflos)

inflos_piege_EH <- inflos_piege %>%
    filter(Sol == "EH") %>%
    pull(inflos)

inflos_target_ER <- inflos_piege_ER 
inflos_target_PS <- inflos_piege_PS 
inflos_target_EH <- inflos_piege_EH 

inflos_current_ER <- floraison_ER_to_correct$inflos 
inflos_current_PS <- floraison_PS_to_correct$inflos 
inflos_current_EH <- floraison_EH_to_correct$inflos 

## Fonction de coût
my_rmse <- function(x, y) {
    n_obs <- length(x)
    sqrt(sum((x - y)^2) / n_obs ) 
}

## ER
objectiveER <- function(x) {
    
    poids <- x[1:50]
    morts <- poids * ecart_ER / sum(poids)
    
    inflos <- inflos_current_ER
    inflos[1:49] <- inflos[1:49] - cumsum(morts)[1:49]
    my_rmse(x[51] * inflos, inflos_target_ER)
}

library(mco)
resER <- nsga2(objectiveER, 51, 1,
               lower.bounds = c(rep(0, 51)),
               upper.bounds = c(rep(100, 50), 6000),
               popsize = 200, 
               generations = 100)

my_x_ER <- resER$par[1, 1:50]
my_alpha_ER <- resER$par[1, 51]

deads_ER <- my_x_ER * ecart_ER / sum(my_x_ER)

inflos_ER <- inflos_current_ER
inflos_ER[1:49] <- inflos_ER[1:49] - cumsum(deads_ER)[1:49]
inflos_ER <- inflos_ER * my_alpha_ER

## PS
objectivePS <- function(x) {
    
    poids <- x[1:50]
    morts <- poids * ecart_PS / sum(poids)
    
    inflos <- inflos_current_PS
    inflos[1:49] <- inflos[1:49] - cumsum(morts)[1:49]
    my_rmse(x[51] * inflos, inflos_target_PS)
}


resPS <- nsga2(objectivePS, 51, 1,
               lower.bounds = c(rep(0, 51)),
               upper.bounds = c(rep(100, 50), 6000),
               popsize = 200, 
               generations = 100)

my_x_PS <- resPS$par[1, 1:50]
my_alpha_PS <- resPS$par[1, 51]

deads_PS <- my_x_PS * ecart_PS / sum(my_x_PS)

inflos_PS <- inflos_current_PS
inflos_PS[1:49] <- inflos_PS[1:49] - cumsum(deads_PS)[1:49]
inflos_PS <- inflos_PS * my_alpha_PS

## EH
objectiveEH <- function(x) {
    
    poids <- x[1:50]
    morts <- poids * ecart_EH / sum(poids)
    
    inflos <- inflos_current_EH
    inflos[1:49] <- inflos[1:49] - cumsum(morts)[1:49]
    my_rmse(x[51] * inflos, inflos_target_EH)
}


resEH <- nsga2(objectiveEH, 51, 1,
             lower.bounds = c(rep(0, 51)),
             upper.bounds = c(rep(100, 50), 6000),
             popsize = 200, 
             generations = 100)

my_x_EH <- resEH$par[1, 1:50]
my_alpha_EH <- resEH$par[1, 51]

deads_EH <- my_x_EH * ecart_EH / sum(my_x_EH)

inflos_EH <- inflos_current_EH
inflos_EH[1:49] <- inflos_EH[1:49] - cumsum(deads_EH)[1:49]
inflos_EH <- inflos_EH * my_alpha_EH


# Résultats ---------------------------------------------------------------


resultats_ER <- cbind(date = date2017,
                   corrected = inflos_ER,
                   piege = inflos_target_ER,
                   floraison = my_alpha_ER * inflos_current_ER) %>% as_tibble %>%
    mutate_at("date", as_date) %>%
    gather(floraison, corrected, piege, key = Source, value = Nombre, factor_key = TRUE)

resultats_PS <- cbind(date = date2017,
                      corrected = inflos_PS,
                      piege = inflos_target_PS,
                      floraison = my_alpha_PS * inflos_current_PS) %>% as_tibble %>%
    mutate_at("date", as_date) %>%
    gather(floraison, corrected, piege, key = Source, value = Nombre, factor_key = TRUE)

resultats_EH <- cbind(date = date2017,
                      corrected = inflos_EH,
                      piege = inflos_target_EH,
                      floraison = my_alpha_EH * inflos_current_EH) %>% as_tibble %>%
    mutate_at("date", as_date) %>%
    gather(floraison, corrected, piege, key = Source, value = Nombre, factor_key = TRUE)

plot_ER <- resultats_ER %>% ggplot(aes(x = date, y = Nombre, color = Source)) +
    geom_line(lwd = 0.75) +
    scale_color_discrete(labels = c(expression(alpha * I[t]^{1}),
                                    bquote(I[t]^{c}),
                                    expression(I[t]^{2}))) +
    theme(legend.title = element_blank(), legend.text = element_text(size = 14),
          axis.title = element_text(size = 11), axis.text = element_text(size = 10)) +
    labs(title = "Enherbement ras") +
    ylab("Nombre d'inflos") +
    xlab("Date") +
    ylim(c(0, 6001))


plot_PS <- resultats_PS %>% ggplot(aes(x = date, y = Nombre, color = Source)) +
    geom_line(lwd = 0.75) +
    scale_color_discrete(labels = c(expression(alpha*I[t]^{1}),
                                    expression(I[t]^{c}),
                                    expression(I[t]^{2}))) +
    theme(legend.title = element_blank(), legend.text = element_text(size = 14),
          axis.title = element_text(size = 11), axis.text = element_text(size = 10)) +
    labs(title = "Paillage synthétique") +
    ylab("Nombre d'inflos") +
    xlab("Date") +
    ylim(c(0, 6001))


plot_EH <- resultats_EH %>% ggplot(aes(x = date, y = Nombre, color = Source)) +
    geom_line(lwd = 0.75) +
    scale_color_discrete(labels = c(expression(alpha*I[t]^{1}),
                                    expression(I[t]^{c}),
                                    expression(I[t]^{2}))) +
    theme(legend.title = element_blank(), legend.text = element_text(size = 14),
          axis.title = element_text(size = 11), axis.text = element_text(size = 10)) +
    labs(title = "Enherbement haut") +
    ylab("Nombre d'inflos") +
    xlab("Date") +
    ylim(c(0, 6001))

# Débourrements -----------------------------------------------------------

burst_ER <- floraison2017_ER %>% arrange(birth) %>% count(birth)
burst_PS <- floraison2017_PS %>% arrange(birth) %>% count(birth)
burst_EH <- floraison2017_EH %>% arrange(birth) %>% count(birth)
    
burstER <- burstPS <- burstEH <- rep(0, length(date2017))
ind_ER <- which(date2017 %in% burst_ER$birth)
ind_PS <- which(date2017 %in% burst_PS$birth)
ind_EH <- which(date2017 %in% burst_EH$birth)

burstER[ind_ER] <- burst_ER$n[-(1:2)]
burstPS[ind_PS] <- burst_PS$n[-(1:3)]
burstEH[ind_EH] <- burst_EH$n[-(1:7)]

burstER[1] <- burstER[1] + sum(burst_ER$n[1:2])
burstPS[1] <- burstPS[1] + sum(burst_PS$n[1:3])
burstEH[1] <- burstEH[1] + sum(burst_EH$n[1:7])

burst <- cbind(burstER, burstPS, burstEH)

# Mise à l'échelle --------------------------------------------------------

new_inflos_ER <- inflos_ER #* sum(inflos_piege_ER)
new_inflos_PS <- inflos_PS #* sum(inflos_piege_PS)
new_inflos_EH <- inflos_EH #* sum(inflos_piege_EH)

new_burst_ER <- burstER * my_alpha_ER #/ floraison_ER$inflos * new_inflos_ER
new_burst_PS <- burstPS * my_alpha_PS #/ floraison_PS$inflos * new_inflos_PS
new_burst_EH <- burstEH * my_alpha_EH #/ floraison_EH$inflos * new_inflos_EH

# new_deads_obs_ER <- deadsER * my_alpha_ER #/ floraison_ER$inflos * new_inflos_ER
# new_deads_obs_PS <- deadsPS * my_alpha_PS #/ floraison_PS$inflos * new_inflos_PS
# new_deads_obs_EH <- deadsEH * my_alpha_EH #/ floraison_EH$inflos * new_inflos_EH
# 
# new_deads_est_ER <- deads_ER * my_alpha_ER #* sum(inflos_piege_ER)
# new_deads_est_PS <- deads_PS * my_alpha_PS #* sum(inflos_piege_PS)
# new_deads_est_EH <- deads_EH * my_alpha_EH #* sum(inflos_piege_EH)
# 
# new_deads_ER <- c(new_deads_est_ER, new_deads_obs_ER[51:length(floraison_ER$deads)])
# new_deads_PS <- c(new_deads_est_PS, new_deads_obs_PS[51:length(floraison_PS$deads)])
# new_deads_EH <- c(new_deads_est_EH, new_deads_obs_EH[51:length(floraison_EH$deads)])

# Morts -------------------------------------------------------------------

deadsER <- cumsum(new_burst_ER) - new_inflos_ER
new_deads_ER <- deadsER - lag(deadsER)
new_deads_ER[1] <- deadsER[1]

deadsPS <- cumsum(new_burst_PS) - new_inflos_PS
new_deads_PS <- deadsPS - lag(deadsPS)
new_deads_PS[1] <- deadsPS[1]

deadsEH <- cumsum(new_burst_EH) - new_inflos_EH
new_deads_EH <- deadsEH - lag(deadsEH)
new_deads_EH[1] <- deadsEH[1]


# Modification ! ----------------------------------------------------------

deads_attra_ER <- rev(lag(rev(new_deads_ER), 7))
deads_attra_ER[1] <- deads_attra_ER[1] + sum(new_deads_ER[1:7])
new_attra_ER <- cumsum(new_burst_ER) - cumsum(deads_attra_ER)
new_attra_ER[74:80] <- floraison_ER$inflos[81:87] * my_alpha_ER

deads_attra_PS <- rev(lag(rev(new_deads_PS), 7))
deads_attra_PS[1] <- deads_attra_PS[1] + sum(new_deads_PS[1:7])
new_attra_PS <- cumsum(new_burst_PS) - cumsum(deads_attra_PS)
new_attra_PS[74:80] <- floraison_PS$inflos[81:87] * my_alpha_PS

deads_attra_EH <- rev(lag(rev(new_deads_EH), 7))
deads_attra_EH[1] <- deads_attra_EH[1] + sum(new_deads_EH[1:7])
new_attra_EH <- cumsum(new_burst_EH) - cumsum(deads_attra_EH)
new_attra_EH[74:80] <- floraison_EH$inflos[81:87] * my_alpha_EH
## Correction valeurs négatives
new_attra_EH[3] <- new_attra_EH[3] - new_attra_EH[2]
new_attra_EH[5] <- new_attra_EH[5] - new_attra_EH[4]
new_attra_EH[12] <- new_attra_EH[12] - sum(new_attra_EH[6:10]) + new_attra_EH[11]
new_attra_EH[c(2, 4, 6, 7, 8, 9, 10, 11)] <- 0

# Plot attra --------------------------------------------------------------

attra_ER <- cbind(date = date2017,
                  Attractives = new_attra_ER,
                  Vivantes = inflos_ER) %>% 
    as_tibble %>% 
    mutate_at("date", as_date) %>% 
    gather(Attractives, Vivantes, key = Inflorescences, value =  Nombre)

attra_ER %>% ggplot(aes(x = date, y = Nombre, color = Inflorescences)) +
    geom_line(lwd = 0.75) +
    xlab("Date") +
    theme(legend.title = element_blank(), legend.text = element_text(size = 14)) +
    scale_color_discrete(labels = c(expression(I[t]^{a}), 
                                    expression(I[t]^{c}))) +
    labs(title = "Enherbement ras") +
    ylim(c(0, 4700))

attra_PS <- cbind(date = date2017,
                  Attractives = new_attra_PS,
                  Vivantes = inflos_PS) %>% 
    as_tibble %>% 
    mutate_at("date", as_date) %>% 
    gather(Attractives, Vivantes, key = Inflorescences, value =  Nombre)

attra_PS %>% ggplot(aes(x = date, y = Nombre, color = Inflorescences)) +
    geom_line(lwd = 0.75) +
    xlab("Date") +
    theme(legend.title = element_blank(), legend.text = element_text(size = 14)) +
    scale_color_discrete(labels = c(expression(I[t]^{a}), 
                                    expression(I[t]^{c}))) +
    labs(title = "Paillage synthétique") +
    ylim(c(0, 4700))

attra_EH <- cbind(date = date2017,
                  Attractives = new_attra_EH,
                  Vivantes = inflos_EH) %>% 
    as_tibble %>% 
    mutate_at("date", as_date) %>% 
    gather(Attractives, Vivantes, key = Inflorescences, value =  Nombre)

attra_EH %>% ggplot(aes(x = date, y = Nombre, color = Inflorescences)) +
    geom_line(lwd = 0.75) +
    xlab("Date") +
    theme(legend.title = element_blank(), legend.text = element_text(size = 14)) +
    scale_color_discrete(labels = c(expression(I[t]^{a}), 
                                    expression(I[t]^{c}))) +
    labs(title = "Enherbement haut") +
    ylim(c(0, 4700))

# Propre ------------------------------------------------------------------

ajusted_ER <- cbind(date = date2017,
                    burst = new_burst_ER,
                    deads = new_deads_ER,
                    inflos = new_inflos_ER) %>%
    as_tibble() %>% 
    mutate_at("date", as_date) %>% 
    mutate(ld = rev(lag(rev(deads), 7))) %>% 
    mutate(decaled = cumsum(burst) - cumsum(ld))

ajusted_PS <- cbind(date = date2017,
                    burst = new_burst_PS,
                    deads = new_deads_PS,
                    inflos = new_inflos_PS) %>% 
    as_tibble() %>% 
    mutate_at("date", as_date) %>% 
    mutate(ld = rev(lag(rev(deads), 7))) %>% 
    mutate(decaled = cumsum(burst) - cumsum(ld))


ajusted_EH <- cbind(date = date2017,
                    burst = new_burst_EH,
                    deads = new_deads_EH,
                    inflos = new_inflos_EH) %>%
    as_tibble() %>% 
    mutate_at("date", as_date) %>% 
    mutate(ld = rev(lag(rev(deads), 7))) %>% 
    mutate(decaled = cumsum(burst) - cumsum(ld))



ajusted_EH %>% ggplot(aes(x = date, y = inflos)) +
    geom_line() +
    geom_line(aes(y = cumsum(burst) - cumsum(deads)), col = "red") +
    geom_line(aes(y = cumsum(burst) - cumsum(ld)), col = "green4")


deads_decalees <- cbind(ajusted_ER$decaled, ajusted_PS$decaled, ajusted_EH$decaled)
deads_decalees[74:80, ] <- cbind(floraison_ER$inflos[81:87] * my_alpha_ER,
                                 floraison_PS$inflos[81:87] * my_alpha_PS,
                                 floraison_EH$inflos[81:87] * my_alpha_EH)

# write.csv(deads_decalees, file = "corrected.csv")

to_plot_ER <- cbind(Date = date2017,
                    IT1 = inflos_piege_ER,
                    ITC = new_inflos_ER,
                    ITA = deads_decalees[, 1]) %>% 
    as_tibble %>% 
    mutate_at("Date", as_date) %>% 
    gather(ITC, ITA, IT1, key = Inflos, value = Nombre) %>% 
    ggplot(aes(x = Date, y = Nombre, color = Inflos)) +
    geom_line(lwd = 0.75) +
    scale_color_discrete(labels = c(expression(I[t]^{1}), 
                                    expression(I[t]^{a}), expression(alpha~I[t]^{c}))) +
    theme(legend.title = element_blank(), legend.text = element_text(size = 14),
          axis.title = element_text(size = 14), axis.text = element_text(size = 13)) +
    labs(title = "Enherbement ras") +
    ylab("Nombre d'inflos") +
    xlab("Date") +
    ylim(c(0, 6001))

to_plot_PS <- cbind(Date = date2017,
                    IT1 = inflos_piege_PS,
                    ITC = new_inflos_PS,
                    ITA = deads_decalees[, 2]) %>% 
    as_tibble %>% 
    mutate_at("Date", as_date) %>% 
    gather(ITC, ITA, IT1, key = Inflos, value = Nombre) %>% 
    ggplot(aes(x = Date, y = Nombre, color = Inflos)) +
    geom_line(lwd = 0.75) +
    scale_color_discrete(labels = c(expression(I[t]^{1}), 
                                    expression(I[t]^{a}), expression(alpha~I[t]^{c}))) +
    theme(legend.title = element_blank(), legend.text = element_text(size = 14),
          axis.title = element_text(size = 14), axis.text = element_text(size = 13)) +
    labs(title = "Paillage synthétique") +
    ylab("Nombre d'inflos") +
    xlab("Date") +
    ylim(c(0, 6001))

to_plot_EH <- cbind(Date = date2017,
                    IT1 = inflos_piege_EH,
                    ITC = new_inflos_EH,
                    ITA = deads_decalees[, 3]) %>% 
    as_tibble %>%
    mutate_at("Date", as_date) %>% 
    gather(ITC, ITA, IT1, key = Inflos, value = Nombre) %>% 
    ggplot(aes(x = Date, y = Nombre, color = Inflos)) +
    geom_line(lwd = 0.75) +
    scale_color_discrete(labels = c(expression(I[t]^{1}), 
                                   expression(I[t]^{a}), expression(alpha~I[t]^{c}))) +
    theme(legend.title = element_blank(), legend.text = element_text(size = 14),
          axis.title = element_text(size = 14), axis.text = element_text(size = 13)) +
    labs(title = "Enherbement haut") +
    ylab("Nombre d'inflos") +
    xlab("Date") +
    ylim(c(0, 6001))


# Comparaison burst simu/obs ----------------------------------------------

simu_burst_ER <- c(350.0134343452573, 453.0494680425028, 111.41292783253148, 105.4176075524184, 106.14146989801527, 106.88419984201273, 107.92798314266074, 27.862121398660715, 26.76922996155252, 25.358363282095244, 25.54519926027305, 26.41473950792908, 27.007805792169464, 28.189144320188845, 122.8007618803285, 128.3152149460844, 101.53006793971205, 102.33544605578878, 104.74898408664677, 102.96824259332047, 121.16128750960435, 134.7787425587716, 137.86280269690303, 137.01578907675332, 139.7596137548663, 142.57200716958042, 145.70417381412688, 154.58366499420154, 37.24443126919395, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 46.82048999967827, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 89.78342873052377, 144.49181086038928, 135.12353798107915, 132.69392868814563, 131.2803785071253, 129.6511363926496, 127.96846943359883, 79.59601412352224, 76.77659518316676, 6.2627699110336845, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 10.67835733741944, 36.910199285431624, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)

simu_burst_PS <- c(231.35134498309577, 263.1403688655092, 35.519398739122536, 31.215276884534056, 31.79127788960595, 31.583174941078404, 27.640386994722814, 14.635660924044249, 14.993744364076822, 6.1507722412770045, 5.63795948726839, 0.7257925024988745, 0.0, 0.0, 75.4595353180211, 82.00232081716146, 17.8869887330277, 22.031034975909087, 22.79195886181951, 10.221408347499747, 57.75228834845634, 7.847338390478435, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 17.390700893578728, 10.609708410549484, 0.0, 19.363579198503697, 39.87554903766926, 40.48034835461954, 21.077981753081588, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 7.623219674981039, 120.08536501224773, 122.45307526157539, 116.3976867246767, 116.15027405087227, 116.60636777369129, 116.8083064709301, 116.82490644302892, 533.106536317362, 560.4567648461133, 140.00994342817992, 132.54929197450312, 133.98619823729632, 133.91811097302968, 73.78742001818966, 0.0, 0.0, 51.87075831583061, 54.81638852567342, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 106.45969010628332, 148.99610796959686, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)

simu_burst_EH <- c(180.25526992154695, 226.4936275997441, 45.307288331510335, 41.830591828513484, 42.459873262367445, 41.513904182456194, 16.444664672369008, 48.81911577305092, 51.78093673625543, 23.632431806994763, 22.215401686226823, 21.092401323262774, 19.75710680770917, 18.939874846341613, 57.85483855055529, 61.264141439995974, 18.558375145392965, 15.228234310169707, 12.339015543243248, 10.107907329043584, 56.57018423922785, 69.7204379136405, 73.39554865212266, 60.48088882226898, 62.52913543682146, 65.04068749631428, 67.8495593231891, 72.64188499930185, 54.048468319005345, 0.0, 0.0, 0.0, 0.0, 13.74297566369447, 18.78743554547672, 8.201569977850445, 0.0, 0.0, 272.0025461613084, 291.05531639789984, 293.04305813638183, 295.13692413595504, 339.0804362944493, 342.20498733445817, 344.48633781059795, 189.67182503217072, 188.37403396318862, 190.65850372114684, 192.83008224721402, 195.24765293695233, 643.2649930205931, 671.037358038544, 356.2633431972951, 353.73502886675954, 358.28397128334166, 356.0093372915361, 11.437054191174589, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)


comp_burst_ER <- cbind(Date = date2017, BTC = cumsum(new_burst_ER), BTS = cumsum(simu_burst_ER)) %>% 
    as_tibble %>% 
    mutate_at("Date", as_date) %>% 
    gather(BTC, BTS, key = type, value = Nombre) %>% 
    ggplot(aes(x = Date, y = Nombre, color = type)) +
    geom_line(lwd = 0.75) +
    theme(legend.title = element_blank(), legend.text = element_text(size = 14)) + 
    ylab("Somme cumulée") +
    scale_color_discrete(labels = c(expression(B[t]^{c}),
                                    expression(B[t]^{s}))) +
    ggtitle("Enherbement ras")

comp_burst_PS <- cbind(Date = date2017, BTC = cumsum(new_burst_PS), BTS = cumsum(simu_burst_PS)) %>% 
    as_tibble %>% 
    mutate_at("Date", as_date) %>% 
    gather(BTC, BTS, key = type, value = Nombre) %>% 
    ggplot(aes(x = Date, y = Nombre, color = type)) +
    geom_line(lwd = 0.75) +
    theme(legend.title = element_blank(), legend.text = element_text(size = 14)) + 
    ylab("Somme cumulée") +
    scale_color_discrete(labels = c(expression(B[t]^{c}),
                                    expression(B[t]^{s}))) +
    ggtitle("Paillage synthétique")


comp_burst_EH <- cbind(Date = date2017, BTC = cumsum(new_burst_EH), BTS = cumsum(simu_burst_EH)) %>% 
    as_tibble %>% 
    mutate_at("Date", as_date) %>% 
    gather(BTC, BTS, key = type, value = Nombre) %>% 
    ggplot(aes(x = Date, y = Nombre, color = type)) +
    geom_line(lwd = 0.75) +
    theme(legend.title = element_blank(), legend.text = element_text(size = 14)) + 
    ylab("Somme cumulée") +
    scale_color_discrete(labels = c(expression(B[t]^{c}),
                                    expression(B[t]^{s}))) +
    ggtitle("Enherbement haut")
