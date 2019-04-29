## Script pour prendre les inflorescences du fichier floraison et les corriger pour les faire
## coincider avec les inflos du fichier piege.

# Packages / data ---------------------------------------------------------

library(tidyverse)
library(magrittr)
library(lubridate)

inflos_piege_ER <- read_csv2("Data/2017_B1_enh.ras.csv") %>% select(date, inflos_vivantes)
inflos_piege_PS <- read_csv2("Data/2017_B1_bache.csv") %>% select(date, inflos_vivantes)
inflos_piege_EH <- read_csv2("Data/2017_B1_enh.haut.csv") %>% select(date, inflos_vivantes)

inflos_floraison <- read_csv2("Data/raw/2017_floraison.csv")
inflos_floraison %<>% mutate_at(c("ApdateC", "ApdateM",
                                  c(paste0("Lat",1:5,"dateC"), paste0("Lat", 1:5, "dateM"))), dmy) %>% 
    filter(Annee == 2017) %>% 
    mutate_at("Traitm", as_factor) %>% 
    filter(Bloc == 1)

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
                       inflos_lat3, inflos_lat4, inflos_lat5)

floraison2017_ER <- floraison2017 %>% filter(Traitm == "Sn")
floraison2017_PS <- floraison2017 %>% filter(Traitm == "B")
floraison2017_EH <- floraison2017 %>% filter(Traitm == "E")

date <- floraison2017 %$% unique(c(birth, death)) %>% na.omit
date <- min(date):max(date) %>% as_date

inflos_floraison_ER <- rep(NA, length(date))
inflos_floraison_PS <- rep(NA, length(date))
inflos_floraison_EH <- rep(NA, length(date))
alive <- rep(NA, length(date))
dead <- rep(NA, length(date))
alive_ER <- rep(NA, length(date))
alive_PS <- rep(NA, length(date))
alive_EH <- rep(NA, length(date))
for (day in 1:length(date)) {
    inflos_floraison_ER[day] <- floraison2017_ER %$% which(birth <= date[day] & death > date[day]) %>%
        length + floraison2017_ER %$% which(birth <= date[day] & is.na(death)) %>% length
    inflos_floraison_PS[day] <- floraison2017_PS %$% which(birth <= date[day] & death > date[day]) %>%
        length + floraison2017_ER %$% which(birth <= date[day] & is.na(death)) %>% length
    inflos_floraison_EH[day] <- floraison2017_EH %$% which(birth <= date[day] & death > date[day]) %>%
        length + floraison2017_ER %$% which(birth <= date[day] & is.na(death)) %>% length
    alive[day] <- floraison2017 %$% which(birth == date[day]) %>% na.omit %>% length
    dead[day] <- floraison2017 %$% which(death == date[day]) %>% na.omit %>%  length
    alive_ER[day] <- floraison2017_ER %$% which(birth == date[day]) %>% na.omit %>%  length
    alive_PS[day] <- floraison2017_PS %$% which(birth == date[day]) %>% na.omit %>%  length
    alive_EH[day] <- floraison2017_EH %$% which(birth == date[day]) %>% na.omit %>%  length
}

inflos_floraison_ER <- cbind(date, inflos = inflos_floraison_ER) %>% as_tibble %>% 
    mutate_at("date", as_date) %>% filter(date >= "2018-07-18")
inflos_floraison_PS <- cbind(date, inflos = inflos_floraison_PS) %>% as_tibble %>% 
    mutate_at("date", as_date) %>% filter(date >= "2018-07-18")
inflos_floraison_EH <- cbind(date, inflos = inflos_floraison_EH) %>% as_tibble %>% 
    mutate_at("date", as_date) %>% filter(date >= "2018-07-18")

inflos_floraison_ER$date <- inflos_floraison_ER$date - 365
inflos_floraison_PS$date <- inflos_floraison_PS$date - 365
inflos_floraison_EH$date <- inflos_floraison_EH$date - 365

# Correction --------------------------------------------------------------

index <- inflos_floraison_ER %$% which(date == "2017-08-01" | date == "2017-09-05" | date == "2017-09-06")
ecart_ER <- (inflos_floraison_ER[50, 2] - inflos_floraison_ER[51, 2]) %>%
    as.numeric() / sum(inflos_floraison_ER$inflos)
ecart_PS <- (inflos_floraison_PS[50, 2] - inflos_floraison_PS[51, 2]) %>%
    as.numeric() / sum(inflos_floraison_PS$inflos)
ecart_EH <- (inflos_floraison_EH[50, 2] - inflos_floraison_EH[51, 2]) %>% 
    as.numeric() / sum(inflos_floraison_EH$inflos)
date2017 <- inflos_floraison_ER$date

inflos_piege_ER <- approx(inflos_piege_ER$date %>% as.numeric,
                           inflos_piege_ER$inflos_vivantes,
                           xout = date2017 %>% as.numeric)$y

inflos_piege_PS <- approx(inflos_piege_PS$date %>% as.numeric,
                           inflos_piege_PS$inflos_vivantes,
                           xout = date2017 %>% as.numeric)$y

inflos_piege_EH <- approx(inflos_piege_EH$date %>% as.numeric,
                           inflos_piege_EH$inflos_vivantes,
                           xout = date2017 %>% as.numeric)$y

inflos_target_ER <- inflos_piege_ER / sum(inflos_piege_ER)
inflos_target_PS <- inflos_piege_PS / sum(inflos_piege_PS)
inflos_target_EH <- inflos_piege_EH / sum(inflos_piege_EH)

inflos_current_ER <- inflos_floraison_ER$inflos / sum(inflos_floraison_ER$inflos)
inflos_current_PS <- inflos_floraison_PS$inflos / sum(inflos_floraison_PS$inflos)
inflos_current_EH <- inflos_floraison_EH$inflos / sum(inflos_floraison_EH$inflos)

inflos_ER <- inflos_floraison_ER$inflos
inflos_ER[16:50] <- inflos_ER[16:50] - seq(0, 16, length.out = 35)
inflos_ER <- inflos_ER / sum(inflos_ER)
## Corriger entre le 1er aout et le 5 septembre

# plot(date2017, inflos_target_ER)
# lines(date2017, inflos_ER)
# 
# plot(date2017, inflos_target_PS)
# lines(date2017, inflos_current_PS)

my_rmse <- function(x, y) {
    n_obs <- length(x)
    sqrt(sum((x - y)^2) / n_obs ) 
}

objectiveEH <- function(x) {
    
    poids <- x[1:50]
    morts <- poids * ecart_EH / sum(poids)
    
    inflos <- inflos_current_EH
    inflos[1:50] <- inflos[1:50] - cumsum(morts)
    my_rmse(x[51] * inflos, inflos_target_EH)
}

library(mco)
resEH <- nsga2(objectiveEH, 51, 1,
             lower.bounds = c(rep(0, 51)),
             upper.bounds = c(rep(100, 50), 6000),
             popsize = 200, 
             generations = 100)

my_x_EH <- resEH$par[1, 1:50]
my_alpha_EH <- resEH$par[1, 51]

deads <- my_x_EH * ecart_EH / sum(my_x_EH)

inflos_EH <- inflos_current_EH
inflos_EH[1:50] <- inflos_EH[1:50] - cumsum(deads)
inflos_EH <- inflos_EH * my_alpha_EH

plot(date2017, inflos_target_EH)
lines(date2017, inflos_EH)
lines(date2017, inflos_current_EH, col = "red")


objectiveER <- function(x) {
    
    poids <- x[1:50]
    morts <- poids * ecart_ER / sum(poids)
    
    inflos <- inflos_current_ER
    inflos[1:50] <- inflos[1:50] - cumsum(morts)
    my_rmse(x[51] * inflos, inflos_target_ER)
}


resER <- nsga2(objectiveER, 51, 1,
               lower.bounds = c(rep(0, 51)),
               upper.bounds = c(rep(100, 50), 6000),
               popsize = 200, 
               generations = 100)

my_x_ER <- resER$par[1, 1:50]
my_alpha_ER <- resER$par[1, 51]

deads <- my_x * ecart_ER / sum(my_x)

inflos_ER <- inflos_current_ER
inflos_ER[1:50] <- inflos_ER[1:50] - cumsum(deads)
inflos_ER <- inflos_ER * my_alpha_ER

plot(date2017, inflos_target_ER)
lines(date2017, inflos_ER)
lines(date2017, inflos_current_ER, col = "red")

## PS

objectivePS <- function(x) {
    
    poids <- x[1:50]
    morts <- poids * ecart_PS / sum(poids)
    
    inflos <- inflos_current_PS
    inflos[1:50] <- inflos[1:50] - cumsum(morts)
    my_rmse(x[51] * inflos, inflos_target_PS)
}


resPS <- nsga2(objectivePS, 51, 1,
               lower.bounds = c(rep(0, 51)),
               upper.bounds = c(rep(100, 50), 6000),
               popsize = 200, 
               generations = 100)

my_x_PS <- resPS$par[1, 1:50]
my_alpha_PS <- resPS$par[1, 51]

deads <- my_x * ecart_PS / sum(my_x)

inflos_PS <- inflos_current_PS
inflos_PS[1:50] <- inflos_PS[1:50] - cumsum(deads)
inflos_PS <- inflos_PS * my_alpha_PS

plot(date2017, inflos_target_PS)
lines(date2017, inflos_PS)
lines(date2017, inflos_current_PS, col = "red")


resultats_ER <- cbind(date = date2017,
                   corrected = inflos_ER,
                   piege = inflos_target_ER,
                   floraison = my_alpha_ER * inflos_current_ER) %>% as_tibble %>% 
    mutate_at("date", as_date) %>% 
    gather(corrected, piege, floraison, key = Source, value = Nombre)

resultats_PS <- cbind(date = date2017,
                      corrected = inflos_PS,
                      piege = inflos_target_PS,
                      floraison = my_alpha_PS * inflos_current_PS) %>% as_tibble %>% 
    mutate_at("date", as_date) %>% 
    gather(corrected, piege, floraison, key = Source, value = Nombre)

resultats_EH <- cbind(date = date2017,
                      corrected = inflos_EH,
                      piege = inflos_target_EH,
                      floraison = my_alpha_EH * inflos_current_EH) %>% as_tibble %>% 
    mutate_at("date", as_date) %>% 
    gather(corrected, piege, floraison, key = Source, value = Nombre)

plot_ER <- resultats_ER %>% ggplot(aes(x = date, y = Nombre, color = Source)) +
    geom_point() + 
    geom_line()


plot_PS <- resultats_PS %>% ggplot(aes(x = date, y = Nombre, color = Source)) +
    geom_point() + 
    geom_line()


plot_EH <- resultats_EH %>% ggplot(aes(x = date, y = Nombre, color = Source)) +
    geom_point() + 
    geom_line()


library(gridExtra)
grid.arrange(plot_ER, plot_PS, plot_EH, nrow = 3)


resultat <- cbind(inflos_ER = inflos_ER * sum(inflos_piege_ER),
                  inflos_PS = inflos_PS * sum(inflos_piege_PS),
                  inflos_EH = inflos_EH * sum(inflos_piege_EH))

burst_ER <- (floraison2017_ER %>% arrange(birth) %>% count(birth))
burst_PS <- (floraison2017_PS %>% arrange(birth) %>% count(birth))
burst_EH <- (floraison2017_EH %>% arrange(birth) %>% count(birth))
burst_ER$birth <- burst_ER$birth - 365
burst_PS$birth <- burst_PS$birth - 365
burst_EH$birth <- burst_EH$birth - 365

burstER <- burstPS <- burstEH <- rep(0, length(date2017))
ind_ER <- which(date2017 %in% burst_ER$birth)
ind_PS <- which(date2017 %in% burst_PS$birth)
ind_EH <- which(date2017 %in% burst_EH$birth)

burstER[ind_ER] <- burst_ER$n[-(1:2)]
burstPS[ind_PS] <- burst_PS$n[-(1:3)]
burstEH[ind_EH] <- burst_EH$n[-(1:7)]

burst <- cbind(burstER, burstPS, burstEH)
