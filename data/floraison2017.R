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
# write.csv(floraison2017, file = "2017_floraison_not_corrected.csv")

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
# alive <- rep(NA, length(date))
# dead <- rep(NA, length(date))
# alive_ER <- rep(NA, length(date))
# alive_PS <- rep(NA, length(date))
# alive_EH <- rep(NA, length(date))
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
    # alive[day] <- floraison2017 %$% which(birth == date[day]) %>% na.omit %>% length
    # dead[day] <- floraison2017 %$% which(death == date[day]) %>% na.omit %>%  length
    # alive_ER[day] <- floraison2017_ER %$% which(birth == date[day]) %>% na.omit %>%  length
    # alive_PS[day] <- floraison2017_PS %$% which(birth == date[day]) %>% na.omit %>%  length
    # alive_EH[day] <- floraison2017_EH %$% which(birth == date[day]) %>% na.omit %>%  length
}

floraison_ER <- cbind(date, inflos = inflos_floraison_ER, deads = inflos_deads_ER) %>% 
    as_tibble %>% 
    mutate_at("date", as_date) %>%
    filter(date >= "2017-07-19") %>%
    filter(date < "2017-10-04") %>% 
    mutate(Sol = "ER")

floraison_PS <- cbind(date, inflos = inflos_floraison_PS, deads = inflos_deads_PS) %>%
    as_tibble %>% 
    mutate_at("date", as_date) %>%
    filter(date >= "2017-07-19") %>% 
    filter(date < "2017-10-04") %>% 
    mutate(Sol = "PS")

floraison_EH <- cbind(date, inflos = inflos_floraison_EH, deads = inflos_deads_EH) %>%
    as_tibble %>% 
    mutate_at("date", as_date) %>%
    filter(date >= "2017-07-19") %>%
    filter(date < "2017-10-04") %>% 
    mutate(Sol = "EH")

floraison_all <- cbind(date, inflos = inflos_floraison_all, deads = inflos_deads_all) %>%
    as_tibble %>% 
    mutate_at("date", as_date) %>%
    filter(date >= "2017-07-19") %>%
    filter(date < "2017-10-04") %>% 
    mutate(Sol = "all")


floraison <- rbind(floraison_ER, floraison_PS, floraison_EH, floraison_all)


# write.csv(floraison, file = "2017_floraison_not_corrected.csv")

# Correction --------------------------------------------------------------

index <- floraison_ER %$% which(date == "2017-09-05" | date == "2017-09-06")
ecart_ER <- (floraison_ER[49, 2] - floraison_ER[50, 2]) %>%
    as.numeric() #/ sum(floraison_ER$inflos)

ecart_PS <- (floraison_PS[49, 2] - floraison_PS[50, 2]) %>%
    as.numeric() #/ sum(floraison_PS$inflos)

ecart_EH <- (floraison_EH[49, 2] - floraison_EH[50, 2]) %>% 
    as.numeric() #/ sum(floraison_EH$inflos)

date2017 <- floraison_ER$date

inflos_piege_ER <- inflos_piege %>%
    filter(Sol == "ER") %>%
    filter(date < "2017-10-04") %>% 
    pull(inflos)

inflos_piege_PS <- inflos_piege %>%
    filter(Sol == "PS") %>%
    filter(date < "2017-10-04") %>% 
    pull(inflos)

inflos_piege_EH <- inflos_piege %>%
    filter(Sol == "EH") %>%
    filter(date < "2017-10-04") %>% 
    pull(inflos)

inflos_target_ER <- inflos_piege_ER #/ sum(inflos_piege_ER)
inflos_target_PS <- inflos_piege_PS #/ sum(inflos_piege_PS)
inflos_target_EH <- inflos_piege_EH #/ sum(inflos_piege_EH)

inflos_current_ER <- floraison_ER$inflos #/ sum(floraison_ER$inflos)
inflos_current_PS <- floraison_PS$inflos #/ sum(floraison_PS$inflos)
inflos_current_EH <- floraison_EH$inflos #/ sum(floraison_EH$inflos)

# inflos_ER <- floraison_ER$inflos
# inflos_ER[16:49] <- inflos_ER[16:49] - seq(0, 16, length.out = 35)
# inflos_ER <- inflos_ER / sum(inflos_ER)
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
    inflos[1:49] <- inflos[1:49] - cumsum(morts)[1:49]
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

deads_EH <- my_x_EH * ecart_EH / sum(my_x_EH)

inflos_EH <- inflos_current_EH
inflos_EH[1:49] <- inflos_EH[1:49] - cumsum(deads_EH)[1:49]
inflos_EH <- inflos_EH * my_alpha_EH

plot(date2017, inflos_target_EH)
lines(date2017, inflos_EH)
lines(date2017, inflos_current_EH, col = "red")


objectiveER <- function(x) {
    
    poids <- x[1:50]
    morts <- poids * ecart_ER / sum(poids)
    
    inflos <- inflos_current_ER
    inflos[1:49] <- inflos[1:49] - cumsum(morts)[1:49]
    my_rmse(x[51] * inflos, inflos_target_ER)
}


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

plot(date2017, inflos_target_ER)
lines(date2017, inflos_ER)
lines(date2017, inflos_current_ER, col = "red")

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



ajusted_ER %>% ggplot(aes(x = date, y = inflos)) +
    geom_line() +
    geom_line(aes(y = cumsum(burst) - cumsum(deads)), col = "red") +
    geom_line(aes(y = cumsum(burst) - cumsum(ld)), col = "green4")


deads_decalees <- cbind(ajusted_ER$decaled, ajusted_PS$decaled, ajusted_EH$decaled)
deads_decalees[71:77, ] <- 0

write.csv(deads_decalees, file = "corrected.csv")
