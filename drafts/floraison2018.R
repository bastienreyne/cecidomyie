
# packages / data ---------------------------------------------------------

library(tidyverse)
library(magrittr)
library(lubridate)
floraison <- read.csv("Data/raw/2018_floraison.csv", sep = ";") %>%
    as_tibble %>% 
    filter(NatureBg != "V") %>% 
    mutate(id = str_c(UC, Arbre, TypeBg, NatureBg)) %>% 
    select(-c(Cause1, Cause2, UC, Arbre, TypeBg, NatureBg)) %>% 
    mutate_at("Date", dmy) %>% 
    spread(key = Evenement, value = Date) %>% 
    arrange(Floraison)

# Séparation des 3 sous-blocs ---------------------------------------------

floraison_ER <- floraison[which(!is.na(floraison$id %>% str_extract("ER"))), ]
floraison_PS <- floraison[which(!is.na(floraison$id %>% str_extract("PS"))), ]
floraison_EH <- floraison[which(!is.na(floraison$id %>% str_extract("EH"))), ]

# mise en forme -----------------------------------------------------------

days <- c(floraison$Floraison, floraison$Mort) %>% unique %>% na.omit
days <- as_date(days[1]:days[length(days)])
inflos_ER <- rep(NA, length(days))
inflos_PS <- rep(NA, length(days))
inflos_EH <- rep(NA, length(days))
for (day in 1:length(days)) {
    inflos_ER[day] <- which(floraison_ER$Floraison <= days[day] & floraison_ER$Mort > days[day]) %>% length +
        which(floraison_ER$Floraison <= days[day] & is.na(floraison_ER$Mort)) %>% length
    inflos_PS[day] <- which(floraison_PS$Floraison <= days[day] & floraison_PS$Mort > days[day]) %>% length +
        which(floraison_PS$Floraison <= days[day] & is.na(floraison_PS$Mort)) %>% length
    inflos_EH[day] <- which(floraison_EH$Floraison <= days[day] & floraison_EH$Mort > days[day]) %>% length +
        which(floraison_EH$Floraison <= days[day] & is.na(floraison_EH$Mort)) %>% length
}

# couplage piege / floraison ----------------------------------------------
source("piege2018.R")
day_obs <- which(days %in% piege_PS$Date)
piege_ER %<>% mutate(larve = inflos_ER[day_obs] * piege_ER$ratio)
piege_PS %<>% mutate(larve = inflos_PS[day_obs] * piege_PS$ratio)
piege_EH %<>% mutate(larve = inflos_EH[day_obs] * piege_EH$ratio)
date_ER <- piege_ER$Date
days_ER <- as_date(date_ER[1]:date_ER[length(date_ER)])
laps <- as.numeric(c(1, date_ER[2:15] - date_ER[1:14]))

ER <- cbind(Date = days_ER[-1],
            Inflos = inflos_ER[day_obs[1]:day_obs[length(day_obs)]][-1],
            Larves = rep(piege_ER$larve / laps, laps)[-1]) %>% 
    as_tibble() %>% 
    mutate_at("Date", as_date) %>% 
    mutate(Sol = as_factor("ER"))

PS <- cbind(Date = days_ER[-1],
            Inflos = inflos_PS[day_obs[1]:day_obs[length(day_obs)]][-1],
            Larves = rep(piege_PS$larve / laps, laps)[-1]) %>% 
    as_tibble() %>% 
    mutate_at("Date", as_date) %>% 
    mutate(Sol = as_factor("PS"))

EH <- cbind(Date = days_ER[-1],
            Inflos = inflos_EH[day_obs[1]:day_obs[length(day_obs)]][-1],
            Larves = rep(piege_EH$larve / laps, laps)[-1]) %>% 
    as_tibble() %>% 
    mutate_at("Date", as_date) %>% 
    mutate(Sol = as_factor("EH"))


bloc <- rbind(ER, PS, EH)
bloc %>% ggplot(aes(x = Date, y = Inflos)) +
    geom_line() +
    geom_point(aes(y = Larves)) +
    geom_segment(aes(xend = Date, y = Larves, yend = 0)) +
    facet_grid(Sol ~ .)

piege <- rbind(piege_ER %>% mutate(Sol = "ER"),
               piege_EH %>% mutate(Sol = "EH"),
               piege_PS %>% mutate(Sol = "PS"))
piege %>% ggplot(aes(x = Date, y = inflos)) +
    geom_line() +
    geom_point(aes(y = larves)) +
    geom_segment(aes(xend = Date, y = 0, yend = larves)) +
    facet_grid(Sol ~ .)

# save(bloc, file = "piege2018.Rdata")


# stades phéno ------------------------------------------------------------

floraison_ER %<>% mutate(Stade_E = Floraison + 7,
                         Stade_F = Floraison + 16,
                         Mort = Floraison + 50)

floraison_PS %<>% mutate(Stade_E = Floraison + 7,
                         Stade_F = Floraison + 16,
                         Mort = Floraison + 50)

floraison_EH %<>% mutate(Stade_E = Floraison + 7,
                         Stade_F = Floraison + 16,
                         Mort = Floraison + 50)

load("piege2018.Rdata")
date2018 <- unique(bloc$Date)

stade_C <- matrix(NA, nrow = length(date2018), ncol = 3)
stade_E <- matrix(NA, nrow = length(date2018), ncol = 3)
stade_F <- matrix(NA, nrow = length(date2018), ncol = 3)
for (day in 1:length(date2018)) {
    date <- date2018[day]
    ## Stade C
    stade_C[day, 1] <- floraison_ER %$% which(Floraison >= date & date < Stade_E) %>% length
    stade_C[day, 2] <- floraison_PS %$% which(Floraison >= date & date < Stade_E) %>% length
    stade_C[day, 3] <- floraison_EH %$% which(Floraison >= date & date < Stade_E) %>% length
    
    ## Stade E
    stade_E[day, 1] <- floraison_ER %$% which(Stade_E >= date & date < Stade_F) %>% length
    stade_E[day, 2] <- floraison_PS %$% which(Stade_E >= date & date < Stade_F) %>% length
    stade_E[day, 3] <- floraison_EH %$% which(Stade_E >= date & date < Stade_F) %>% length
    
    ## Stade F
    stade_F[day, 1] <- floraison_ER %$% which(Stade_F >= date & date < Mort) %>% length
    stade_F[day, 2] <- floraison_PS %$% which(Stade_F >= date & date < Mort) %>% length
    stade_F[day, 3] <- floraison_EH %$% which(Stade_F >= date & date < Mort) %>% length
}

save(stade_C, stade_E, stade_F, file = "stades.Rdata")


## plot
stades <- rbind(stade_C, stade_E, stade_F) %>% as_tibble
colnames(stades) <- c("ER", "PS", "EH")
stades %<>% cbind(stade = rep(c("C", "E", "F"), each = 52), date = rep(date2018, 3)) %>% 
    gather(ER, PS, EH, key = Sol, value = Nombre)
stades %>% ggplot(aes(x = date, y = Nombre, fill = stade)) +
    geom_area(position = 'dodge', alpha = 0.4) +
    facet_grid(Sol ~ .)
