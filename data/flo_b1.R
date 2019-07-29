## Script qui édite les dynamiques d'inflorescences issues du dataset 1 pour le bloc 1
## Dynamiques non-corrigées
## Les inflos où pas de dates de morts supprimés (on garde les dates de débourrements, though)

# Packages / data ---------------------------------------------------------

library(tidyverse)
library(magrittr)
library(lubridate)

## Chargement data
inflos_piege <- read_csv("2017_piege.csv") %>% select(date, inflos, Sol)
inflos_floraison <- read_csv2("raw/floraison0.csv")

## Sélection Bloc 2
inflos_floraison %<>% mutate_at(c("ApdateC", "ApdateM",
                                  c(paste0("Lat",1:5,"dateC"),
                                    paste0("Lat", 1:5, "dateM"))), dmy) %>% 
    filter(Annee == 2017) %>% 
    filter(Bloc == 1)

## Bon nom de sous-bloc
inflos_floraison$Traitm[which(inflos_floraison$Traitm == "Sn")] <- "ER"
inflos_floraison$Traitm[which(inflos_floraison$Traitm == "B")] <- "PS"
inflos_floraison$Traitm[which(inflos_floraison$Traitm == "E")] <- "EH"

# Mise en forme inflo floraison -------------------------------------------

## Conpilation de toutes les inflos 
inflos_apic <- inflos_floraison %>% 
    filter(!is.na(ApdateC)) %>%
    select(Traitm, ApdateC, ApdateM) %>% 
    rename(birth = ApdateC, death = ApdateM)

inflos_lat1 <- inflos_floraison %>%
    filter(!is.na(Lat1dateC)) %>%
    select(Traitm, Lat1dateC, Lat1dateM) %>%
    rename(birth = Lat1dateC, death = Lat1dateM)

inflos_lat2 <- inflos_floraison %>% 
    filter(!is.na(Lat2dateC)) %>%
    select(Traitm, Lat2dateC, Lat2dateM) %>%
    rename(birth = Lat2dateC, death = Lat2dateM)

inflos_lat3 <- inflos_floraison %>%
    filter(!is.na(Lat3dateC)) %>%
    select(Traitm, Lat3dateC, Lat3dateM) %>%
    rename(birth = Lat3dateC, death = Lat3dateM)

inflos_lat4 <- inflos_floraison %>%
    filter(!is.na(Lat4dateC)) %>%
    select(Traitm, Lat4dateC, Lat4dateM) %>%
    rename(birth = Lat4dateC, death = Lat4dateM)

inflos_lat5 <- inflos_floraison %>%
    filter(!is.na(Lat5dateC)) %>%
    select(Traitm, Lat5dateC, Lat5dateM) %>%
    rename(birth = Lat5dateC, death = Lat5dateM)

floraison2017 <- rbind(inflos_apic, inflos_lat1, inflos_lat2,
                       inflos_lat3, inflos_lat4, inflos_lat5) %>% 
    arrange(birth)

## Correction de la date.
floraison2017$birth <- floraison2017$birth - 365
floraison2017$death <- floraison2017$death - 365

## Suppression valeurs manquantes
floraison2017_omit <- na.omit(floraison2017)

## Dates
date <- floraison2017_omit %$% unique(c(birth, death))
date <- min(date):max(date) %>% as_date

## Tableau dates débourrements / morts / sol

# write.csv(floraison2017_omit, file = "2017_bursts_death_bloc1.csv")

# Tableau inflos ----------------------------------------------------------

inflos1 <- data.frame(date = date, inflosER = NA, inflosPS = NA, inflosEH = NA)
for (day in 1:length(date)) {
    ## ER
    inflos1[day, 2] <- floraison2017 %>% 
        filter(Traitm == "ER") %$%
        which(birth <= date[day] & death > date[day]) %>% 
        length
    ## PS
    inflos1[day, 3] <- floraison2017 %>% 
        filter(Traitm == "PS") %$%
        which(birth <= date[day] & death > date[day]) %>% 
        length
    ## EH
    inflos1[day, 4] <- floraison2017 %>% 
        filter(Traitm == "EH") %$%
        which(birth <= date[day] & death > date[day]) %>% 
        length
}

# write.csv(inflos1, file = "2017_inflos_dataset1_bloc1.csv")

# Tableau count burst / death ---------------------------------------------

bursts_death <- data.frame(date = date, burstER = NA, deathER = NA,
                           burstPS = NA, deathPS = NA,
                           burstEH = NA, deathEH = NA)
for (day in 1:length(date)) {
    ## ER -- debourrements
    bursts_death[day, 2] <- floraison2017 %>% 
        filter(Traitm == "ER") %$% 
        which(birth == date[day]) %>% 
        length()
    ## ER -- morts
    bursts_death[day, 3] <- floraison2017_omit %>% 
        filter(Traitm == "ER") %$%
        which(death == date[day]) %>% 
        length
    ## PS -- debourrements
    bursts_death[day, 4] <- floraison2017 %>% 
        filter(Traitm == "PS") %$% 
        which(birth == date[day]) %>% 
        length()
    ## PS -- morts
    bursts_death[day, 5] <- floraison2017_omit %>% 
        filter(Traitm == "PS") %$%
        which(death == date[day]) %>% 
        length
    ## EH -- debourrements
    bursts_death[day, 6] <- floraison2017 %>% 
        filter(Traitm == "EH") %$% 
        which(birth == date[day]) %>% 
        length()
    ## EH -- morts
    bursts_death[day, 7] <- floraison2017_omit %>% 
        filter(Traitm == "EH") %$%
        which(death == date[day]) %>% 
        length
}

# write.csv(bursts_death, file = "2017_bursts_death_bloc1.csv")