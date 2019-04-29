
# Packages / data ---------------------------------------------------------

library(tidyverse)
library(magrittr)
library(lubridate)
piege <- read.csv("Data/raw/2018_piege.csv", sep = ";") %>% as_tibble()

# Traitement --------------------------------------------------------------

piege %<>% mutate_at("Date", dmy)
piege %<>% mutate_at("Arbre", as.character)

# Séparation des 3 sous-blocs ---------------------------------------------

piegeER <- piege[which(!is.na(piege$Arbre %>% str_extract("ER"))), ]
piegePS <- piege[which(!is.na(piege$Arbre %>% str_extract("PS"))), ]
piegeEH <- piege[which(!is.na(piege$Arbre %>% str_extract("EH"))), ]

# Addition des pieges -----------------------------------------------------

piegeER_A <- piegeER %>% filter(Piege == "A")
piegeER_B <- piegeER %>% filter(Piege == "B")

piegePS_A <- piegePS %>% filter(Piege == "A")
piegePS_B <- piegePS %>% filter(Piege == "B")

piegeEH_A <- piegeEH %>% filter(Piege == "A")
piegeEH_B <- piegeEH %>% filter(Piege == "B")

## Remplacement des NA par la moyenne de la date et du sous-bloc
piegeER_A[c(62, 63, 64, 65), "NbLarves"] <- 0.5
piegeEH_A[c(16, 17, 20), "NbLarves"] <- 0.647
piegeEH_A[33, "NbLarves"] <- 1.421
piegeEH_A[c(83, 90), "NbLarves"] <- 4.944
piegePS_A[4, "NbLarves"] <- 0.26
piegePS_A[c(12, 15, 16, 19), "NbLarves"] <- 0.33
piegePS_B[19, "NbLarves"] <- 0.33
piegePS_B[36, "NbLarves"] <- 0.947
piegePS_A[c(65, 67, 68), "NbLarves"] <- 0.118
piegePS_B[74, "NbLarves"] <- 3.368
piegePS_A[88, "NbLarves"] <- 2.63
piegePS_A[c(101, 105, 108), "NbLarves"] <- 0.235

## Regroupement A et B
piege_ER <- piegeER_A %>% mutate(inflos = piegeER_A$NbInfloPiege + piegeER_B$NbInfloPiege) %>% 
    mutate(larves = piegeER_A$NbLarves + piegeER_B$NbLarves) %>% 
    select(-c(NbInfloPiege, NbLarves, Arbre, Piege, Observations))

piege_PS <- piegePS_A %>% mutate(inflos = piegePS_A$NbInfloPiege + piegePS_B$NbInfloPiege) %>% 
    mutate(larves = piegePS_A$NbLarves + piegePS_B$NbLarves) %>% 
    select(-c(NbInfloPiege, NbLarves, Arbre, Piege, Observations))

piege_EH <- piegeEH_A %>% mutate(inflos = piegeEH_A$NbInfloPiege + piegeEH_B$NbInfloPiege) %>% 
    mutate(larves = piegeEH_A$NbLarves + piegeEH_B$NbLarves) %>% 
    select(-c(NbInfloPiege, NbLarves, Arbre, Piege, Observations))

# larves / inflos / date --------------------------------------------------

piege_ER %<>% group_by(Date) %>% 
    summarise(inflos = sum(inflos), larves = sum(larves)) %>% 
    mutate(ratio = larves / inflos)

piege_PS %<>% group_by(Date) %>% 
    summarise(inflos = sum(inflos), larves = sum(larves)) %>% 
    mutate(ratio = larves / inflos)

piege_EH %<>% group_by(Date) %>% 
    summarise(inflos = sum(inflos), larves = sum(larves)) %>% 
    mutate(ratio = larves / inflos)

# save(piege_ER, piege_PS, piege_EH, file = "piege2018.Rdata")

true_date2018 <- piege$Date %>% unique %>% dmy
