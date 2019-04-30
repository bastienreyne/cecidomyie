library(tidyverse)
library(magrittr)
library(lubridate)
library(readxl)

## Importation des données
data <- read_csv2("raw/2017_floraison.csv")
data %<>% mutate_at(c("ApdateC", "ApdateM",
                      c(paste0("Lat",1:5,"dateC"),
                        paste0("Lat", 1:5, "dateM"))), dmy)
    
obs_floraison <- function(bloc = 1, modalite = NULL, annee = 2017) {
    
    data %<>% filter(Bloc == bloc & Annee == annee)
    if (!is.null(modalite))
        data %<>% filter(Traitm == modalite)
    
    ## On réorganise les données 1 ligne = 1 inflo
    da <- data %>% filter(!is.na(ApdateC)) %>% select(ApdateC, ApdateM) %>% 
        rename(birth = ApdateC, death = ApdateM)
    dl1 <- data %>% filter(!is.na(Lat1dateC)) %>% select(Lat1dateC, Lat1dateM) %>%
        rename(birth = Lat1dateC, death = Lat1dateM)
    dl2 <- data %>% filter(!is.na(Lat2dateC)) %>% select(Lat2dateC, Lat2dateM) %>%
        rename(birth = Lat2dateC, death = Lat2dateM)
    dl3 <- data %>% filter(!is.na(Lat3dateC)) %>% select(Lat3dateC, Lat3dateM) %>%
        rename(birth = Lat3dateC, death = Lat3dateM)
    dl4 <- data %>% filter(!is.na(Lat4dateC)) %>% select(Lat4dateC, Lat4dateM) %>%
        rename(birth = Lat4dateC, death = Lat4dateM)
    dl5 <- data %>% filter(!is.na(Lat5dateC)) %>% select(Lat5dateC, Lat5dateM) %>%
        rename(birth = Lat5dateC, death = Lat5dateM)
    
    inflo <- bind_rows(da,dl1,dl2,dl3,dl4,dl5) %>% arrange(birth) %>% mutate(stadeC_theo = birth, stadeE_theo = birth+7, stadeF_theo = birth+16, mortes_theo = birth + 50)
    
    days <- unique(c(inflo$birth, inflo$death))
    days <- days[which(!is.na(days))]
    days <- as_date(days[1]:days[length(days)])
    
    alive <- rep(NA, length(days))
    new <- rep(NA, length(days))
    dead <- rep(NA, length(days))
    alive_theo <- rep(NA, length(days))
    stadeC_theo <- rep(NA, length(days))
    stadeE_theo <- rep(NA, length(days))
    stadeF_theo <- rep(NA, length(days))
    dead_theo <- rep(NA, length(days))
    for (day in 1:length(days)){
        alive[day] <- length(which(inflo$birth <= days[day] & inflo$death > days[day])) +
            length(which(inflo$birth <= days[day] & is.na(inflo$death)))
        new[day] <- length(which(inflo$birth == days[day]))
        dead[day] <- length(which(inflo$death <= days[day]))
        alive_theo[day] <- length(which(inflo$birth <= days[day] & inflo$mortes_theo > days[day]))
        stadeC_theo[day] <- length(which(inflo$stadeC_theo <= days[day] & inflo$stadeE_theo > days[day]))
        stadeE_theo[day] <- length(which(inflo$stadeE_theo <= days[day] & inflo$stadeF_theo > days[day]))
        stadeF_theo[day] <- length(which(inflo$stadeF_theo <= days[day] & inflo$mortes_theo > days[day]))
        dead_theo[day] <- length(which(inflo$mortes_theo <= days[day]))
    }
    
    res <- as_tibble(cbind(days, alive, dead, new, alive_theo, stadeC_theo, stadeE_theo, stadeF_theo, dead_theo))
    res %<>% mutate_at("days", list(as_date)) %>% mutate(Bloc = paste("Bloc", bloc)) %>% mutate(Sol = modalite)
    
    # write_csv2(res, paste0("2017_flo_B", bloc,"_", modalite,".csv"))
    return(res)
    
}

b1 <- obs_floraison(modalite = "B")
r1 <- obs_floraison(modalite = "Sn")
h1 <- obs_floraison(modalite = "E")
b2 <- obs_floraison(modalite = "B", bloc = 2)
r2 <- obs_floraison(modalite = "Sn", bloc = 2)
h2 <- obs_floraison(modalite = "E", bloc = 2)

final <- bind_rows(b1,h1,r1,b2,r2,h2)
# write_csv2(final, "2017_floraison.csv")
