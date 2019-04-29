library(tidyverse)
library(readxl)
library(magrittr)
library(lubridate)

## Importation des données
piege <- read_excel("/home/bastien/Stage/Moi/piege0.xlsx", sheet = 3)
piege <- piege[-which(piege$bloc==2 & piege$trait=='enh.ras' & piege$arbre==19),] ## suppression arbre 19
piege <- piege[-which(piege$bloc==2 & piege$trait=='enh.ras' & piege$arbre==36),] ## suppression arbre 36

## 2100 observations
piege %<>% mutate_at(vars(starts_with("nb.")),list(as.numeric))
piege %<>% mutate_at(c("piege", "trait"), list(as.factor))
piege %<>% mutate_at("date", list(as.Date))
piege %<>% select(-c(observations, nb.larves.estima, larve))
piege %<>% arrange(arbre) %>% arrange(trait) %>% arrange(date)

piege2 <- piege %>% filter(bloc==2)
## Concentrons-nous sur le bloc1
piege %<>% filter(bloc==1)

## Correction des noms de piège
bad_ind <- which(piege$piege=="B")[which(which(piege$piege=="B") %% 2 == 1)]
piege[bad_ind,]$piege <- "A"

## Gestion des NA
piege[which(is.na(piege$nb.larves) & piege$nb.inflo.piege == 0), ]$nb.larves <- 0
piege[which(is.na(piege$nb.inflo.piege)), ]$nb.inflo.piege <- 2.25
piege[which(is.na(piege$nb.inflo)), ]$nb.inflo <- 26.55
piege[which(is.na(piege$nb.larves) & piege$trait == "bache"), ]$nb.larves <- 2.57
piege[which(is.na(piege$nb.larves) & piege$trait == "enh.haut"), ]$nb.larves <- 1.5
piege[which(is.na(piege$nb.larves) & piege$trait == "enh.ras"), ]$nb.larves <- 2.63

piegeA <- piege %>% filter(date < "2017-08-17")
piegeAB <- piege %>% filter(date > "2017-08-17")

estimation_piege_bloc1 <- function(modalite = NULL){
    ## Renvoie un fichier .csv contenant les estimations du nombre de larves dans la parcelle, le nombre d'inflorescences vivante dans la parcelle, le nombre d'inflorescences mortes et le rapport de larves/inflo_vivantes. Les estimations sont données pour chaque date.
    ## Argument : modalité prend "bache", "enh.ras" ou "enh.haut". Par défaut NULL renvoie pour le bloc entier
    if (!is.null(modalite)){
        piegeA %<>% filter(trait==modalite)
        piegeAB %<>% filter(trait==modalite)
    }
    
    ### Initialisation data
    nb.arbre.bloc <- 153
    nb.arbre.bache <- 45
    nb.arbre.ras <- 49
    nb.arbre.haut <- 59
    larves <- rep(NA, 20)
    inflos_vivantes <- rep(NA, 20)
    inflos_mortes <- rep(NA, 20)
    larves_inflos <- rep(NA, 20)
    
    ### Avant 2017-08-17 (un seul piège)
    piegeA %<>% mutate(larves.arbre = nb.larves * nb.inflo / nb.inflo.piege)
    piegeA$larves.arbre[which(is.na(piegeA$larves.arbre) | is.infinite(piegeA$larves.arbre))] <- 0
    
    dateA <- unique(piegeA$date)
    
    for (day in 1:length(dateA)){
        aux <- piegeA %>% filter(date==dateA[day])
        larves[day] <- sum(aux$larves.arbre)
        inflos_vivantes[day] <- sum(aux$nb.inflo)
        inflos_mortes[day] <- sum(aux$nb.inflo.morte)
    }
    
    ### Après 2017-08-17 (deux pièges)
    dateAB <- unique(piegeAB$date)
    
    ## Séparation 
    piegeAB_A <- piegeAB %>% filter(piege=="A")
    piegeAB_B <- piegeAB %>% filter(piege=="B")
    
    piegeAB_A %<>% bind_cols(nb.larves_B = piegeAB_B$nb.larves, nb.inflo.piege_B = piegeAB_B$nb.inflo.piege)
    piegeAB_A %<>% mutate(nb.larvesAB = nb.larves+nb.larves_B, nb.inflo.piegeAB = nb.inflo.piege+nb.inflo.piege_B) %>% mutate(larves.arbre = nb.larvesAB * nb.inflo / nb.inflo.piegeAB)
    piegeAB_A$larves.arbre[which(is.na(piegeAB_A$larves.arbre) | is.infinite(piegeAB_A$larves.arbre))] <- 0
    
    for (day in 1:length(dateAB)){
        aux <- piegeAB_A %>% filter(date==dateAB[day])
        
        larves[day+length(dateA)] <- sum(aux$larves.arbre)
        inflos_vivantes[day+length(dateA)] <- sum(aux$nb.inflo)
        inflos_mortes[day+length(dateA)] <- sum(aux$nb.inflo.morte)
    }
    
    arbre.piege <- dim(piegeA %>% filter(date=="2017-07-18"))[1]
    if (is.null(modalite)){
        coef <- nb.arbre.bloc/arbre.piege
    } else {
        if (modalite == "bache")
            coef <- nb.arbre.bache/arbre.piege
        if (modalite == "enh.ras")
            coef <- nb.arbre.ras/arbre.piege
        if (modalite == "enh.haut")
            coef <- nb.arbre.haut/arbre.piege
    }

    larves <- coef * larves
    inflos_vivantes <- coef * inflos_vivantes
    inflos_mortes <- coef * inflos_mortes
    larves_inflos <- larves / inflos_vivantes
    
    df <- as_tibble(cbind(date = c(dateA, dateAB), larves = larves, inflos_vivantes = inflos_vivantes, inflos_mortes, larves_inflos = larves_inflos))%>% mutate(Bloc = "Bloc 1") %>% mutate(Sol = modalite)
    df$date <- as_date(df$date)
    
    #write.csv2(df, paste0("Bloc1_",modalite,".csv"))
    return(df)
}



## Bloc 2

## gestion des NA

bad_ind <- which(piege2$piege=="B")[which(which(piege2$piege=="B") %% 2 == 1)]
piege2[bad_ind,]$piege <- "A"

piege2[which(is.na(piege2$nb.inflo.piege)), ]$nb.inflo.piege <- 0.84
piege2[which(is.na(piege2$nb.larves) & piege2$nb.inflo.piege == 0), ]$nb.larves <- 0
piege2[which(is.na(piege2$nb.larves)), ]$nb.larves <- 0

piege2A <- piege2 %>% filter(date < "2017-08-17")
piege2AB <- piege2 %>% filter(date > "2017-08-17")

estimation_piege_bloc2 <- function(modalite = NULL){
    ## Renvoie un fichier .csv contenant les estimations du nombre de larves dans la parcelle, le nombre d'inflorescences vivante dans la parcelle, le nombre d'inflorescences mortes et le rapport de larves/inflo_vivantes. Les estimations sont données pour chaque date.
    ## Argument : modalité prend "bache", "enh.ras" ou "enh.haut". Par défaut NULL renvoie pour le bloc entier
    if (!is.null(modalite)){
        piege2A %<>% filter(trait==modalite)
        piege2AB %<>% filter(trait==modalite)
    }
    
    ### Initialisation data
    nb.arbre.bloc <- 150
    nb.arbre.bache <- 51
    nb.arbre.ras <- 53
    nb.arbre.haut <- 46
    larves <- rep(NA, 20)
    inflos_vivantes <- rep(NA, 20)
    inflos_mortes <- rep(NA, 20)
    larves_inflos <- rep(NA, 20)
    
    ### Avant 2017-08-17 (un seul piège)
    piege2A %<>% mutate(larves.arbre = nb.larves * nb.inflo / nb.inflo.piege)
    piege2A$larves.arbre[which(is.na(piege2A$larves.arbre) | is.infinite(piege2A$larves.arbre))] <- 0
    
    dateA <- unique(piege2A$date)
    
    for (day in 1:length(dateA)){
        aux <- piege2A %>% filter(date==dateA[day])
        larves[day] <- sum(aux$larves.arbre)
        inflos_vivantes[day] <- sum(aux$nb.inflo)
        inflos_mortes[day] <- sum(aux$nb.inflo.morte)
    }
    
    ### Après 2017-08-17 (deux pièges)
    dateAB <- unique(piege2AB$date)
    
    ## Séparation 
    piegeAB_A <- piege2AB %>% filter(piege=="A")
    piegeAB_B <- piege2AB %>% filter(piege=="B")
    
    piegeAB_A %<>% bind_cols(nb.larves_B = piegeAB_B$nb.larves, nb.inflo.piege_B = piegeAB_B$nb.inflo.piege)
    piegeAB_A %<>% mutate(nb.larvesAB = nb.larves+nb.larves_B, nb.inflo.piegeAB = nb.inflo.piege+nb.inflo.piege_B) %>% mutate(larves.arbre = nb.larvesAB * nb.inflo / nb.inflo.piegeAB)
    piegeAB_A$larves.arbre[which(is.na(piegeAB_A$larves.arbre) | is.infinite(piegeAB_A$larves.arbre))] <- 0
    
    for (day in 1:length(dateAB)){
        aux <- piegeAB_A %>% filter(date==dateAB[day])
        
        larves[day+length(dateA)] <- sum(aux$larves.arbre)
        inflos_vivantes[day+length(dateA)] <- sum(aux$nb.inflo)
        inflos_mortes[day+length(dateA)] <- sum(aux$nb.inflo.morte)
    }
    
    arbre.piege <- dim(piege2A %>% filter(date=="2017-07-18"))[1]
    if (is.null(modalite)){
        coef <- nb.arbre.bloc/arbre.piege
    } else {
        if (modalite == "bache")
            coef <- nb.arbre.bache/arbre.piege
        if (modalite == "enh.ras")
            coef <- nb.arbre.ras/arbre.piege
        if (modalite == "enh.haut")
            coef <- nb.arbre.haut/arbre.piege
    }

    larves <- coef * larves
    inflos_vivantes <- coef * inflos_vivantes
    inflos_mortes <- coef * inflos_mortes
    larves_inflos <- larves / inflos_vivantes
    
    df <- as_tibble(cbind(date = c(dateA, dateAB), larves = larves, inflos_vivantes = inflos_vivantes, inflos_mortes, larves_inflos = larves_inflos)) %>% mutate(Bloc = "Bloc 2") %>% mutate(Sol = modalite)
    df$date <- as_date(df$date)
    
    #write.csv2(df, paste0("Bloc2_",modalite,".csv"))
    return(df)
}


b1 <- estimation_piege_bloc1(modalite = "bache")
r1 <- estimation_piege_bloc1(modalite = "enh.ras")
h1 <- estimation_piege_bloc1(modalite = "enh.haut")
b2 <- estimation_piege_bloc2(modalite = "bache")
r2 <- estimation_piege_bloc2(modalite = "enh.ras")
h2 <- estimation_piege_bloc2(modalite = "enh.haut")

final <- bind_rows(b1,r1,h1,b2,r2,h2)
#write_csv2(final, "2017_piege.csv")

date <- b1$date
days <- as_date(date[1]:date[20])
laps <- as.numeric(c(1, date[2:20] - date[1:19]))

values_b1 <- as_tibble(cbind(date= days[-1],
                             larves = rep(b1$larves/laps, laps)[-1],
                             inflos_vivantes = approx(b1$date, b1$inflos_vivantes, days)$y[-1],
                             inflos_mortes = rep(b1$inflos_mortes/laps, laps)[-1] )) %>%
    mutate(Bloc = factor("Bloc 1"), Sol = factor("Bache")) %>%
    mutate_at("date", as_date)

values_r1 <- as_tibble(cbind(date= days[-1],
                             larves = rep(r1$larves/laps, laps)[-1], 
                             inflos_vivantes = approx(r1$date, r1$inflos_vivantes, days)$y[-1],
                             inflos_mortes = rep(r1$inflos_mortes/laps, laps)[-1] )) %>% 
    mutate(Bloc = factor("Bloc 1"), Sol = factor("Enherbement ras")) %>%
    mutate_at("date", as_date)

values_h1 <- as_tibble(cbind(date= days[-1],
                             larves = rep(h1$larves/laps, laps)[-1],
                             inflos_vivantes = approx(h1$date, h1$inflos_vivantes, days)$y[-1],
                             inflos_mortes = rep(h1$inflos_mortes/laps, laps)[-1] )) %>%
    mutate(Bloc = factor("Bloc 1"), Sol = factor("Enherbement haut")) %>% 
    mutate_at("date", as_date)

values_b2 <- as_tibble(cbind(date= days[-1], 
                             larves = rep(b2$larves/laps, laps)[-1],
                             inflos_vivantes = approx(b2$date, b2$inflos_vivantes, days)$y[-1],
                             inflos_mortes = rep(b2$inflos_mortes/laps, laps)[-1] )) %>%
    mutate(Bloc = factor("Bloc 2"), Sol = factor("Bache")) %>% 
    mutate_at("date", as_date)

values_r2 <- as_tibble(cbind(date= days[-1], 
                             larves = rep(r2$larves/laps, laps)[-1],
                             inflos_vivantes = approx(r2$date, r2$inflos_vivantes, days)$y[-1],
                             inflos_mortes = rep(r2$inflos_mortes/laps, laps)[-1] )) %>%
    mutate(Bloc = factor("Bloc 2"), Sol = factor("Enherbement ras")) %>%
    mutate_at("date", as_date)

values_h2 <- as_tibble(cbind(date= days[-1],
                             larves = rep(h2$larves/laps, laps)[-1],
                             inflos_vivantes = approx(h2$date, h2$inflos_vivantes, days)$y[-1],
                             inflos_mortes = rep(h2$inflos_mortes/laps, laps)[-1] )) %>%
    mutate(Bloc = factor("Bloc 2"), Sol = factor("Enherbement haut")) %>%
    mutate_at("date", as_date)

# write_csv(values_b1, "/home/bastien/Stage/Moi/b1.csv")
# write_csv(values_r1, "/home/bastien/Stage/Moi/r1.csv")
# write_csv(values_h1, "/home/bastien/Stage/Moi/h1.csv")
# write_csv(values_b2, "/home/bastien/Stage/Moi/b2.csv")
# write_csv(values_r2, "/home/bastien/Stage/Moi/r2.csv")
# write_csv(values_h2, "/home/bastien/Stage/Moi/h2.csv")
