## Script implémentant le sous-modèle 1

## Paramètres fixes
sex_ratio <- 0.5
mu_PS <- 0
proba_pupe <- 0.77
pupe_duration <- 5
eggs <- 150
mu_larvation <- 0.04
nb_jours <- 80

## Paramètres floraison
moyenne <- data.frame(mean_ER = 31.97826, mean_PS = 28.65, mean_EH = 36.25301)
stddev <- data.frame(sd_ER = 11.47071, sd_PS = 13.35809, sd_EH = 15.01044)

## Fonctions du modèle
inflo_attractive <- function(bursts, sousbloc, delta_t) {
    
    mean_hat <- moyenne %>% select(ends_with(sousbloc)) %>% pull
    sd_hat <- stddev %>% select(ends_with(sousbloc)) %>% pull
    FdR <- pnorm(1:50, mean_hat - delta_t, sd_hat)
    
    # browser()
    inflo <- rep(NA, 80)
    inflo[1] <- bursts[1]
    for (t in 2:80) {
        inflo[t] <- bursts[t] + sum(bursts[(t-1):max(1, t - 50)] *
                                        (1 - FdR[1:min(50, t-1)]))
    }
    
    inflo
}


incoming <- function(gamma, inflos) {
    ## Individus exogene
    gamma * inflos
}


exchange <- function(proba_migration, inflos) {
    ## Echange entre les sous-blocs
    alphaER <- alphaB <- alphaEH <- matrix(0, nrow = nb_jours, ncol = 3)
    alphaER[, 1] <- alphaEH[, 3] <- 1 - proba_migration
    alphaER[, 3] <- proba_migration * inflos[, 1] / (inflos[, 2] + inflos[, 1])
    alphaEH[, 1] <- proba_migration * inflos[, 3] / (inflos[, 2] + inflos[, 3])
    alphaB[, 1] <- proba_migration * inflos[, 2] / (inflos[, 2] + inflos[, 3])
    alphaB[, 3] <- proba_migration * inflos[, 2] / (inflos[, 2] + inflos[, 1])
    
    return(list(alphaER, alphaB, alphaEH))
}

## Nouveau «p_m»
exchange2 <- function(leaving_to_ps, inflos) {
    ## Echange entre les sous-blocs
    alphaER <- alphaPS <- alphaEH <- matrix(0, nrow = nb_jours, ncol = 3)
    
    alphaPS[, 1] <- leaving_to_ps * inflos[, 2] / (inflos[, 1] + inflos[, 2])
    alphaPS[, 3] <- leaving_to_ps * inflos[, 2] / (inflos[, 3] + inflos[, 2])
    
    alphaER[, 1] <- inflos[, 1] / (inflos[, 1] + inflos[, 2]) +
        (1 - leaving_to_ps) * inflos[, 2] / (inflos[, 1] + inflos[, 2])
    
    alphaEH[, 3] <- inflos[, 3] / (inflos[, 3] + inflos[, 2]) +
        (1 - leaving_to_ps) * inflos[, 2] / (inflos[, 3] + inflos[, 2])
    
    return(list(alphaER, alphaPS, alphaEH))
}


disponibility <- function(day, inflo_capacity, inflos, females) {
    ## Disponibilité des ressources
    ans <- rep(NA, 3)
    ans[females[day, ] <= inflo_capacity * inflos[day, ]] <- 1
    ans[females[day, ] > inflo_capacity * inflos[day, ]] <- inflo_capacity *
        inflos[day, which(females[day, ] > inflo_capacity * inflos[day, ])] /
        females[day, which(females[day, ] > inflo_capacity * inflos[day, ])]
    
    ans
}


larvaes_count <- function(day, inflo_capacity, inflos, females) {
    ## Nombre de larves chaque jour
    beta7 <-  0.025
    beta8 <-  0.075
    beta9 <-  0.400
    beta10 <- 0.400
    beta11 <- 0.075
    beta12 <- 0.025
    
    larvae7 <- larvae8 <- larvae9 <- larvae10 <- larvae11 <- larvae12 <- 0
    if (day > 7) {
        R <- disponibility(day - 7, inflo_capacity, inflos, females)
        larvae7 <- females[day - 7, ] * R
    }
    
    if (day > 8) {
        R <- disponibility(day - 8, inflo_capacity, inflos, females)
        larvae8 <- females[day - 8, ] * R
    }
    
    if (day > 9) {
        R <- disponibility(day - 9, inflo_capacity, inflos, females)
        larvae9 <- females[day - 9, ] * R
    }
    
    if (day > 10) {
        R <- disponibility(day - 10, inflo_capacity, inflos, females)
        larvae10 <- females[day - 10, ] * R
    }
    
    if (day > 11) {
        R <- disponibility(day - 11, inflo_capacity, inflos, females)
        larvae11 <- females[day - 11, ] * R
    }
    
    if (day > 12) {
        R <- disponibility(day - 12, inflo_capacity, inflos, females)
        larvae12 <- females[day - 12, ] * R
    }
    
    
    beta7 * larvae7 + beta8 * larvae8 + beta9 * larvae9 +
        beta10 * larvae10 + beta11 * larvae11 + beta12 * larvae12
}


emerging <- function(day, larves, mu_sol, mu_global) {
    ## Femelles endogenes
    if (day <= 5)
        c(0, 0, 0)
    else
        larves[day - pupe_duration, ] * mu_sol * mu_global
}

females_count <- function(day, alpha, females_exo, females_endo) {
    ## Nombre total de femelles
    females_exo[day] + alpha[day, ] %*% females_endo[day, ]
}

dynamics <- function(arg, inflos) {
    ## Calcule le nombre de larves (inch'allah)
    gamma <- arg[1]
    proba_migration <- arg[2]
    mu_ER <- arg[3]
    mu_EH <- arg[4]
    inflo_capacity <- arg[5]
    
    
    alpha <- exchange(proba_migration, inflos)
    females_exo <- incoming(gamma, inflos)
    # females_exo <- matrix(20, nrow = 80, ncol = 3)
    larves <- matrix(0, nrow = nb_jours, ncol = 3)
    females_endo <- matrix(0, nrow = nb_jours, ncol = 3)
    females <- matrix(0, nrow = nb_jours, ncol = 3)
    mu_sol <- c(mu_ER, mu_PS, mu_EH)
    for (jour in 1:nb_jours) {
        larves[jour, ] <- larvaes_count(jour, inflo_capacity, inflos, females)
        females_endo[jour, ] <- emerging(jour, larves, mu_sol)
        females[jour, 1] <- females_count(jour, alpha[[1]], 
                                          females_exo[, 1], females_endo)
        females[jour, 2] <- females_count(jour, alpha[[2]],
                                          females_exo[, 2], females_endo)
        females[jour, 3] <- females_count(jour, alpha[[3]],
                                          females_exo[, 3], females_endo)
    }
    
    larves
}

dynamics2 <- function(arg, inflos) {
    ## Calcule le nombre de larves (inch'allah)
    gamma <- arg[1]
    leaving <- arg[2]
    mu_ER <- arg[3]
    mu_EH <- arg[4]
    inflo_capacity <- arg[5]
    mu_global <- arg[6]
    
    alpha <- exchange2(leaving, inflos)
    females_exo <- incoming(gamma, inflos)
    # females_exo <- matrix(20, nrow = 80, ncol = 3)
    larves <- matrix(0, nrow = nb_jours, ncol = 3)
    females_endo <- matrix(0, nrow = nb_jours, ncol = 3)
    females <- matrix(0, nrow = nb_jours, ncol = 3)
    mu_sol <- c(mu_ER, mu_PS, mu_EH)
    for (jour in 1:nb_jours) {
        larves[jour, ] <- larvaes_count(jour, inflo_capacity, inflos, females)
        females_endo[jour, ] <- emerging(jour, larves, mu_sol, mu_global)
        females[jour, 1] <- females_count(jour, alpha[[1]],
                                          females_exo[, 1], females_endo)
        females[jour, 2] <- females_count(jour, alpha[[2]],
                                          females_exo[, 2], females_endo)
        females[jour, 3] <- females_count(jour, alpha[[3]],
                                          females_exo[, 3], females_endo)
    }
    
    larves
}

females_count3 <- function(day, alpha, females_exo, females_endo, saison_deb, saison_end) {
    ## Nombre total de femelles
    if (day <= 59)
        return((females_exo[day] + alpha[day, ] %*% females_endo[day, ]) * saison_deb)
    if (day > 59)
        return((females_exo[day] + alpha[day, ] %*% females_endo[day, ]) * saison_end)
}


decomposition <- function(arg, inflos) {
    ## Calcule le nombre de larves (inch'allah)
    gamma <- arg[1]
    proba_migration <- arg[2]
    mu_ER <- arg[3]
    mu_EH <- arg[4]
    inflo_capacity <- arg[5]
    
    alpha <- exchange(proba_migration, inflos)
    females_exo <- incoming(gamma, inflos)
    # females_exo <- matrix(20, nrow = 80, ncol = 3)
    larves <- matrix(0, nrow = nb_jours, ncol = 3)
    females_endo <- matrix(0, nrow = nb_jours, ncol = 3)
    females <- matrix(0, nrow = nb_jours, ncol = 3)
    mu_sol <- c(mu_ER, mu_PS, mu_EH)
    prop_endo <- matrix(NA, nrow = nb_jours, ncol = 3)
    prop_exo <- matrix(NA, nrow = nb_jours, ncol = 3)
    prop_side <- matrix(NA, nrow = nb_jours, ncol = 3)
    larves_endo <- matrix(NA, nrow = nb_jours, ncol = 3)
    larves_exo <- matrix(NA, nrow = nb_jours, ncol = 3)
    larves_side <- matrix(NA, nrow = nb_jours, ncol = 3)
    female_side <- matrix(NA, nrow = nb_jours, ncol = 3)
    for (jour in 1:nb_jours) {
        larves[jour, ] <- larvaes_count(jour, inflo_capacity, inflos, females)
        females_endo[jour, ] <- emerging(jour, larves, mu_sol)
        
        larves_exo[jour, ] <- larvaes_count2(jour, inflo_capacity, inflos, females_exo)
        larves_endo[jour, ] <- larvaes_count2(jour, inflo_capacity, inflos,
                                              females_endo * (1 - proba_migration))
        
        female_side[jour, 1] <- females_side(jour, alpha[[1]], cbind(0, females_endo[, 2:3]))
        female_side[jour, 2] <- females_side(jour, alpha[[2]], 
                                             cbind(females_endo[, 1], 0, females_endo[, 3]))
        female_side[jour, 3] <- females_side(jour, alpha[[3]], cbind(females_endo[, 1:2], 0))
        
        larves_side[jour, ] <- larvaes_count2(jour, inflo_capacity, inflos, female_side)
        
        females[jour, 1] <- females_count(jour, alpha[[1]], females_exo[, 1], females_endo)
        females[jour, 2] <- females_count(jour, alpha[[2]], females_exo[, 2], females_endo)
        females[jour, 3] <- females_count(jour, alpha[[3]], females_exo[, 3], females_endo)
    }
    prop_endo <- larves_endo / (larves_endo + larves_side + larves_exo)
    prop_side <- larves_side / (larves_endo + larves_side + larves_exo)
    prop_exo <- larves_exo / (larves_endo + larves_side + larves_exo)
    
    list(larves, prop_exo * larves, prop_endo * larves, prop_side * larves)
}


larvaes_count2 <- function(day, inflo_capacity, inflos, females) {
    ## Nombre de larves chaque jour
    beta7 <-  1
    beta8 <-  0
    beta9 <-  0
    beta10 <- 0
    beta11 <- 0
    beta12 <- 0
    
    larvae7 <- larvae8 <- larvae9 <- larvae10 <- larvae11 <- larvae12 <- 0
    if (day > 7) {
        larvae7 <- females[day - 7, ] * eggs * mu_larvation
    }
    
    if (day > 8) {
        larvae8 <- females[day - 8, ] * eggs * mu_larvation
    }
    
    if (day > 9) {
        larvae9 <- females[day - 9, ] * eggs * mu_larvation
    }
    
    if (day > 10) {
        larvae10 <- females[day - 10, ] * eggs * mu_larvation
    }
    
    if (day > 11) {
        larvae11 <- females[day - 11, ] * eggs * mu_larvation
    }
    
    if (day > 12) {
        larvae12 <- females[day - 12, ] * eggs * mu_larvation
    }
    
    
    beta7 * larvae7 + beta8 * larvae8 + beta9 * larvae9 +
        beta10 * larvae10 + beta11 * larvae11 + beta12 * larvae12
}

females_side <- function(day, alpha, females_endo) {
    ## Nombre total de femelles
    alpha[day, ] %*% females_endo[day, ]
}






decomposition2 <- function(arg, inflos) {
    ## Calcule le nombre de larves (inch'allah)
    gamma <- arg[1]
    leaving <- arg[2]
    mu_ER <- arg[3]
    mu_EH <- arg[4]
    inflo_capacity <- arg[5]
    
    alpha <- exchange2(leaving, inflos)
    females_exo <- incoming(gamma, inflos)
    # females_exo <- matrix(20, nrow = 80, ncol = 3)
    larves <- matrix(0, nrow = nb_jours, ncol = 3)
    females_endo <- matrix(0, nrow = nb_jours, ncol = 3)
    females <- matrix(0, nrow = nb_jours, ncol = 3)
    mu_sol <- c(mu_ER, mu_PS, mu_EH)
    prop_endo <- matrix(NA, nrow = nb_jours, ncol = 3)
    prop_exo <- matrix(NA, nrow = nb_jours, ncol = 3)
    prop_side <- matrix(NA, nrow = nb_jours, ncol = 3)
    larves_endo <- matrix(NA, nrow = nb_jours, ncol = 3)
    larves_exo <- matrix(NA, nrow = nb_jours, ncol = 3)
    larves_side <- matrix(NA, nrow = nb_jours, ncol = 3)
    female_side <- matrix(NA, nrow = nb_jours, ncol = 3)
    female_endo <- matrix(NA, nrow = nb_jours, ncol = 3)
    for (jour in 1:nb_jours) {
        larves[jour, ] <- larvaes_count(jour, inflo_capacity, inflos, females)
        females_endo[jour, ] <- emerging(jour, larves, mu_sol)
        
        female_endo[jour, 1] <- females_side(jour, alpha[[1]], cbind(females_endo[, 1], 0, 0))
        female_endo[jour, 2] <- females_side(jour, alpha[[2]], matrix(0, nrow = nb_jours, ncol = 3))
        female_endo[jour, 3] <- females_side(jour, alpha[[3]], cbind(0, 0, females_endo[, 3]))
        
        female_side[jour, 1] <- females_side(jour, alpha[[1]], cbind(0, females_endo[, 2:3]))
        female_side[jour, 2] <- females_side(jour, alpha[[2]], 
                                             cbind(females_endo[, 1], 0, females_endo[, 3]))
        female_side[jour, 3] <- females_side(jour, alpha[[3]], cbind(females_endo[, 1:2], 0))
        
        larves_side[jour, ] <- larvaes_count2(jour, inflo_capacity, inflos, female_side)
        larves_exo[jour, ] <- larvaes_count2(jour, inflo_capacity, inflos, females_exo)
        larves_endo[jour, ] <- larvaes_count2(jour, inflo_capacity, inflos, female_endo)
        
        females[jour, 1] <- females_count(jour, alpha[[1]], females_exo[, 1], females_endo)
        females[jour, 2] <- females_count(jour, alpha[[2]], females_exo[, 2], females_endo)
        females[jour, 3] <- females_count(jour, alpha[[3]], females_exo[, 3], females_endo)
    }
    prop_endo <- larves_endo / (larves_endo + larves_side + larves_exo)
    prop_side <- larves_side / (larves_endo + larves_side + larves_exo)
    prop_exo <- larves_exo / (larves_endo + larves_side + larves_exo)
    
    list(larves, prop_exo * larves, prop_endo * larves, prop_side * larves)
}
