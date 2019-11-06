## Script implémentant le sous-modèle 1

# Paramètres fixes --------------------------------------------------------

sex_ratio <- 0.5
mu_PS <- 0
pupe_duration <- 5
eggs <- 150
mu_larvation <- 0.04
nb_jours <- 80
load("/home/bastien/cecidomyie/data/p_pup.Rdata")
load("/home/bastien/cecidomyie/data/p_pup15j.Rdata")
load("/home/bastien/cecidomyie/data/sortie_diapause2017.Rdata")
bursts <- as.matrix(read.csv("/home/bastien/cecidomyie/data/2017_bursts_simulated.csv", row.names = 1))
mu <- 29
sigma <- 14
CDF_norm <- pnorm(1:50, mu, sigma)

# Fonctions ---------------------------------------------------------------

incoming <- function(gamma, inflos) {
    ## Individus exogene
    ## Arrivent proportionnellement aux inflos
    gamma * inflos
}

exchange <- function(param_migration, inflos) {
    ## Échange entre les sous-blocs du bloc 1 [ER|PS|EH]
    ## Les matrices alpha représente les individus VENANT dans le sous-bloc
    ## Par exemple, alphaER[, 3] désigne les individus allant de EH dans ER
    alphaER <- alphaPS <- alphaEH <- matrix(0, nrow = nb_jours, ncol = 3)
    
    alphaER[, 1] <- inflos[, 1] / (inflos[, 1] + inflos[, 2] * param_migration +
                                       inflos[, 3] * param_migration^2 + 1)
    alphaPS[, 2] <- inflos[, 2] / (inflos[, 2] + inflos[, 1] * param_migration +
                                       inflos[, 3] * param_migration + 1)
    alphaEH[, 3] <- inflos[, 3] / (inflos[, 3] + inflos[, 2] * param_migration +
                                       inflos[, 1] * param_migration^2 + 1)
    
    alphaER[, 2] <- param_migration * inflos[, 1] / 
        (inflos[, 2] + inflos[, 1] * param_migration + inflos[, 3] * param_migration + 1)
    alphaEH[, 2] <- param_migration * inflos[, 3] / 
        (inflos[, 2] + inflos[, 1] * param_migration + inflos[, 3] * param_migration + 1)
    alphaPS[, 1] <- param_migration * inflos[, 2] /
        (inflos[, 1] + inflos[, 2] * param_migration + inflos[, 3] * param_migration^2 + 1)
    alphaPS[, 3] <- param_migration * inflos[, 2] /
        (inflos[, 3] + inflos[, 2] * param_migration + inflos[, 1] * param_migration^2 + 1)
    
    alphaER[, 3] <- param_migration^2 * inflos[, 1] /
        (inflos[, 3] + inflos[, 2] * param_migration + inflos[, 1] * param_migration^2 + 1)
    alphaEH[, 1] <- param_migration^2 * inflos[, 3] /
        (inflos[, 1] + inflos[, 2] * param_migration + inflos[, 3] * param_migration^2 + 1)
    
    
    list(alphaER, alphaPS, alphaEH)
}

exchange_b2 <- function(param_migration, inflos) {
    ## Échange entre les sous-blocs du bloc 2 [ER|EH|PS]
    ## Les matrices alpha représente les individus VENANT dans le sous-bloc
    ## Par exemple, alphaER[, 3] désigne les individus allant de EH dans ER
    alphaER <- alphaPS <- alphaEH <- matrix(0, nrow = nb_jours, ncol = 3)
    
    alphaER[, 1] <- inflos[, 1] / (inflos[, 1] + inflos[, 3] * param_migration +
                                       inflos[, 2] * param_migration^2 + 1)
    alphaPS[, 2] <- inflos[, 2] / (inflos[, 2] + inflos[, 1] * param_migration^2 +
                                       inflos[, 3] * param_migration + 1)
    alphaEH[, 3] <- inflos[, 3] / (inflos[, 3] + inflos[, 2] * param_migration +
                                       inflos[, 1] * param_migration + 1)
    
    alphaER[, 2] <- param_migration^2 * inflos[, 1] / 
        (inflos[, 2] + inflos[, 1] * param_migration^2 + inflos[, 3] * param_migration + 1)
    alphaEH[, 2] <- param_migration * inflos[, 3] / 
        (inflos[, 2] + inflos[, 1] * param_migration^2 + inflos[, 3] * param_migration + 1)
    alphaPS[, 1] <- param_migration^2 * inflos[, 2] /
        (inflos[, 1] + inflos[, 2] * param_migration^2 + inflos[, 3] * param_migration + 1)
    alphaPS[, 3] <- param_migration * inflos[, 2] /
        (inflos[, 3] + inflos[, 2] * param_migration + inflos[, 1] * param_migration + 1)
    
    alphaER[, 3] <- param_migration * inflos[, 1] /
        (inflos[, 3] + inflos[, 2] * param_migration + inflos[, 1] * param_migration + 1)
    alphaEH[, 1] <- param_migration * inflos[, 3] /
        (inflos[, 1] + inflos[, 2] * param_migration^2 + inflos[, 3] * param_migration + 1)
    
    
    list(alphaER, alphaPS, alphaEH)
}

disponibility <- function(day, inflo_capacity, inflos, females) {
    ## Disponibilité des ressources
    ## Coefficient R dans le modèle
    ## max(1, k * I/F) pour chacun des trois sous-blocs
    ans <- rep(NA, 3)
    ans[females[day, ] <= inflo_capacity * inflos[day, ]] <- 1
    ans[females[day, ] > inflo_capacity * inflos[day, ]] <- inflo_capacity *
        inflos[day, which(females[day, ] > inflo_capacity * inflos[day, ])] /
        females[day, which(females[day, ] > inflo_capacity * inflos[day, ])]
    
    ans
}

larvaes_count <- function(day, inflo_capacity, inflos, females, mu_sol, reproduction) {
    ## Nombre de larves qui s'éjectent des inflorescences
    ## L_t = F_{t-dl} * R * E_0 * mu_ell
    beta7 <-  0.025
    beta8 <-  0.075
    beta9 <-  0.400
    beta10 <- 0.400
    beta11 <- 0.075
    beta12 <- 0.025
    
    larvae7 <- larvae8 <- larvae9 <- larvae10 <- larvae11 <- larvae12 <- 0
    if (day > 7) {
        R <- disponibility(day - 7, inflo_capacity, inflos, females)
        larvae7 <- females[day - 7, ] * R * reproduction
    }
    
    if (day > 8) {
        R <- disponibility(day - 8, inflo_capacity, inflos, females)
        larvae8 <- females[day - 8, ] * R * reproduction
    }
    
    if (day > 9) {
        R <- disponibility(day - 9, inflo_capacity, inflos, females)
        larvae9 <- females[day - 9, ] * R * reproduction
    }
    
    if (day > 10) {
        R <- disponibility(day - 10, inflo_capacity, inflos, females)
        larvae10 <- females[day - 10, ] * R * reproduction
    }
    
    if (day > 11) {
        R <- disponibility(day - 11, inflo_capacity, inflos, females)
        larvae11 <- females[day - 11, ] * R * reproduction
    }
    
    if (day > 12) {
        R <- disponibility(day - 12, inflo_capacity, inflos, females)
        larvae12 <- females[day - 12, ] * R * reproduction
    }
    
    
    beta7 * larvae7 + beta8 * larvae8 + beta9 * larvae9 +
        beta10 * larvae10 + beta11 * larvae11 + beta12 * larvae12
}

larvaes_count_E <- function(day, inflo_capacity, inflos, females, mu_sol, reproduction, duree_dvpmt) {
    ## Nombre de larves qui s'éjectent des inflorescences
    ## L_t = F_{t-dl} * R * E_0 * mu_ell
    ## duree_dvpmt : début de la durée de développement après la ponte
    ## i.e duree_dvmpt = 3 signifie que les larves sortent entre 3 et 8 jours après la ponte
    duree_dvpmt <- round(duree_dvpmt)
    beta7 <-  0.025
    beta8 <-  0.075
    beta9 <-  0.400
    beta10 <- 0.400
    beta11 <- 0.075
    beta12 <- 0.025
    
    larvae7 <- larvae8 <- larvae9 <- larvae10 <- larvae11 <- larvae12 <- 0
    if (day > duree_dvpmt & day - duree_dvpmt <= 80) {
        R <- disponibility(day - duree_dvpmt, inflo_capacity, inflos, females)
        larvae7 <- females[day - duree_dvpmt, ] * R * reproduction
    }
    
    if (day > duree_dvpmt + 1 & day - duree_dvpmt + 1 <= 80) {
        R <- disponibility(day - duree_dvpmt + 1, inflo_capacity, inflos, females)
        larvae8 <- females[day - duree_dvpmt, ] * R * reproduction
    }
    
    if (day > duree_dvpmt + 2 & day - duree_dvpmt + 2 <= 80) {
        R <- disponibility(day - duree_dvpmt + 2, inflo_capacity, inflos, females)
        larvae9 <- females[day - duree_dvpmt, ] * R * reproduction
    }
    
    if (day > duree_dvpmt + 3 & day - duree_dvpmt + 3 <= 80) {
        R <- disponibility(day - duree_dvpmt + 3, inflo_capacity, inflos, females)
        larvae10 <- females[day - duree_dvpmt, ] * R * reproduction
    }
    
    if (day > duree_dvpmt + 4 & day - duree_dvpmt + 4 <= 80) {
        R <- disponibility(day - duree_dvpmt + 4, inflo_capacity, inflos, females)
        larvae11 <- females[day - duree_dvpmt, ] * R * reproduction
    }
    
    if (day > duree_dvpmt + 5 & day - duree_dvpmt + 5 <= 80) {
        R <- disponibility(day - duree_dvpmt + 5, inflo_capacity, inflos, females)
        larvae12 <- females[day - duree_dvpmt, ] * R * reproduction
    }
    
    
    beta7 * larvae7 + beta8 * larvae8 + beta9 * larvae9 +
        beta10 * larvae10 + beta11 * larvae11 + beta12 * larvae12
}


emerging_A <- function(day, larves, mu_sol, stock) {
    ## Renvoie le nombre de femelles qui émergent pour un jour donné.
    ## Individus en pupaison et en diapause confondus
    ## Modèle A
    if (day <= 5)
        c(0, 0, 0)
    else
        ((larves[day - pupe_duration, ] * mu_sol) * 0.77 +
             sortie_diapause2017[day] * stock) * mu_sol * sex_ratio
}

emerging_B <- function(day, larves, mu_sol, stock, coef) {
    ## Renvoie le nombre de femelles qui émergent pour un jour donné.
    ## Individus en pupaison et en diapause confondus
    ## Modèle B
    if (day <= 5)
        c(0, 0, 0)
    else
        ((larves[day - pupe_duration, ] * mu_sol) *
             ((p_pup15j - mean(p_pup15j)) * coef + mean(p_pup15j))[day] +
             sortie_diapause2017[day] * stock) * mu_sol * sex_ratio
}

emerging_B2 <- function(day, larves, mu_sol, stock, coef, intercept) {
    ## Renvoie le nombre de femelles qui émergent pour un jour donné.
    ## Individus en pupaison et en diapause confondus
    ## Modèle B2
    if (day <= 5)
        c(0, 0, 0)
    else
        ((larves[day - pupe_duration, ] * mu_sol) *
             max(min(((p_pup15j - mean(p_pup15j)) * coef + intercept)[day], 1), 0)  +
             sortie_diapause2017[day] * stock) *
        mu_sol * sex_ratio
}

emerging <- function(day, larves, mu_sol, stock) {
    ## Renvoie le nombre de femelles qui émergent pour un jour donné.
    ## Individus en pupaison et en diapause confondus
    ## Les individus en diapause ne sont confronté qu'une seule fois aux paramètres mu_sol
    ## Modèle C et D
    if (day <= 5)
        c(0, 0, 0)
    else
        ((larves[day - pupe_duration, ] * mu_sol) * p_pup15j[day - pupe_duration] +
             sortie_diapause2017[day] * stock) * mu_sol * sex_ratio
}

females_count <- function(day, alpha, females_exo, females_endo) {
    ## Nombre total de femelles chaque jour pour chaque sous-bloc
    ## F^{exo} + F^{endo}
    females_exo[day] + alpha[day, ] %*% females_endo[day, ]
}


# Dynamiques --------------------------------------------------------------

## Ces fonctions ne sont valables que pour le bloc 1
## Car les sous-parcelles ne sont pas disposées dans le même ordre

dynamics_A <- function(arg, inflos) {
    ## Renvoie la dynamique de larves pour les trois sous-blocs
    ## Modèle A
    gamma <- arg[1]
    migration <- arg[2]
    mu_ER <- arg[3]
    mu_EH <- arg[4]
    inflo_capacity <- arg[5]
    stock <- arg[6]
    reproduction <- arg[7]
    
    alpha <- exchange(migration, inflos)
    females_exo <- incoming(gamma, inflos)
    # females_exo <- matrix(30, nrow = 80, ncol = 3)
    larves <- matrix(0, nrow = nb_jours, ncol = 3)
    females_endo <- matrix(0, nrow = nb_jours, ncol = 3)
    females <- matrix(0, nrow = nb_jours, ncol = 3)
    mu_sol <- c(mu_ER, mu_PS, mu_EH)
    for (jour in 1:nb_jours) {
        larves[jour, ] <- larvaes_count(jour, inflo_capacity, inflos, females,
                                        mu_sol, reproduction)
        females_endo[jour, ] <- emerging_A(jour, larves, mu_sol, stock)
        females[jour, 1] <- females_count(jour, alpha[[1]],
                                          females_exo[, 1], females_endo)
        females[jour, 2] <- females_count(jour, alpha[[2]],
                                          females_exo[, 2], females_endo)
        females[jour, 3] <- females_count(jour, alpha[[3]],
                                          females_exo[, 3], females_endo)
    }
    
    larves
}

dynamics_B <- function(arg, inflos) {
    ## Renvoie la dynamique de larves pour les trois sous-blocs
    ## Modèle B
    gamma <- arg[1]
    migration <- arg[2]
    mu_ER <- arg[3]
    mu_EH <- arg[4]
    inflo_capacity <- arg[5]
    stock <- arg[6]
    reproduction <- arg[7]
    coef <- arg[8]
    
    alpha <- exchange(migration, inflos)
    females_exo <- incoming(gamma, inflos)
    # females_exo <- matrix(30, nrow = 80, ncol = 3)
    larves <- matrix(0, nrow = nb_jours, ncol = 3)
    females_endo <- matrix(0, nrow = nb_jours, ncol = 3)
    females <- matrix(0, nrow = nb_jours, ncol = 3)
    mu_sol <- c(mu_ER, mu_PS, mu_EH)
    for (jour in 1:nb_jours) {
        larves[jour, ] <- larvaes_count(jour, inflo_capacity, inflos, females,
                                        mu_sol, reproduction)
        females_endo[jour, ] <- emerging_B(jour, larves, mu_sol, stock, coef)
        females[jour, 1] <- females_count(jour, alpha[[1]],
                                          females_exo[, 1], females_endo)
        females[jour, 2] <- females_count(jour, alpha[[2]],
                                          females_exo[, 2], females_endo)
        females[jour, 3] <- females_count(jour, alpha[[3]],
                                          females_exo[, 3], females_endo)
    }
    
    larves
}

dynamics_B2 <- function(arg, inflos) {
    ## Renvoie la dynamique de larves pour les trois sous-blocs
    ## Modèle B2
    gamma <- arg[1]
    migration <- arg[2]
    mu_ER <- arg[3]
    mu_EH <- arg[4]
    inflo_capacity <- arg[5]
    stock <- arg[6]
    reproduction <- arg[7]
    coef <- arg[8]
    intercept <- arg[9]
    
    alpha <- exchange(migration, inflos)
    females_exo <- incoming(gamma, inflos)
    # females_exo <- matrix(30, nrow = 80, ncol = 3)
    larves <- matrix(0, nrow = nb_jours, ncol = 3)
    females_endo <- matrix(0, nrow = nb_jours, ncol = 3)
    females <- matrix(0, nrow = nb_jours, ncol = 3)
    mu_sol <- c(mu_ER, mu_PS, mu_EH)
    for (jour in 1:nb_jours) {
        larves[jour, ] <- larvaes_count(jour, inflo_capacity, inflos, females,
                                        mu_sol, reproduction)
        females_endo[jour, ] <- emerging_B2(jour, larves, mu_sol, stock, coef, intercept)
        females[jour, 1] <- females_count(jour, alpha[[1]],
                                          females_exo[, 1], females_endo)
        females[jour, 2] <- females_count(jour, alpha[[2]],
                                          females_exo[, 2], females_endo)
        females[jour, 3] <- females_count(jour, alpha[[3]],
                                          females_exo[, 3], females_endo)
    }
    
    larves
}


dynamics <- function(arg, inflos) {
    ## Renvoie la dynamique de larves pour les trois sous-blocs
    ## Modèle C
    gamma <- arg[1]
    migration <- arg[2]
    mu_ER <- arg[3]
    mu_EH <- arg[4]
    inflo_capacity <- arg[5]
    stock <- arg[6]
    reproduction <- arg[7]
    
    alpha <- exchange(migration, inflos)
    females_exo <- incoming(gamma, inflos)
    # females_exo <- matrix(30, nrow = 80, ncol = 3)
    larves <- matrix(0, nrow = nb_jours, ncol = 3)
    females_endo <- matrix(0, nrow = nb_jours, ncol = 3)
    females <- matrix(0, nrow = nb_jours, ncol = 3)
    mu_sol <- c(mu_ER, mu_PS, mu_EH)
    for (jour in 1:nb_jours) {
        larves[jour, ] <- larvaes_count(jour, inflo_capacity, inflos, females,
                                        mu_sol, reproduction)
        females_endo[jour, ] <- emerging(jour, larves, mu_sol, stock)
        females[jour, 1] <- females_count(jour, alpha[[1]],
                                          females_exo[, 1], females_endo)
        females[jour, 2] <- females_count(jour, alpha[[2]],
                                          females_exo[, 2], females_endo)
        females[jour, 3] <- females_count(jour, alpha[[3]],
                                          females_exo[, 3], females_endo)
    }
    
    larves
}


inflos_attractives <- function(burst, dA = 16) {
    ## Permet de simuler des inflos attractives
    ## dA permet de choisir la durée voulue
    inflos <- matrix(NA, nrow = nrow(burst), ncol = 3)
    inflos[1, ] <- burst[1, ]
    days <- 1:nrow(burst) 
    for (t in days[-1]) {
        tempER <- 0
        tempPS <- 0
        tempEH <- 0
        for (j in 1:min(dA, t-1)) {
            tempER <- tempER + burst[t-j, 1] * (1 - CDF_norm[j]) 
            tempPS <- tempPS + burst[t-j, 2] * (1 - CDF_norm[j]) 
            tempEH <- tempEH + burst[t-j, 3] * (1 - CDF_norm[j]) 
        } 
        inflos[t, ] <- burst[t, ] + c(tempER, tempPS, tempEH)
    }
    inflos
}

dynamics_C <- function(arg, inflos) {
    ## Renvoie la dynamique de larves pour les trois sous-blocs
    ## Modèle C où l'on peut calibrer la durée d'attractivité
    gamma <- arg[1]
    migration <- arg[2]
    mu_ER <- arg[3]
    mu_EH <- arg[4]
    inflo_capacity <- arg[5]
    stock <- arg[6]
    reproduction <- arg[7]
    duree_attra <- arg[8]
    
    inflos <- inflos_attractives(bursts, duree_attra) + 1
    alpha <- exchange(migration, inflos)
    females_exo <- incoming(gamma, inflos)
    # females_exo <- matrix(30, nrow = 80, ncol = 3)
    larves <- matrix(0, nrow = nb_jours, ncol = 3)
    females_endo <- matrix(0, nrow = nb_jours, ncol = 3)
    females <- matrix(0, nrow = nb_jours, ncol = 3)
    mu_sol <- c(mu_ER, mu_PS, mu_EH)
    for (jour in 1:nb_jours) {
        larves[jour, ] <- larvaes_count(jour, inflo_capacity, inflos, females,
                                        mu_sol, reproduction)
        females_endo[jour, ] <- emerging(jour, larves, mu_sol, stock)
        females[jour, 1] <- females_count(jour, alpha[[1]],
                                          females_exo[, 1], females_endo)
        females[jour, 2] <- females_count(jour, alpha[[2]],
                                          females_exo[, 2], females_endo)
        females[jour, 3] <- females_count(jour, alpha[[3]],
                                          females_exo[, 3], females_endo)
    }
    
    larves
}

dynamics_season_b1 <- function(arg, inflos) {
    ## Renvoie la dynamique de larves pour les trois sous-blocs
    ## Modèle D
    gamma <- arg[1]
    migration <- arg[2]
    mu_ER <- arg[3]
    mu_EH <- arg[4]
    inflo_capacity <- arg[5]
    stock <- arg[6]
    # stock <- 0
    reproduction <- arg[7]
    season <- arg[8]
    
    alpha <- exchange(migration, inflos)
    females_exo <- incoming(gamma, inflos)
    # females_exo <- matrix(30, nrow = 80, ncol = 3)
    larves <- matrix(0, nrow = nb_jours, ncol = 3)
    females_endo <- matrix(0, nrow = nb_jours, ncol = 3)
    females <- matrix(0, nrow = nb_jours, ncol = 3)
    mu_sol <- c(mu_ER, mu_PS, mu_EH)
    for (jour in 1:nb_jours) {
        if (jour < 59) {
            larves[jour, ] <- larvaes_count(jour, inflo_capacity, inflos, females,
                                            mu_sol, reproduction)
            females_endo[jour, ] <- emerging(jour, larves, mu_sol, stock)
            females[jour, 1] <- females_count(jour, alpha[[1]],
                                              females_exo[, 1], females_endo)
            females[jour, 2] <- females_count(jour, alpha[[2]],
                                              females_exo[, 2], females_endo)
            females[jour, 3] <- females_count(jour, alpha[[3]],
                                              females_exo[, 3], females_endo)
        } else {
            larves[jour, ] <- larvaes_count(jour, inflo_capacity, inflos, females,
                                            mu_sol, reproduction)
            females_endo[jour, ] <- emerging(jour, larves, mu_sol, stock)
            females[jour, 1] <- females_count(jour, alpha[[1]],
                                              females_exo[, 1], females_endo)
            females[jour, 2] <- females_count(jour, alpha[[2]],
                                              females_exo[, 2], females_endo)
            females[jour, 3] <- females_count(jour, alpha[[3]],
                                              females_exo[, 3], females_endo)
            females[jour, ] <- females[jour, ] * season
        }
    }
    
    larves
}

dynamics_E <- function(arg, inflos) {
    ## Renvoie la dynamique de larves pour les trois sous-blocs
    ## Modèle E
    gamma <- arg[1]
    migration <- arg[2]
    mu_ER <- arg[3]
    mu_EH <- arg[4]
    inflo_capacity <- arg[5]
    stock <- arg[6]
    reproduction <- arg[7]
    duree_dvpmt <- arg[8]
    
    alpha <- exchange(migration, inflos)
    females_exo <- incoming(gamma, inflos)
    # females_exo <- matrix(30, nrow = 80, ncol = 3)
    larves <- matrix(0, nrow = nb_jours, ncol = 3)
    females_endo <- matrix(0, nrow = nb_jours, ncol = 3)
    females <- matrix(0, nrow = nb_jours, ncol = 3)
    mu_sol <- c(mu_ER, mu_PS, mu_EH)
    for (jour in 1:nb_jours) {
        larves[jour, ] <- larvaes_count_E(jour, inflo_capacity, inflos, females,
                                        mu_sol, reproduction, duree_dvpmt)
        females_endo[jour, ] <- emerging(jour, larves, mu_sol, stock)
        females[jour, 1] <- females_count(jour, alpha[[1]],
                                          females_exo[, 1], females_endo)
        females[jour, 2] <- females_count(jour, alpha[[2]],
                                          females_exo[, 2], females_endo)
        females[jour, 3] <- females_count(jour, alpha[[3]],
                                          females_exo[, 3], females_endo)
    }
    
    larves
}

# Dynamiques bloc 2 -------------------------------------------------------


dynamics_b2 <- function(arg, inflos) {
    ## Renvoie la dynamique de larves pour les trois sous-blocs
    ## Modèle C (sans ajustement inflos)
    gamma <- arg[1]
    migration <- arg[2]
    mu_ER <- arg[3]
    mu_EH <- arg[4]
    inflo_capacity <- arg[5]
    stock <- arg[6]
    reproduction <- arg[7]
    
    alpha <- exchange_b2(migration, inflos)
    females_exo <- incoming(gamma, inflos)
    # females_exo <- matrix(30, nrow = 80, ncol = 3)
    larves <- matrix(0, nrow = nb_jours, ncol = 3)
    females_endo <- matrix(0, nrow = nb_jours, ncol = 3)
    females <- matrix(0, nrow = nb_jours, ncol = 3)
    mu_sol <- c(mu_ER, mu_PS, mu_EH)
    for (jour in 1:nb_jours) {
        larves[jour, ] <- larvaes_count(jour, inflo_capacity, inflos, females,
                                        mu_sol, reproduction)
        females_endo[jour, ] <- emerging(jour, larves, mu_sol, stock)
        females[jour, 1] <- females_count(jour, alpha[[1]],
                                          females_exo[, 1], females_endo)
        females[jour, 2] <- females_count(jour, alpha[[2]],
                                          females_exo[, 2], females_endo)
        females[jour, 3] <- females_count(jour, alpha[[3]],
                                          females_exo[, 3], females_endo)
    }
    
    larves
}

dynamics_season_b2 <- function(arg, inflos) {
    ## Renvoie la dynamique de larves pour les trois sous-blocs
    ## Modèle D
    gamma <- arg[1]
    migration <- arg[2]
    mu_ER <- arg[3]
    mu_EH <- arg[4]
    inflo_capacity <- arg[5]
    stock <- arg[6]
    # stock <- 0
    reproduction <- arg[7]
    season <- arg[8]
    
    alpha <- exchange(migration, inflos)
    females_exo <- incoming(gamma, inflos)
    # females_exo <- matrix(30, nrow = 80, ncol = 3)
    larves <- matrix(0, nrow = nb_jours, ncol = 3)
    females_endo <- matrix(0, nrow = nb_jours, ncol = 3)
    females <- matrix(0, nrow = nb_jours, ncol = 3)
    mu_sol <- c(mu_ER, mu_PS, mu_EH)
    for (jour in 1:nb_jours) {
        if (jour < 36) {
            larves[jour, ] <- larvaes_count(jour, inflo_capacity, inflos, females,
                                            mu_sol, reproduction)
            females_endo[jour, ] <- emerging(jour, larves, mu_sol, stock)
            females[jour, 1] <- females_count(jour, alpha[[1]],
                                              females_exo[, 1], females_endo)
            females[jour, 2] <- females_count(jour, alpha[[2]],
                                              females_exo[, 2], females_endo)
            females[jour, 3] <- females_count(jour, alpha[[3]],
                                              females_exo[, 3], females_endo)
        }
        
        if (jour >= 36) {
            larves[jour, ] <- larvaes_count(jour, inflo_capacity, inflos, females,
                                            mu_sol, reproduction)
            females_endo[jour, ] <- emerging(jour, larves, mu_sol, stock)
            females[jour, 1] <- females_count(jour, alpha[[1]],
                                              females_exo[, 1], females_endo)
            females[jour, 2] <- females_count(jour, alpha[[2]],
                                              females_exo[, 2], females_endo)
            females[jour, 3] <- females_count(jour, alpha[[3]],
                                              females_exo[, 3], females_endo)
            females[jour, ] <- females[jour, ] * season
        }
        
    }
    
    larves
}


# Fonctions décompositions ------------------------------------------------

larvaes_count2 <- function(day, inflo_capacity, inflos, females, reproduction) {
    ## Nombre de larves chaque jour sans limitation de ressources                   
    # beta7 <-  0
    # beta8 <-  0
    # beta9 <-  0
    # beta10 <- 1
    # beta11 <- 0
    # beta12 <- 0
    beta7 <-  0.025
    beta8 <-  0.075
    beta9 <-  0.400
    beta10 <- 0.400
    beta11 <- 0.075
    beta12 <- 0.025
    
    larvae7 <- larvae8 <- larvae9 <- larvae10 <- larvae11 <- larvae12 <- 0
    if (day > 7) {
        larvae7 <- females[day - 7, ] * reproduction
    }
    
    if (day > 8) {
        larvae8 <- females[day - 8, ] * reproduction
    }
    
    if (day > 9) {
        larvae9 <- females[day - 9, ] * reproduction
    }
    
    if (day > 10) {
        larvae10 <- females[day - 10, ] * reproduction
    }
    
    if (day > 11) {
        larvae11 <- females[day - 11, ] * reproduction
    }
    
    if (day > 12) {
        larvae12 <- females[day - 12, ] * reproduction
    }
    
    
    beta7 * larvae7 + beta8 * larvae8 + beta9 * larvae9 +
        beta10 * larvae10 + beta11 * larvae11 + beta12 * larvae12
}

larvaes_count2_E <- function(day, inflo_capacity, inflos, females, reproduction, duree_dvpmt) {
    ## Nombre de larves chaque jour sans limitation de ressources                   
    ## Modèle E
    duree_dvpmt <- round(duree_dvpmt)
    beta7 <-  0.025
    beta8 <-  0.075
    beta9 <-  0.400
    beta10 <- 0.400
    beta11 <- 0.075
    beta12 <- 0.025
    
    larvae7 <- larvae8 <- larvae9 <- larvae10 <- larvae11 <- larvae12 <- 0
    if (day > duree_dvpmt & day - duree_dvpmt <= 80) {
        larvae7 <- females[day - duree_dvpmt, ] * reproduction
    }
    
    if (day > duree_dvpmt + 1 & day - duree_dvpmt + 1 <= 80) {
        larvae8 <- females[day - duree_dvpmt, ] * reproduction
    }
    
    if (day > duree_dvpmt + 2 & day - duree_dvpmt + 2 <= 80) {
        larvae9 <- females[day - duree_dvpmt, ] * reproduction
    }
    
    if (day > duree_dvpmt + 3 & day - duree_dvpmt + 3 <= 80) {
        larvae10 <- females[day - duree_dvpmt, ] * reproduction
    }
    
    if (day > duree_dvpmt + 4 & day - duree_dvpmt + 4 <= 80) {
        larvae11 <- females[day - duree_dvpmt, ] * reproduction
    }
    
    if (day > duree_dvpmt + 5 & day - duree_dvpmt + 5 <= 80) {
        larvae12 <- females[day - duree_dvpmt, ] * reproduction
    }
    
    
    beta7 * larvae7 + beta8 * larvae8 + beta9 * larvae9 +
        beta10 * larvae10 + beta11 * larvae11 + beta12 * larvae12
}

females_side <- function(day, alpha, females_endo) {
    ## Nombre total de femelles
    alpha[day, ] %*% females_endo[day, ]
}

emerging_diap <- function(day, larves, mu_sol, stock) {
    ## Renvoie le nombre de femelles en diapause qui émergent
    if (day <= 5)
        c(0, 0, 0)
    else
        sortie_diapause2017[day] * stock * sex_ratio * mu_sol
}

decomposition_A <- function(arg, inflos) {
    ## Décompose la provenance des femelles : pupaison, diapause,
    ## side ou exogène
    ## Modèle A
    gamma <- arg[1]
    migration <- arg[2]
    mu_ER <- arg[3]
    mu_EH <- arg[4]
    inflo_capacity <- arg[5]
    stock <- arg[6]
    reproduction <- arg[7]
    
    ## Modèle normal
    alpha <- exchange(migration, inflos)
    females_exo <- incoming(gamma, inflos)
    # females_exo <- matrix(30, nrow = 80, ncol = 3)
    larves <- matrix(0, nrow = nb_jours, ncol = 3)
    females_endo <- matrix(0, nrow = nb_jours, ncol = 3)
    females <- matrix(0, nrow = nb_jours, ncol = 3)
    mu_sol <- c(mu_ER, mu_PS, mu_EH)
    
    ## Décomposition
    prop_endo <- matrix(NA, nrow = nb_jours, ncol = 3)
    prop_exo <- matrix(NA, nrow = nb_jours, ncol = 3)
    prop_side <- matrix(NA, nrow = nb_jours, ncol = 3)
    larves_pupe <- matrix(NA, nrow = nb_jours, ncol = 3)
    larves_diap <- matrix(NA, nrow = nb_jours, ncol = 3)
    larves_exo <- matrix(NA, nrow = nb_jours, ncol = 3)
    larves_side <- matrix(NA, nrow = nb_jours, ncol = 3)
    female_side <- matrix(NA, nrow = nb_jours, ncol = 3)
    female_pupe <- matrix(NA, nrow = nb_jours, ncol = 3)
    female_diap <- matrix(NA, nrow = nb_jours, ncol = 3)
    for (jour in 1:nb_jours) {
        ## Modèle normal
        larves[jour, ] <- larvaes_count(jour, inflo_capacity, inflos, females,
                                        mu_sol, reproduction)
        females_endo[jour, ] <- emerging_A(jour, larves, mu_sol, stock)
        females[jour, 1] <- females_count(jour, alpha[[1]],
                                          females_exo[, 1], females_endo)
        females[jour, 2] <- females_count(jour, alpha[[2]],
                                          females_exo[, 2], females_endo)
        females[jour, 3] <- females_count(jour, alpha[[3]],
                                          females_exo[, 3], females_endo)
        
        ## Décomposition
        female_pupe[jour, 1] <- emerging_A(jour, larves, mu_sol, stock = 0)[1] * alpha[[1]][jour, 1]
        female_pupe[jour, 2] <- emerging_A(jour, larves, mu_sol, stock = 0)[2] * alpha[[2]][jour, 2]
        female_pupe[jour, 3] <- emerging_A(jour, larves, mu_sol, stock = 0)[3] * alpha[[3]][jour, 3]
        
        female_diap[jour, 1] <- emerging_diap(jour, larves, mu_sol, stock)[1] * alpha[[1]][jour, 1]
        female_diap[jour, 2] <- emerging_diap(jour, larves, mu_sol, stock)[2] * alpha[[2]][jour, 2]
        female_diap[jour, 3] <- emerging_diap(jour, larves, mu_sol, stock)[3] * alpha[[3]][jour, 3]
        
        female_side[jour, 1] <- females_side(jour, alpha[[1]], cbind(0, females_endo[, 2:3]))
        female_side[jour, 2] <- females_side(jour, alpha[[2]], 
                                             cbind(females_endo[, 1], 0, females_endo[, 3]))
        female_side[jour, 3] <- females_side(jour, alpha[[3]], cbind(females_endo[, 1:2], 0))
        
        larves_side[jour, ] <- larvaes_count2(jour, inflo_capacity, inflos, female_side, reproduction)
        larves_exo[jour, ]  <- larvaes_count2(jour, inflo_capacity, inflos, females_exo, reproduction)
        larves_pupe[jour, ] <- larvaes_count2(jour, inflo_capacity, inflos, female_pupe, reproduction)
        larves_diap[jour, ] <- larvaes_count2(jour, inflo_capacity, inflos, female_diap, reproduction)
        
    }
    prop_pupe <- larves_pupe / (larves_pupe + larves_diap + larves_side + larves_exo)
    prop_diap <- larves_diap / (larves_pupe + larves_diap + larves_side + larves_exo)
    prop_side <- larves_side / (larves_pupe + larves_diap + larves_side + larves_exo)
    prop_exo  <- larves_exo  / (larves_pupe + larves_diap + larves_side + larves_exo)
    
    list(larves, prop_pupe * larves, prop_diap * larves,
         prop_side * larves, prop_exo * larves, females,
         female_pupe, female_diap, female_side, females_exo)
}


decomposition_B <- function(arg, inflos) {
    ## Décompose la provenance des femelles : pupaison, diapause,
    ## side ou exogène
    ## Modèle B
    gamma <- arg[1]
    migration <- arg[2]
    mu_ER <- arg[3]
    mu_EH <- arg[4]
    inflo_capacity <- arg[5]
    stock <- arg[6]
    reproduction <- arg[7]
    coef <- arg[8]
    
    ## Modèle normal
    alpha <- exchange(migration, inflos)
    females_exo <- incoming(gamma, inflos)
    # females_exo <- matrix(30, nrow = 80, ncol = 3)
    larves <- matrix(0, nrow = nb_jours, ncol = 3)
    females_endo <- matrix(0, nrow = nb_jours, ncol = 3)
    females <- matrix(0, nrow = nb_jours, ncol = 3)
    mu_sol <- c(mu_ER, mu_PS, mu_EH)
    
    ## Décomposition
    prop_endo <- matrix(NA, nrow = nb_jours, ncol = 3)
    prop_exo <- matrix(NA, nrow = nb_jours, ncol = 3)
    prop_side <- matrix(NA, nrow = nb_jours, ncol = 3)
    larves_pupe <- matrix(NA, nrow = nb_jours, ncol = 3)
    larves_diap <- matrix(NA, nrow = nb_jours, ncol = 3)
    larves_exo <- matrix(NA, nrow = nb_jours, ncol = 3)
    larves_side <- matrix(NA, nrow = nb_jours, ncol = 3)
    female_side <- matrix(NA, nrow = nb_jours, ncol = 3)
    female_pupe <- matrix(NA, nrow = nb_jours, ncol = 3)
    female_diap <- matrix(NA, nrow = nb_jours, ncol = 3)
    for (jour in 1:nb_jours) {
        ## Modèle normal
        larves[jour, ] <- larvaes_count(jour, inflo_capacity, inflos, females,
                                        mu_sol, reproduction)
        females_endo[jour, ] <- emerging_B(jour, larves, mu_sol, stock, coef)
        females[jour, 1] <- females_count(jour, alpha[[1]],
                                          females_exo[, 1], females_endo)
        females[jour, 2] <- females_count(jour, alpha[[2]],
                                          females_exo[, 2], females_endo)
        females[jour, 3] <- females_count(jour, alpha[[3]],
                                          females_exo[, 3], females_endo)
        
        ## Décomposition
        female_pupe[jour, 2] <- emerging_B(jour, larves, mu_sol,
                                           stock = 0, coef)[2] * alpha[[2]][jour, 2]
        female_pupe[jour, 3] <- emerging_B(jour, larves, mu_sol,
                                           stock = 0, coef)[3] * alpha[[3]][jour, 3]
        female_pupe[jour, 1] <- emerging_B(jour, larves, mu_sol,
                                           stock = 0, coef)[1] * alpha[[1]][jour, 1]
        
        female_diap[jour, 1] <- emerging_diap(jour, larves, mu_sol, stock)[1] * alpha[[1]][jour, 1]
        female_diap[jour, 2] <- emerging_diap(jour, larves, mu_sol, stock)[2] * alpha[[2]][jour, 2]
        female_diap[jour, 3] <- emerging_diap(jour, larves, mu_sol, stock)[3] * alpha[[3]][jour, 3]
        
        female_side[jour, 1] <- females_side(jour, alpha[[1]], cbind(0, females_endo[, 2:3]))
        female_side[jour, 2] <- females_side(jour, alpha[[2]], 
                                             cbind(females_endo[, 1], 0, females_endo[, 3]))
        female_side[jour, 3] <- females_side(jour, alpha[[3]], cbind(females_endo[, 1:2], 0))
        
        larves_side[jour, ] <- larvaes_count2(jour, inflo_capacity, inflos, female_side, reproduction)
        larves_exo[jour, ]  <- larvaes_count2(jour, inflo_capacity, inflos, females_exo, reproduction)
        larves_pupe[jour, ] <- larvaes_count2(jour, inflo_capacity, inflos, female_pupe, reproduction)
        larves_diap[jour, ] <- larvaes_count2(jour, inflo_capacity, inflos, female_diap, reproduction)
        
    }
    prop_pupe <- larves_pupe / (larves_pupe + larves_diap + larves_side + larves_exo)
    prop_diap <- larves_diap / (larves_pupe + larves_diap + larves_side + larves_exo)
    prop_side <- larves_side / (larves_pupe + larves_diap + larves_side + larves_exo)
    prop_exo  <- larves_exo  / (larves_pupe + larves_diap + larves_side + larves_exo)
    
    list(larves, prop_pupe * larves, prop_diap * larves,
         prop_side * larves, prop_exo * larves, females,
         female_pupe, female_diap, female_side, females_exo)
}

decomposition_B2 <- function(arg, inflos) {
    ## Décompose la provenance des femelles : pupaison, diapause,
    ## side ou exogène
    ## Modèle B2
    gamma <- arg[1]
    migration <- arg[2]
    mu_ER <- arg[3]
    mu_EH <- arg[4]
    inflo_capacity <- arg[5]
    stock <- arg[6]
    reproduction <- arg[7]
    coef <- arg[8]
    intercept <- arg[9]
    
    ## Modèle normal
    alpha <- exchange(migration, inflos)
    females_exo <- incoming(gamma, inflos)
    # females_exo <- matrix(30, nrow = 80, ncol = 3)
    larves <- matrix(0, nrow = nb_jours, ncol = 3)
    females_endo <- matrix(0, nrow = nb_jours, ncol = 3)
    females <- matrix(0, nrow = nb_jours, ncol = 3)
    mu_sol <- c(mu_ER, mu_PS, mu_EH)
    
    ## Décomposition
    prop_endo <- matrix(NA, nrow = nb_jours, ncol = 3)
    prop_exo <- matrix(NA, nrow = nb_jours, ncol = 3)
    prop_side <- matrix(NA, nrow = nb_jours, ncol = 3)
    larves_pupe <- matrix(NA, nrow = nb_jours, ncol = 3)
    larves_diap <- matrix(NA, nrow = nb_jours, ncol = 3)
    larves_exo <- matrix(NA, nrow = nb_jours, ncol = 3)
    larves_side <- matrix(NA, nrow = nb_jours, ncol = 3)
    female_side <- matrix(NA, nrow = nb_jours, ncol = 3)
    female_pupe <- matrix(NA, nrow = nb_jours, ncol = 3)
    female_diap <- matrix(NA, nrow = nb_jours, ncol = 3)
    for (jour in 1:nb_jours) {
        ## Modèle normal
        larves[jour, ] <- larvaes_count(jour, inflo_capacity, inflos, females,
                                        mu_sol, reproduction)
        females_endo[jour, ] <- emerging_B2(jour, larves, mu_sol, stock, coef, intercept)
        females[jour, 1] <- females_count(jour, alpha[[1]],
                                          females_exo[, 1], females_endo)
        females[jour, 2] <- females_count(jour, alpha[[2]],
                                          females_exo[, 2], females_endo)
        females[jour, 3] <- females_count(jour, alpha[[3]],
                                          females_exo[, 3], females_endo)
        
        ## Décomposition
        female_pupe[jour, 2] <- emerging_B2(jour, larves, mu_sol,
                                           stock = 0, coef, intercept)[2] * alpha[[2]][jour, 2]
        female_pupe[jour, 3] <- emerging_B2(jour, larves, mu_sol,
                                           stock = 0, coef, intercept)[3] * alpha[[3]][jour, 3]
        female_pupe[jour, 1] <- emerging_B2(jour, larves, mu_sol,
                                           stock = 0, coef, intercept)[1] * alpha[[1]][jour, 1]
        
        female_diap[jour, 1] <- emerging_diap(jour, larves, mu_sol, stock)[1] * alpha[[1]][jour, 1]
        female_diap[jour, 2] <- emerging_diap(jour, larves, mu_sol, stock)[2] * alpha[[2]][jour, 2]
        female_diap[jour, 3] <- emerging_diap(jour, larves, mu_sol, stock)[3] * alpha[[3]][jour, 3]
        
        female_side[jour, 1] <- females_side(jour, alpha[[1]], cbind(0, females_endo[, 2:3]))
        female_side[jour, 2] <- females_side(jour, alpha[[2]], 
                                             cbind(females_endo[, 1], 0, females_endo[, 3]))
        female_side[jour, 3] <- females_side(jour, alpha[[3]], cbind(females_endo[, 1:2], 0))
        
        larves_side[jour, ] <- larvaes_count2(jour, inflo_capacity, inflos, female_side, reproduction)
        larves_exo[jour, ]  <- larvaes_count2(jour, inflo_capacity, inflos, females_exo, reproduction)
        larves_pupe[jour, ] <- larvaes_count2(jour, inflo_capacity, inflos, female_pupe, reproduction)
        larves_diap[jour, ] <- larvaes_count2(jour, inflo_capacity, inflos, female_diap, reproduction)
        
    }
    prop_pupe <- larves_pupe / (larves_pupe + larves_diap + larves_side + larves_exo)
    prop_diap <- larves_diap / (larves_pupe + larves_diap + larves_side + larves_exo)
    prop_side <- larves_side / (larves_pupe + larves_diap + larves_side + larves_exo)
    prop_exo  <- larves_exo  / (larves_pupe + larves_diap + larves_side + larves_exo)
    
    list(larves, prop_pupe * larves, prop_diap * larves,
         prop_side * larves, prop_exo * larves, females,
         female_pupe, female_diap, female_side, females_exo)
}

decomposition <- function(arg, inflos) {
    ## Décompose la provenance des femelles : pupaison, diapause,
    ## side ou exogène
    ## Modele C
    gamma <- arg[1]
    migration <- arg[2]
    mu_ER <- arg[3]
    mu_EH <- arg[4]
    inflo_capacity <- arg[5]
    stock <- arg[6]
    reproduction <- arg[7]
    
    ## Modèle normal
    alpha <- exchange(migration, inflos)
    females_exo <- incoming(gamma, inflos)
    # females_exo <- matrix(30, nrow = 80, ncol = 3)
    larves <- matrix(0, nrow = nb_jours, ncol = 3)
    females_endo <- matrix(0, nrow = nb_jours, ncol = 3)
    females <- matrix(0, nrow = nb_jours, ncol = 3)
    mu_sol <- c(mu_ER, mu_PS, mu_EH)
    
    ## Décomposition
    prop_endo <- matrix(NA, nrow = nb_jours, ncol = 3)
    prop_exo <- matrix(NA, nrow = nb_jours, ncol = 3)
    prop_side <- matrix(NA, nrow = nb_jours, ncol = 3)
    larves_pupe <- matrix(NA, nrow = nb_jours, ncol = 3)
    larves_diap <- matrix(NA, nrow = nb_jours, ncol = 3)
    larves_exo <- matrix(NA, nrow = nb_jours, ncol = 3)
    larves_side <- matrix(NA, nrow = nb_jours, ncol = 3)
    female_side <- matrix(NA, nrow = nb_jours, ncol = 3)
    female_pupe <- matrix(NA, nrow = nb_jours, ncol = 3)
    female_diap <- matrix(NA, nrow = nb_jours, ncol = 3)
    for (jour in 1:nb_jours) {
        ## Modèle normal
        larves[jour, ] <- larvaes_count(jour, inflo_capacity, inflos, females,
                                        mu_sol, reproduction)
        females_endo[jour, ] <- emerging(jour, larves, mu_sol, stock)
        females[jour, 1] <- females_count(jour, alpha[[1]],
                                          females_exo[, 1], females_endo)
        females[jour, 2] <- females_count(jour, alpha[[2]],
                                          females_exo[, 2], females_endo)
        females[jour, 3] <- females_count(jour, alpha[[3]],
                                          females_exo[, 3], females_endo)
        
        ## Décomposition
        female_pupe[jour, 1] <- emerging(jour, larves, mu_sol, stock = 0)[1] * alpha[[1]][jour, 1]
        female_pupe[jour, 2] <- emerging(jour, larves, mu_sol, stock = 0)[2] * alpha[[2]][jour, 2]
        female_pupe[jour, 3] <- emerging(jour, larves, mu_sol, stock = 0)[3] * alpha[[3]][jour, 3]
        
        female_diap[jour, 1] <- emerging_diap(jour, larves, mu_sol, stock)[1] * alpha[[1]][jour, 1]
        female_diap[jour, 2] <- emerging_diap(jour, larves, mu_sol, stock)[2] * alpha[[2]][jour, 2]
        female_diap[jour, 3] <- emerging_diap(jour, larves, mu_sol, stock)[3] * alpha[[3]][jour, 3]
        
        female_side[jour, 1] <- females_side(jour, alpha[[1]], cbind(0, females_endo[, 2:3]))
        female_side[jour, 2] <- females_side(jour, alpha[[2]], 
                                             cbind(females_endo[, 1], 0, females_endo[, 3]))
        female_side[jour, 3] <- females_side(jour, alpha[[3]], cbind(females_endo[, 1:2], 0))
        
        larves_side[jour, ] <- larvaes_count2(jour, inflo_capacity, inflos, female_side, reproduction)
        larves_exo[jour, ]  <- larvaes_count2(jour, inflo_capacity, inflos, females_exo, reproduction)
        larves_pupe[jour, ] <- larvaes_count2(jour, inflo_capacity, inflos, female_pupe, reproduction)
        larves_diap[jour, ] <- larvaes_count2(jour, inflo_capacity, inflos, female_diap, reproduction)
        
    }
    prop_pupe <- larves_pupe / (larves_pupe + larves_diap + larves_side + larves_exo)
    prop_diap <- larves_diap / (larves_pupe + larves_diap + larves_side + larves_exo)
    prop_side <- larves_side / (larves_pupe + larves_diap + larves_side + larves_exo)
    prop_exo  <- larves_exo  / (larves_pupe + larves_diap + larves_side + larves_exo)
    
    list(larves, prop_pupe * larves, prop_diap * larves,
         prop_side * larves, prop_exo * larves, females,
         female_pupe, female_diap, female_side, females_exo)
}

decomposition_C2 <- function(arg, inflos) {
    ## Décompose la provenance des femelles : pupaison, diapause,
    ## side ou exogène
    ## Modele C (possibilité de calibrer la duréée d'attra)
    gamma <- arg[1]
    migration <- arg[2]
    mu_ER <- arg[3]
    mu_EH <- arg[4]
    inflo_capacity <- arg[5]
    stock <- arg[6]
    reproduction <- arg[7]
    duree_attra <- arg[8]
    
    inflos <- inflos_attractives(bursts, duree_attra)
    ## Modèle normal
    alpha <- exchange(migration, inflos)
    females_exo <- incoming(gamma, inflos)
    # females_exo <- matrix(30, nrow = 80, ncol = 3)
    larves <- matrix(0, nrow = nb_jours, ncol = 3)
    females_endo <- matrix(0, nrow = nb_jours, ncol = 3)
    females <- matrix(0, nrow = nb_jours, ncol = 3)
    mu_sol <- c(mu_ER, mu_PS, mu_EH)
    
    ## Décomposition
    prop_endo <- matrix(NA, nrow = nb_jours, ncol = 3)
    prop_exo <- matrix(NA, nrow = nb_jours, ncol = 3)
    prop_side <- matrix(NA, nrow = nb_jours, ncol = 3)
    larves_pupe <- matrix(NA, nrow = nb_jours, ncol = 3)
    larves_diap <- matrix(NA, nrow = nb_jours, ncol = 3)
    larves_exo <- matrix(NA, nrow = nb_jours, ncol = 3)
    larves_side <- matrix(NA, nrow = nb_jours, ncol = 3)
    female_side <- matrix(NA, nrow = nb_jours, ncol = 3)
    female_pupe <- matrix(NA, nrow = nb_jours, ncol = 3)
    female_diap <- matrix(NA, nrow = nb_jours, ncol = 3)
    for (jour in 1:nb_jours) {
        ## Modèle normal
        larves[jour, ] <- larvaes_count(jour, inflo_capacity, inflos, females,
                                        mu_sol, reproduction)
        females_endo[jour, ] <- emerging(jour, larves, mu_sol, stock)
        females[jour, 1] <- females_count(jour, alpha[[1]],
                                          females_exo[, 1], females_endo)
        females[jour, 2] <- females_count(jour, alpha[[2]],
                                          females_exo[, 2], females_endo)
        females[jour, 3] <- females_count(jour, alpha[[3]],
                                          females_exo[, 3], females_endo)
        
        ## Décomposition
        female_pupe[jour, 2] <- emerging(jour, larves, mu_sol,
                                         stock = 0)[2] * alpha[[2]][jour, 2]
        female_pupe[jour, 3] <- emerging(jour, larves, mu_sol,
                                         stock = 0)[3] * alpha[[3]][jour, 3]
        female_pupe[jour, 1] <- emerging(jour, larves, mu_sol,
                                         stock = 0)[1] * alpha[[1]][jour, 1]
        
        female_diap[jour, 1] <- emerging_diap(jour, larves, mu_sol, stock)[1] * alpha[[1]][jour, 1]
        female_diap[jour, 2] <- emerging_diap(jour, larves, mu_sol, stock)[2] * alpha[[2]][jour, 2]
        female_diap[jour, 3] <- emerging_diap(jour, larves, mu_sol, stock)[3] * alpha[[3]][jour, 3]
        
        female_side[jour, 1] <- females_side(jour, alpha[[1]], cbind(0, females_endo[, 2:3]))
        female_side[jour, 2] <- females_side(jour, alpha[[2]], 
                                             cbind(females_endo[, 1], 0, females_endo[, 3]))
        female_side[jour, 3] <- females_side(jour, alpha[[3]], cbind(females_endo[, 1:2], 0))
        
        larves_side[jour, ] <- larvaes_count2(jour, inflo_capacity, inflos, female_side, reproduction)
        larves_exo[jour, ]  <- larvaes_count2(jour, inflo_capacity, inflos, females_exo, reproduction)
        larves_pupe[jour, ] <- larvaes_count2(jour, inflo_capacity, inflos, female_pupe, reproduction)
        larves_diap[jour, ] <- larvaes_count2(jour, inflo_capacity, inflos, female_diap, reproduction)
        
    }
    prop_pupe <- larves_pupe / (larves_pupe + larves_diap + larves_side + larves_exo)
    prop_diap <- larves_diap / (larves_pupe + larves_diap + larves_side + larves_exo)
    prop_side <- larves_side / (larves_pupe + larves_diap + larves_side + larves_exo)
    prop_exo  <- larves_exo  / (larves_pupe + larves_diap + larves_side + larves_exo)
    
    list(larves, prop_pupe * larves, prop_diap * larves,
         prop_side * larves, prop_exo * larves, females,
         female_pupe, female_diap, female_side, females_exo)
}



decomposition_season_bloc1 <- function(arg, inflos) {
    ## Décompose la provenance des femelles : pupaison, diapause,
    ## side ou exogène
    ## Modele D
    gamma <- arg[1]
    migration <- arg[2]
    mu_ER <- arg[3]
    mu_EH <- arg[4]
    inflo_capacity <- arg[5]
    stock <- arg[6]
    # stock <- 0
    reproduction <- arg[7]
    season <- arg[8]
    
    ## Modèle normal
    alpha <- exchange(migration, inflos)
    females_exo <- incoming(gamma, inflos)
    # females_exo <- matrix(30, nrow = 80, ncol = 3)
    larves <- matrix(0, nrow = nb_jours, ncol = 3)
    females_endo <- matrix(0, nrow = nb_jours, ncol = 3)
    females <- matrix(0, nrow = nb_jours, ncol = 3)
    mu_sol <- c(mu_ER, mu_PS, mu_EH)
    
    ## Décomposition
    prop_endo <- matrix(NA, nrow = nb_jours, ncol = 3)
    prop_exo <- matrix(NA, nrow = nb_jours, ncol = 3)
    prop_side <- matrix(NA, nrow = nb_jours, ncol = 3)
    larves_pupe <- matrix(NA, nrow = nb_jours, ncol = 3)
    larves_diap <- matrix(NA, nrow = nb_jours, ncol = 3)
    larves_exo <- matrix(NA, nrow = nb_jours, ncol = 3)
    larves_side <- matrix(NA, nrow = nb_jours, ncol = 3)
    female_side <- matrix(NA, nrow = nb_jours, ncol = 3)
    female_pupe <- matrix(NA, nrow = nb_jours, ncol = 3)
    female_diap <- matrix(NA, nrow = nb_jours, ncol = 3)
    for (jour in 1:nb_jours) {
        if (jour < 59) {
            ## Modèle normal
            larves[jour, ] <- larvaes_count(jour, inflo_capacity, inflos, females,
                                            mu_sol, reproduction)
            females_endo[jour, ] <- emerging(jour, larves, mu_sol, stock)
            females[jour, 1] <- females_count(jour, alpha[[1]],
                                              females_exo[, 1], females_endo)
            females[jour, 2] <- females_count(jour, alpha[[2]],
                                              females_exo[, 2], females_endo)
            females[jour, 3] <- females_count(jour, alpha[[3]],
                                              females_exo[, 3], females_endo)
        } else {
            larves[jour, ] <- larvaes_count(jour, inflo_capacity, inflos, females,
                                            mu_sol, reproduction)
            females_endo[jour, ] <- emerging(jour, larves, mu_sol, stock)
            females[jour, 1] <- females_count(jour, alpha[[1]],
                                              females_exo[, 1], females_endo)
            females[jour, 2] <- females_count(jour, alpha[[2]],
                                              females_exo[, 2], females_endo)
            females[jour, 3] <- females_count(jour, alpha[[3]],
                                              females_exo[, 3], females_endo)
            females[jour, ] <- females[jour, ] * season
        }
        
        ## Décomposition
        female_pupe[jour, 1] <- emerging(jour, larves, mu_sol, stock = 0)[1] * alpha[[1]][jour, 1]
        female_pupe[jour, 2] <- emerging(jour, larves, mu_sol, stock = 0)[2] * alpha[[2]][jour, 2]
        female_pupe[jour, 3] <- emerging(jour, larves, mu_sol, stock = 0)[3] * alpha[[3]][jour, 3]
        
        female_diap[jour, 1] <- emerging_diap(jour, larves, mu_sol, stock)[1] * alpha[[1]][jour, 1]
        female_diap[jour, 2] <- emerging_diap(jour, larves, mu_sol, stock)[2] * alpha[[2]][jour, 2]
        female_diap[jour, 3] <- emerging_diap(jour, larves, mu_sol, stock)[3] * alpha[[3]][jour, 3]
        
        female_side[jour, 1] <- females_side(jour, alpha[[1]], cbind(0, females_endo[, 2:3]))
        female_side[jour, 2] <- females_side(jour, alpha[[2]], 
                                             cbind(females_endo[, 1], 0, females_endo[, 3]))
        female_side[jour, 3] <- females_side(jour, alpha[[3]], cbind(females_endo[, 1:2], 0))
        
        larves_side[jour, ] <- larvaes_count2(jour, inflo_capacity, inflos, female_side, reproduction)
        larves_exo[jour, ]  <- larvaes_count2(jour, inflo_capacity, inflos, females_exo, reproduction)
        larves_pupe[jour, ] <- larvaes_count2(jour, inflo_capacity, inflos, female_pupe, reproduction)
        larves_diap[jour, ] <- larvaes_count2(jour, inflo_capacity, inflos, female_diap, reproduction)
        
    }
    prop_pupe <- larves_pupe / (larves_pupe + larves_diap + larves_side + larves_exo)
    prop_diap <- larves_diap / (larves_pupe + larves_diap + larves_side + larves_exo)
    prop_side <- larves_side / (larves_pupe + larves_diap + larves_side + larves_exo)
    prop_exo  <- larves_exo  / (larves_pupe + larves_diap + larves_side + larves_exo)
    
    list(larves, prop_pupe * larves, prop_diap * larves,
         prop_side * larves, prop_exo * larves, females,
         female_pupe, female_diap, female_side, females_exo)
}



decomposition_E <- function(arg, inflos) {
    ## Décompose la provenance des femelles : pupaison, diapause,
    ## side ou exogène
    ## Modele C
    gamma <- arg[1]
    migration <- arg[2]
    mu_ER <- arg[3]
    mu_EH <- arg[4]
    inflo_capacity <- arg[5]
    stock <- arg[6]
    reproduction <- arg[7]
    duree_dvpmt <- arg[8]
    
    ## Modèle normal
    alpha <- exchange(migration, inflos)
    females_exo <- incoming(gamma, inflos)
    # females_exo <- matrix(30, nrow = 80, ncol = 3)
    larves <- matrix(0, nrow = nb_jours, ncol = 3)
    females_endo <- matrix(0, nrow = nb_jours, ncol = 3)
    females <- matrix(0, nrow = nb_jours, ncol = 3)
    mu_sol <- c(mu_ER, mu_PS, mu_EH)
    
    ## Décomposition
    prop_endo <- matrix(NA, nrow = nb_jours, ncol = 3)
    prop_exo <- matrix(NA, nrow = nb_jours, ncol = 3)
    prop_side <- matrix(NA, nrow = nb_jours, ncol = 3)
    larves_pupe <- matrix(NA, nrow = nb_jours, ncol = 3)
    larves_diap <- matrix(NA, nrow = nb_jours, ncol = 3)
    larves_exo <- matrix(NA, nrow = nb_jours, ncol = 3)
    larves_side <- matrix(NA, nrow = nb_jours, ncol = 3)
    female_side <- matrix(NA, nrow = nb_jours, ncol = 3)
    female_pupe <- matrix(NA, nrow = nb_jours, ncol = 3)
    female_diap <- matrix(NA, nrow = nb_jours, ncol = 3)
    for (jour in 1:nb_jours) {
        ## Modèle normal
        larves[jour, ] <- larvaes_count_E(jour, inflo_capacity, inflos, females,
                                        mu_sol, reproduction, duree_dvpmt)
        females_endo[jour, ] <- emerging(jour, larves, mu_sol, stock)
        females[jour, 1] <- females_count(jour, alpha[[1]],
                                          females_exo[, 1], females_endo)
        females[jour, 2] <- females_count(jour, alpha[[2]],
                                          females_exo[, 2], females_endo)
        females[jour, 3] <- females_count(jour, alpha[[3]],
                                          females_exo[, 3], females_endo)
        
        ## Décomposition
        female_pupe[jour, 1] <- emerging(jour, larves, mu_sol, stock = 0)[1] * alpha[[1]][jour, 1]
        female_pupe[jour, 2] <- emerging(jour, larves, mu_sol, stock = 0)[2] * alpha[[2]][jour, 2]
        female_pupe[jour, 3] <- emerging(jour, larves, mu_sol, stock = 0)[3] * alpha[[3]][jour, 3]
        
        female_diap[jour, 1] <- emerging_diap(jour, larves, mu_sol, stock)[1] * alpha[[1]][jour, 1]
        female_diap[jour, 2] <- emerging_diap(jour, larves, mu_sol, stock)[2] * alpha[[2]][jour, 2]
        female_diap[jour, 3] <- emerging_diap(jour, larves, mu_sol, stock)[3] * alpha[[3]][jour, 3]
        
        female_side[jour, 1] <- females_side(jour, alpha[[1]], cbind(0, females_endo[, 2:3]))
        female_side[jour, 2] <- females_side(jour, alpha[[2]], 
                                             cbind(females_endo[, 1], 0, females_endo[, 3]))
        female_side[jour, 3] <- females_side(jour, alpha[[3]], cbind(females_endo[, 1:2], 0))
        
        larves_side[jour, ] <- larvaes_count2_E(jour, inflo_capacity, inflos, female_side,
                                                reproduction, duree_dvpmt)
        larves_exo[jour, ]  <- larvaes_count2_E(jour, inflo_capacity, inflos, females_exo,
                                                reproduction, duree_dvpmt)
        larves_pupe[jour, ] <- larvaes_count2_E(jour, inflo_capacity, inflos, female_pupe,
                                                reproduction, duree_dvpmt)
        larves_diap[jour, ] <- larvaes_count2_E(jour, inflo_capacity, inflos, female_diap,
                                                reproduction, duree_dvpmt)
        # browser()
        
    }
    prop_pupe <- larves_pupe / (larves_pupe + larves_diap + larves_side + larves_exo)
    prop_diap <- larves_diap / (larves_pupe + larves_diap + larves_side + larves_exo)
    prop_side <- larves_side / (larves_pupe + larves_diap + larves_side + larves_exo)
    prop_exo  <- larves_exo  / (larves_pupe + larves_diap + larves_side + larves_exo)
    # browser()
    list(larves, prop_pupe * larves, prop_diap * larves,
         prop_side * larves, prop_exo * larves, females,
         female_pupe, female_diap, female_side, females_exo)
}


# Décomposition bloc 2 ----------------------------------------------------

decomposition_b2 <- function(arg, inflos) {
    ## Décompose la provenance des femelles : pupaison, diapause,
    ## side ou exogène
    ## Modèle C
    gamma <- arg[1]
    migration <- arg[2]
    mu_ER <- arg[3]
    mu_EH <- arg[4]
    inflo_capacity <- arg[5]
    stock <- arg[6]
    reproduction <- arg[7]
    
    ## Modèle normal
    alpha <- exchange_b2(migration, inflos)
    females_exo <- incoming(gamma, inflos)
    # females_exo <- matrix(30, nrow = 80, ncol = 3)
    larves <- matrix(0, nrow = nb_jours, ncol = 3)
    females_endo <- matrix(0, nrow = nb_jours, ncol = 3)
    females <- matrix(0, nrow = nb_jours, ncol = 3)
    mu_sol <- c(mu_ER, mu_PS, mu_EH)
    
    ## Décomposition
    prop_endo <- matrix(NA, nrow = nb_jours, ncol = 3)
    prop_exo <- matrix(NA, nrow = nb_jours, ncol = 3)
    prop_side <- matrix(NA, nrow = nb_jours, ncol = 3)
    larves_pupe <- matrix(NA, nrow = nb_jours, ncol = 3)
    larves_diap <- matrix(NA, nrow = nb_jours, ncol = 3)
    larves_exo <- matrix(NA, nrow = nb_jours, ncol = 3)
    larves_side <- matrix(NA, nrow = nb_jours, ncol = 3)
    female_side <- matrix(NA, nrow = nb_jours, ncol = 3)
    female_pupe <- matrix(NA, nrow = nb_jours, ncol = 3)
    female_diap <- matrix(NA, nrow = nb_jours, ncol = 3)
    for (jour in 1:nb_jours) {
        ## Modèle normal
        larves[jour, ] <- larvaes_count(jour, inflo_capacity, inflos, females,
                                        mu_sol, reproduction)
        females_endo[jour, ] <- emerging(jour, larves, mu_sol, stock)
        females[jour, 1] <- females_count(jour, alpha[[1]],
                                          females_exo[, 1], females_endo)
        females[jour, 2] <- females_count(jour, alpha[[2]],
                                          females_exo[, 2], females_endo)
        females[jour, 3] <- females_count(jour, alpha[[3]],
                                          females_exo[, 3], females_endo)
        
        ## Décomposition
        female_pupe[jour, 1] <- emerging(jour, larves, mu_sol, stock = 0)[1] * alpha[[1]][jour, 1]
        female_pupe[jour, 2] <- emerging(jour, larves, mu_sol, stock = 0)[2] * alpha[[2]][jour, 2]
        female_pupe[jour, 3] <- emerging(jour, larves, mu_sol, stock = 0)[3] * alpha[[3]][jour, 3]
        
        female_diap[jour, 1] <- emerging_diap(jour, larves, mu_sol, stock)[1] * alpha[[1]][jour, 1]
        female_diap[jour, 2] <- emerging_diap(jour, larves, mu_sol, stock)[2] * alpha[[2]][jour, 2]
        female_diap[jour, 3] <- emerging_diap(jour, larves, mu_sol, stock)[3] * alpha[[3]][jour, 3]
        
        female_side[jour, 1] <- females_side(jour, alpha[[1]], cbind(0, females_endo[, 2:3]))
        female_side[jour, 2] <- females_side(jour, alpha[[2]], 
                                             cbind(females_endo[, 1], 0, females_endo[, 3]))
        female_side[jour, 3] <- females_side(jour, alpha[[3]], cbind(females_endo[, 1:2], 0))
        
        larves_side[jour, ] <- larvaes_count2(jour, inflo_capacity, inflos, female_side, reproduction)
        larves_exo[jour, ]  <- larvaes_count2(jour, inflo_capacity, inflos, females_exo, reproduction)
        larves_pupe[jour, ] <- larvaes_count2(jour, inflo_capacity, inflos, female_pupe, reproduction)
        larves_diap[jour, ] <- larvaes_count2(jour, inflo_capacity, inflos, female_diap, reproduction)
        
    }
    prop_pupe <- larves_pupe / (larves_pupe + larves_diap + larves_side + larves_exo)
    prop_diap <- larves_diap / (larves_pupe + larves_diap + larves_side + larves_exo)
    prop_side <- larves_side / (larves_pupe + larves_diap + larves_side + larves_exo)
    prop_exo  <- larves_exo  / (larves_pupe + larves_diap + larves_side + larves_exo)
    
    list(larves, prop_pupe * larves, prop_diap * larves,
         prop_side * larves, prop_exo * larves, females,
         female_pupe, female_diap, female_side, females_exo)
}




decomposition_season_bloc2 <- function(arg, inflos) {
    ## Décompose la provenance des femelles : pupaison, diapause,
    ## side ou exogène
    gamma <- arg[1]
    migration <- arg[2]
    mu_ER <- arg[3]
    mu_EH <- arg[4]
    inflo_capacity <- arg[5]
    stock <- arg[6]
    # stock <- 0
    reproduction <- arg[7]
    season <- arg[8]
    
    ## Modèle normal
    alpha <- exchange_b2(migration, inflos)
    # females_exo <- incoming(gamma, inflos)
    females_exo <- matrix(30, nrow = 80, ncol = 3)
    larves <- matrix(0, nrow = nb_jours, ncol = 3)
    females_endo <- matrix(0, nrow = nb_jours, ncol = 3)
    females <- matrix(0, nrow = nb_jours, ncol = 3)
    mu_sol <- c(mu_ER, mu_PS, mu_EH)
    
    ## Décomposition
    prop_endo <- matrix(NA, nrow = nb_jours, ncol = 3)
    prop_exo <- matrix(NA, nrow = nb_jours, ncol = 3)
    prop_side <- matrix(NA, nrow = nb_jours, ncol = 3)
    larves_pupe <- matrix(NA, nrow = nb_jours, ncol = 3)
    larves_diap <- matrix(NA, nrow = nb_jours, ncol = 3)
    larves_exo <- matrix(NA, nrow = nb_jours, ncol = 3)
    larves_side <- matrix(NA, nrow = nb_jours, ncol = 3)
    female_side <- matrix(NA, nrow = nb_jours, ncol = 3)
    female_pupe <- matrix(NA, nrow = nb_jours, ncol = 3)
    female_diap <- matrix(NA, nrow = nb_jours, ncol = 3)
    for (jour in 1:nb_jours) {
        if (jour < 36) {
            larves[jour, ] <- larvaes_count(jour, inflo_capacity, inflos, females,
                                            mu_sol, reproduction)
            females_endo[jour, ] <- emerging(jour, larves, mu_sol, stock)
            females[jour, 1] <- females_count(jour, alpha[[1]],
                                              females_exo[, 1], females_endo)
            females[jour, 2] <- females_count(jour, alpha[[2]],
                                              females_exo[, 2], females_endo)
            females[jour, 3] <- females_count(jour, alpha[[3]],
                                              females_exo[, 3], females_endo)
        }
        
        if (jour >= 36 & jour < 51) {
            larves[jour, ] <- larvaes_count(jour, inflo_capacity, inflos, females,
                                            mu_sol, reproduction)
            females_endo[jour, ] <- emerging(jour, larves, mu_sol, stock)
            females[jour, 1] <- females_count(jour, alpha[[1]],
                                              females_exo[, 1], females_endo)
            females[jour, 2] <- females_count(jour, alpha[[2]],
                                              females_exo[, 2], females_endo)
            females[jour, 3] <- females_count(jour, alpha[[3]],
                                              females_exo[, 3], females_endo)
            females[jour, 2:3] <- females[jour, 2:3] * season
        }
        
        if (jour >= 51) {
            larves[jour, ] <- larvaes_count(jour, inflo_capacity, inflos, females,
                                            mu_sol, reproduction)
            females_endo[jour, ] <- emerging(jour, larves, mu_sol, stock)
            females[jour, 1] <- females_count(jour, alpha[[1]],
                                              females_exo[, 1], females_endo)
            females[jour, 2] <- females_count(jour, alpha[[2]],
                                              females_exo[, 2], females_endo)
            females[jour, 3] <- females_count(jour, alpha[[3]],
                                              females_exo[, 3], females_endo)
            females[jour, ] <- females[jour, ] * season
        }
        
        ## Décomposition
        female_pupe[jour, 1] <- emerging(jour, larves, mu_sol, stock = 0)[1] * alpha[[1]][jour, 1]
        female_pupe[jour, 2] <- emerging(jour, larves, mu_sol, stock = 0)[2] * alpha[[2]][jour, 2]
        female_pupe[jour, 3] <- emerging(jour, larves, mu_sol, stock = 0)[3] * alpha[[3]][jour, 3]
        
        female_diap[jour, 1] <- emerging_diap(jour, larves, mu_sol, stock)[1] * alpha[[1]][jour, 1]
        female_diap[jour, 2] <- emerging_diap(jour, larves, mu_sol, stock)[2] * alpha[[2]][jour, 2]
        female_diap[jour, 3] <- emerging_diap(jour, larves, mu_sol, stock)[3] * alpha[[3]][jour, 3]
        
        female_side[jour, 1] <- females_side(jour, alpha[[1]], cbind(0, females_endo[, 2:3]))
        female_side[jour, 2] <- females_side(jour, alpha[[2]], 
                                             cbind(females_endo[, 1], 0, females_endo[, 3]))
        female_side[jour, 3] <- females_side(jour, alpha[[3]], cbind(females_endo[, 1:2], 0))
        
        larves_side[jour, ] <- larvaes_count2(jour, inflo_capacity, inflos, female_side, reproduction)
        larves_exo[jour, ]  <- larvaes_count2(jour, inflo_capacity, inflos, females_exo, reproduction)
        larves_pupe[jour, ] <- larvaes_count2(jour, inflo_capacity, inflos, female_pupe, reproduction)
        larves_diap[jour, ] <- larvaes_count2(jour, inflo_capacity, inflos, female_diap, reproduction)
        
    }
    prop_pupe <- larves_pupe / (larves_pupe + larves_diap + larves_side + larves_exo)
    prop_diap <- larves_diap / (larves_pupe + larves_diap + larves_side + larves_exo)
    prop_side <- larves_side / (larves_pupe + larves_diap + larves_side + larves_exo)
    prop_exo  <- larves_exo  / (larves_pupe + larves_diap + larves_side + larves_exo)
    
    list(larves, prop_pupe * larves, prop_diap * larves,
         prop_side * larves, prop_exo * larves, females,
         female_pupe, female_diap, female_side, females_exo)
}

decomposition_15sept_bloc2 <- function(arg, inflos) {
    ## Décompose la provenance des femelles : pupaison, diapause,
    ## side ou exogène
    gamma <- arg[1]
    migration <- arg[2]
    mu_ER <- arg[3]
    mu_EH <- arg[4]
    inflo_capacity <- arg[5]
    stock <- arg[6]
    # stock <- 0
    reproduction <- arg[7]
    season <- arg[8]
    
    ## Modèle normal
    alpha <- exchange_b2(migration, inflos)
    # females_exo <- incoming(gamma, inflos)
    females_exo <- matrix(30, nrow = 80, ncol = 3)
    larves <- matrix(0, nrow = nb_jours, ncol = 3)
    females_endo <- matrix(0, nrow = nb_jours, ncol = 3)
    females <- matrix(0, nrow = nb_jours, ncol = 3)
    mu_sol <- c(mu_ER, mu_PS, mu_EH)
    
    ## Décomposition
    prop_endo <- matrix(NA, nrow = nb_jours, ncol = 3)
    prop_exo <- matrix(NA, nrow = nb_jours, ncol = 3)
    prop_side <- matrix(NA, nrow = nb_jours, ncol = 3)
    larves_pupe <- matrix(NA, nrow = nb_jours, ncol = 3)
    larves_diap <- matrix(NA, nrow = nb_jours, ncol = 3)
    larves_exo <- matrix(NA, nrow = nb_jours, ncol = 3)
    larves_side <- matrix(NA, nrow = nb_jours, ncol = 3)
    female_side <- matrix(NA, nrow = nb_jours, ncol = 3)
    female_pupe <- matrix(NA, nrow = nb_jours, ncol = 3)
    female_diap <- matrix(NA, nrow = nb_jours, ncol = 3)
    for (jour in 1:nb_jours) {
        if (jour < 59) {
            ## Modèle normal
            larves[jour, ] <- larvaes_count(jour, inflo_capacity, inflos, females,
                                            mu_sol, reproduction)
            females_endo[jour, ] <- emerging(jour, larves, mu_sol, stock)
            females[jour, 1] <- females_count(jour, alpha[[1]],
                                              females_exo[, 1], females_endo)
            females[jour, 2] <- females_count(jour, alpha[[2]],
                                              females_exo[, 2], females_endo)
            females[jour, 3] <- females_count(jour, alpha[[3]],
                                              females_exo[, 3], females_endo)
        } else {
            larves[jour, ] <- larvaes_count(jour, inflo_capacity, inflos, females,
                                            mu_sol, reproduction)
            females_endo[jour, ] <- emerging(jour, larves, mu_sol, stock)
            females[jour, 1] <- females_count(jour, alpha[[1]],
                                              females_exo[, 1], females_endo)
            females[jour, 2] <- females_count(jour, alpha[[2]],
                                              females_exo[, 2], females_endo)
            females[jour, 3] <- females_count(jour, alpha[[3]],
                                              females_exo[, 3], females_endo)
            females[jour, ] <- females[jour, ] * season
        }
        
        ## Décomposition
        female_pupe[jour, 1] <- emerging(jour, larves, mu_sol, stock = 0)[1] * alpha[[1]][jour, 1]
        female_pupe[jour, 2] <- emerging(jour, larves, mu_sol, stock = 0)[2] * alpha[[2]][jour, 2]
        female_pupe[jour, 3] <- emerging(jour, larves, mu_sol, stock = 0)[3] * alpha[[3]][jour, 3]
        
        female_diap[jour, 1] <- emerging_diap(jour, larves, mu_sol, stock)[1] * alpha[[1]][jour, 1]
        female_diap[jour, 2] <- emerging_diap(jour, larves, mu_sol, stock)[2] * alpha[[2]][jour, 2]
        female_diap[jour, 3] <- emerging_diap(jour, larves, mu_sol, stock)[3] * alpha[[3]][jour, 3]
        
        female_side[jour, 1] <- females_side(jour, alpha[[1]], cbind(0, females_endo[, 2:3]))
        female_side[jour, 2] <- females_side(jour, alpha[[2]], 
                                             cbind(females_endo[, 1], 0, females_endo[, 3]))
        female_side[jour, 3] <- females_side(jour, alpha[[3]], cbind(females_endo[, 1:2], 0))
        
        larves_side[jour, ] <- larvaes_count2(jour, inflo_capacity, inflos, female_side, reproduction)
        larves_exo[jour, ]  <- larvaes_count2(jour, inflo_capacity, inflos, females_exo, reproduction)
        larves_pupe[jour, ] <- larvaes_count2(jour, inflo_capacity, inflos, female_pupe, reproduction)
        larves_diap[jour, ] <- larvaes_count2(jour, inflo_capacity, inflos, female_diap, reproduction)
        
    }
    prop_pupe <- larves_pupe / (larves_pupe + larves_diap + larves_side + larves_exo)
    prop_diap <- larves_diap / (larves_pupe + larves_diap + larves_side + larves_exo)
    prop_side <- larves_side / (larves_pupe + larves_diap + larves_side + larves_exo)
    prop_exo  <- larves_exo  / (larves_pupe + larves_diap + larves_side + larves_exo)
    
    list(larves, prop_pupe * larves, prop_diap * larves,
         prop_side * larves, prop_exo * larves, females,
         female_pupe, female_diap, female_side, females_exo)
}



# Saisonnalité sur les inflos ---------------------------------------------

dynamics_season_inflos_b1 <- function(arg, inflos) {
    ## Renvoie la dynamique de larves pour les trois sous-blocs
    gamma <- arg[1]
    migration <- arg[2]
    mu_ER <- arg[3]
    mu_EH <- arg[4]
    inflo_capacity <- arg[5]
    stock <- arg[6]
    # stock <- 0
    reproduction <- arg[7]
    season <- arg[8]
    
    inflos[59:80, ] <- inflos[59:80, ] * season
    
    alpha <- exchange(migration, inflos)
    females_exo <- incoming(gamma, inflos)
    # females_exo <- matrix(30, nrow = 80, ncol = 3)
    larves <- matrix(0, nrow = nb_jours, ncol = 3)
    females_endo <- matrix(0, nrow = nb_jours, ncol = 3)
    females <- matrix(0, nrow = nb_jours, ncol = 3)
    mu_sol <- c(mu_ER, mu_PS, mu_EH)
    for (jour in 1:nb_jours) {
        larves[jour, ] <- larvaes_count(jour, inflo_capacity, inflos, females,
                                        mu_sol, reproduction)
        females_endo[jour, ] <- emerging(jour, larves, mu_sol, stock)
        females[jour, 1] <- females_count(jour, alpha[[1]],
                                          females_exo[, 1], females_endo)
        females[jour, 2] <- females_count(jour, alpha[[2]],
                                          females_exo[, 2], females_endo)
        females[jour, 3] <- females_count(jour, alpha[[3]],
                                          females_exo[, 3], females_endo)
        
    }
    
    larves
}


decomposition_season_inflos_bloc1 <- function(arg, inflos) {
    ## Décompose la provenance des femelles : pupaison, diapause,
    ## side ou exogène
    gamma <- arg[1]
    migration <- arg[2]
    mu_ER <- arg[3]
    mu_EH <- arg[4]
    inflo_capacity <- arg[5]
    stock <- arg[6]
    # stock <- 0
    reproduction <- arg[7]
    season <- arg[8]
    
    inflos[59:80, ] <- inflos[59:80, ] * season
    
    ## Modèle normal
    alpha <- exchange(migration, inflos)
    females_exo <- incoming(gamma, inflos)
    # females_exo <- matrix(30, nrow = 80, ncol = 3)
    larves <- matrix(0, nrow = nb_jours, ncol = 3)
    females_endo <- matrix(0, nrow = nb_jours, ncol = 3)
    females <- matrix(0, nrow = nb_jours, ncol = 3)
    mu_sol <- c(mu_ER, mu_PS, mu_EH)
    
    ## Décomposition
    prop_endo <- matrix(NA, nrow = nb_jours, ncol = 3)
    prop_exo <- matrix(NA, nrow = nb_jours, ncol = 3)
    prop_side <- matrix(NA, nrow = nb_jours, ncol = 3)
    larves_pupe <- matrix(NA, nrow = nb_jours, ncol = 3)
    larves_diap <- matrix(NA, nrow = nb_jours, ncol = 3)
    larves_exo <- matrix(NA, nrow = nb_jours, ncol = 3)
    larves_side <- matrix(NA, nrow = nb_jours, ncol = 3)
    female_side <- matrix(NA, nrow = nb_jours, ncol = 3)
    female_pupe <- matrix(NA, nrow = nb_jours, ncol = 3)
    female_diap <- matrix(NA, nrow = nb_jours, ncol = 3)
    for (jour in 1:nb_jours) {
        
        ## Modèle normal
        larves[jour, ] <- larvaes_count(jour, inflo_capacity, inflos, females,
                                        mu_sol, reproduction)
        females_endo[jour, ] <- emerging(jour, larves, mu_sol, stock)
        females[jour, 1] <- females_count(jour, alpha[[1]],
                                          females_exo[, 1], females_endo)
        females[jour, 2] <- females_count(jour, alpha[[2]],
                                          females_exo[, 2], females_endo)
        females[jour, 3] <- females_count(jour, alpha[[3]],
                                          females_exo[, 3], females_endo)
        
        
        ## Décomposition
        female_pupe[jour, 1] <- emerging(jour, larves, mu_sol, stock = 0)[1] * alpha[[1]][jour, 1]
        female_pupe[jour, 2] <- emerging(jour, larves, mu_sol, stock = 0)[2] * alpha[[2]][jour, 2]
        female_pupe[jour, 3] <- emerging(jour, larves, mu_sol, stock = 0)[3] * alpha[[3]][jour, 3]
        
        female_diap[jour, 1] <- emerging_diap(jour, larves, mu_sol, stock)[1] * alpha[[1]][jour, 1]
        female_diap[jour, 2] <- emerging_diap(jour, larves, mu_sol, stock)[2] * alpha[[2]][jour, 2]
        female_diap[jour, 3] <- emerging_diap(jour, larves, mu_sol, stock)[3] * alpha[[3]][jour, 3]
        
        female_side[jour, 1] <- females_side(jour, alpha[[1]], cbind(0, females_endo[, 2:3]))
        female_side[jour, 2] <- females_side(jour, alpha[[2]], 
                                             cbind(females_endo[, 1], 0, females_endo[, 3]))
        female_side[jour, 3] <- females_side(jour, alpha[[3]], cbind(females_endo[, 1:2], 0))
        
        larves_side[jour, ] <- larvaes_count2(jour, inflo_capacity, inflos, female_side, reproduction)
        larves_exo[jour, ]  <- larvaes_count2(jour, inflo_capacity, inflos, females_exo, reproduction)
        larves_pupe[jour, ] <- larvaes_count2(jour, inflo_capacity, inflos, female_pupe, reproduction)
        larves_diap[jour, ] <- larvaes_count2(jour, inflo_capacity, inflos, female_diap, reproduction)
        
    }
    prop_pupe <- larves_pupe / (larves_pupe + larves_diap + larves_side + larves_exo)
    prop_diap <- larves_diap / (larves_pupe + larves_diap + larves_side + larves_exo)
    prop_side <- larves_side / (larves_pupe + larves_diap + larves_side + larves_exo)
    prop_exo  <- larves_exo  / (larves_pupe + larves_diap + larves_side + larves_exo)
    
    list(larves, prop_pupe * larves, prop_diap * larves,
         prop_side * larves, prop_exo * larves, females,
         female_pupe, female_diap, female_side, females_exo)
}

decomposition_season_inflos_bloc2 <- function(arg, inflos) {
    ## Décompose la provenance des femelles : pupaison, diapause,
    ## side ou exogène
    gamma <- arg[1]
    migration <- arg[2]
    mu_ER <- arg[3]
    mu_EH <- arg[4]
    inflo_capacity <- arg[5]
    # stock <- arg[6]
    stock <- 0
    reproduction <- arg[7]
    season <- arg[8]
    
    inflos[36:80, 2:3] <- inflos[36:80, 2:3] * season + 1
    inflos[51:80, 1] <- inflos[51:80, 1] * season + 1
    
    ## Modèle normal
    alpha <- exchange(migration, inflos)
    females_exo <- incoming(gamma, inflos)
    # females_exo <- matrix(30, nrow = 80, ncol = 3)
    larves <- matrix(0, nrow = nb_jours, ncol = 3)
    females_endo <- matrix(0, nrow = nb_jours, ncol = 3)
    females <- matrix(0, nrow = nb_jours, ncol = 3)
    mu_sol <- c(mu_ER, mu_PS, mu_EH)
    
    ## Décomposition
    prop_endo <- matrix(NA, nrow = nb_jours, ncol = 3)
    prop_exo <- matrix(NA, nrow = nb_jours, ncol = 3)
    prop_side <- matrix(NA, nrow = nb_jours, ncol = 3)
    larves_pupe <- matrix(NA, nrow = nb_jours, ncol = 3)
    larves_diap <- matrix(NA, nrow = nb_jours, ncol = 3)
    larves_exo <- matrix(NA, nrow = nb_jours, ncol = 3)
    larves_side <- matrix(NA, nrow = nb_jours, ncol = 3)
    female_side <- matrix(NA, nrow = nb_jours, ncol = 3)
    female_pupe <- matrix(NA, nrow = nb_jours, ncol = 3)
    female_diap <- matrix(NA, nrow = nb_jours, ncol = 3)
    for (jour in 1:nb_jours) {
        
        ## Modèle normal
        larves[jour, ] <- larvaes_count(jour, inflo_capacity, inflos, females,
                                        mu_sol, reproduction)
        females_endo[jour, ] <- emerging(jour, larves, mu_sol, stock)
        females[jour, 1] <- females_count(jour, alpha[[1]],
                                          females_exo[, 1], females_endo)
        females[jour, 2] <- females_count(jour, alpha[[2]],
                                          females_exo[, 2], females_endo)
        females[jour, 3] <- females_count(jour, alpha[[3]],
                                          females_exo[, 3], females_endo)
        
        
        ## Décomposition
        female_pupe[jour, 1] <- emerging(jour, larves, mu_sol, stock = 0)[1] * alpha[[1]][jour, 1]
        female_pupe[jour, 2] <- emerging(jour, larves, mu_sol, stock = 0)[2] * alpha[[2]][jour, 2]
        female_pupe[jour, 3] <- emerging(jour, larves, mu_sol, stock = 0)[3] * alpha[[3]][jour, 3]
        
        female_diap[jour, 1] <- emerging_diap(jour, larves, mu_sol, stock)[1] * alpha[[1]][jour, 1]
        female_diap[jour, 2] <- emerging_diap(jour, larves, mu_sol, stock)[2] * alpha[[2]][jour, 2]
        female_diap[jour, 3] <- emerging_diap(jour, larves, mu_sol, stock)[3] * alpha[[3]][jour, 3]
        
        female_side[jour, 1] <- females_side(jour, alpha[[1]], cbind(0, females_endo[, 2:3]))
        female_side[jour, 2] <- females_side(jour, alpha[[2]], 
                                             cbind(females_endo[, 1], 0, females_endo[, 3]))
        female_side[jour, 3] <- females_side(jour, alpha[[3]], cbind(females_endo[, 1:2], 0))
        
        larves_side[jour, ] <- larvaes_count2(jour, inflo_capacity, inflos, female_side, reproduction)
        larves_exo[jour, ]  <- larvaes_count2(jour, inflo_capacity, inflos, females_exo, reproduction)
        larves_pupe[jour, ] <- larvaes_count2(jour, inflo_capacity, inflos, female_pupe, reproduction)
        larves_diap[jour, ] <- larvaes_count2(jour, inflo_capacity, inflos, female_diap, reproduction)
        
    }
    prop_pupe <- larves_pupe / (larves_pupe + larves_diap + larves_side + larves_exo)
    prop_diap <- larves_diap / (larves_pupe + larves_diap + larves_side + larves_exo)
    prop_side <- larves_side / (larves_pupe + larves_diap + larves_side + larves_exo)
    prop_exo  <- larves_exo  / (larves_pupe + larves_diap + larves_side + larves_exo)
    
    list(larves, prop_pupe * larves, prop_diap * larves,
         prop_side * larves, prop_exo * larves, females,
         female_pupe, female_diap, female_side, females_exo)
}
