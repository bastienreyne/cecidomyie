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

# Fonctions ---------------------------------------------------------------

incoming <- function(gamma, inflos) {
    ## Individus exogene
    gamma * inflos
}

exchange <- function(param_migration, inflos) {
    ## Échange entre les sous-blocs du bloc 1 [ER|PS|EH]
    ## Les matrices alpha représente les individus VENANT dans le sous-bloc
    ## Par exemple, alphaER[, 3] désigne les individus allant de EH dans ER
    alphaER <- alphaPS <- alphaEH <- matrix(0, nrow = nb_jours, ncol = 3)
    
    alphaER[, 1] <- inflos[, 1] / (inflos[, 1] + inflos[, 2] * param_migration +
                                       inflos[, 3] * param_migration^2)
    alphaPS[, 2] <- inflos[, 2] / (inflos[, 2] + inflos[, 1] * param_migration +
                                       inflos[, 3] * param_migration)
    alphaEH[, 3] <- inflos[, 3] / (inflos[, 3] + inflos[, 2] * param_migration +
                                       inflos[, 1] * param_migration^2)
    
    alphaER[, 2] <- param_migration * inflos[, 1] / 
        (inflos[, 2] + inflos[, 1] * param_migration + inflos[, 3] * param_migration)
    alphaEH[, 2] <- param_migration * inflos[, 3] / 
        (inflos[, 2] + inflos[, 1] * param_migration + inflos[, 3] * param_migration)
    alphaPS[, 1] <- param_migration * inflos[, 2] /
        (inflos[, 1] + inflos[, 2] * param_migration + inflos[, 3] * param_migration^2)
    alphaPS[, 3] <- param_migration * inflos[, 2] /
        (inflos[, 3] + inflos[, 2] * param_migration + inflos[, 1] * param_migration^2)
    
    alphaER[, 3] <- param_migration^2 * inflos[, 1] /
        (inflos[, 3] + inflos[, 2] * param_migration + inflos[, 1] * param_migration^2)
    alphaEH[, 1] <- param_migration^2 * inflos[, 3] /
        (inflos[, 1] + inflos[, 2] * param_migration + inflos[, 3] * param_migration^2)
    
    
    list(alphaER, alphaPS, alphaEH)
}

exchange_b2 <- function(param_migration, inflos) {
    ## Échange entre les sous-blocs du bloc 2 [ER|EH|PS]
    ## Les matrices alpha représente les individus VENANT dans le sous-bloc
    ## Par exemple, alphaER[, 3] désigne les individus allant de EH dans ER
    alphaER <- alphaPS <- alphaEH <- matrix(0, nrow = nb_jours, ncol = 3)
    
    alphaER[, 1] <- inflos[, 1] / (inflos[, 1] + inflos[, 3] * param_migration +
                                       inflos[, 2] * param_migration^2)
    alphaPS[, 2] <- inflos[, 2] / (inflos[, 2] + inflos[, 1] * param_migration^2 +
                                       inflos[, 3] * param_migration)
    alphaEH[, 3] <- inflos[, 3] / (inflos[, 3] + inflos[, 2] * param_migration +
                                       inflos[, 1] * param_migration)
    
    alphaER[, 2] <- param_migration^2 * inflos[, 1] / 
        (inflos[, 2] + inflos[, 1] * param_migration^2 + inflos[, 3] * param_migration)
    alphaEH[, 2] <- param_migration * inflos[, 3] / 
        (inflos[, 2] + inflos[, 1] * param_migration^2 + inflos[, 3] * param_migration)
    alphaPS[, 1] <- param_migration^2 * inflos[, 2] /
        (inflos[, 1] + inflos[, 2] * param_migration^2 + inflos[, 3] * param_migration)
    alphaPS[, 3] <- param_migration * inflos[, 2] /
        (inflos[, 3] + inflos[, 2] * param_migration + inflos[, 1] * param_migration)
    
    alphaER[, 3] <- param_migration * inflos[, 1] /
        (inflos[, 3] + inflos[, 2] * param_migration + inflos[, 1] * param_migration)
    alphaEH[, 1] <- param_migration * inflos[, 3] /
        (inflos[, 1] + inflos[, 2] * param_migration^2 + inflos[, 3] * param_migration)
    
    
    list(alphaER, alphaPS, alphaEH)
}

disponibility <- function(day, inflo_capacity, inflos, females) {
    ## Disponibilité des ressources
    ## Coefficient R dans le modèle
    ans <- rep(NA, 3)
    ans[females[day, ] <= inflo_capacity * inflos[day, ]] <- 1
    ans[females[day, ] > inflo_capacity * inflos[day, ]] <- inflo_capacity *
        inflos[day, which(females[day, ] > inflo_capacity * inflos[day, ])] /
        females[day, which(females[day, ] > inflo_capacity * inflos[day, ])]
    
    ans
}

larvaes_count <- function(day, inflo_capacity, inflos, females, mu_sol, reproduction) {
    ## Nombre de larves qui s'éjectent des inflorescences
    beta7 <-  0
    beta8 <-  0
    beta9 <-  0
    beta10 <- 1
    beta11 <- 0
    beta12 <- 0
    
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

emerging <- function(day, larves, mu_sol, stock) {
    ## Renvoie le nombre de femelles qui émergent pour un jour donné.
    ## Individus en pupaison et en diapause confondus
    if (day <= 5)
        c(0, 0, 0)
    else
        ((larves[day - pupe_duration, ] * mu_sol) * 0.77 +
             sortie_diapause2017[day] * stock) * mu_sol * sex_ratio
}

females_count <- function(day, alpha, females_exo, females_endo) {
    ## Nombre total de femelles chaque jour
    females_exo[day] + alpha[day, ] %*% females_endo[day, ]
}

dynamics <- function(arg, inflos) {
    ## Renvoie la dynamique de larves pour les trois sous-blocs
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

dynamics_b2 <- function(arg, inflos) {
    ## Renvoie la dynamique de larves pour les trois sous-blocs
    gamma <- arg[1]
    migration <- arg[2]
    mu_ER <- arg[3]
    mu_EH <- arg[4]
    inflo_capacity <- arg[5]
    stock <- arg[6]
    reproduction <- arg[7]
    
    alpha <- exchange_b2(migration, inflos)
    # females_exo <- incoming(gamma, inflos)
    females_exo <- matrix(30, nrow = 80, ncol = 3)
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

critere <- function(arg, inflos) {
    ## Valeur du critère de minimisation des exogènes.
    gamma <- arg[1]
    migration <- arg[2]
    mu_ER <- arg[3]
    mu_EH <- arg[4]
    inflo_capacity <- arg[5]
    stock <- arg[6]
    reproduction <- arg[7]
    
    alpha <- exchange(migration, inflos)
    females_exo <- incoming(gamma, inflos)
    # females_exo <- matrix(20, nrow = 80, ncol = 3)
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
    
    - sum((females - females_exo) / females)
}

larvaes_count2 <- function(day, inflo_capacity, inflos, females, reproduction) {
    ## Nombre de larves chaque jour sans limitation de ressources                    
    beta7 <-  0
    beta8 <-  0
    beta9 <-  0
    beta10 <- 1
    beta11 <- 0
    beta12 <- 0
    
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

decomposition <- function(arg, inflos) {
    ## Décompose la provenance des femelles : pupaison, diapause,
    ## side ou exogène
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

decomposition_b2 <- function(arg, inflos) {
    ## Décompose la provenance des femelles : pupaison, diapause,
    ## side ou exogène
    gamma <- arg[1]
    migration <- arg[2]
    mu_ER <- arg[3]
    mu_EH <- arg[4]
    inflo_capacity <- arg[5]
    stock <- arg[6]
    reproduction <- arg[7]
    
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


dynamics_season_b1 <- function(arg, inflos) {
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

dynamics_season_b2 <- function(arg, inflos) {
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
    }
    
    larves
}

decomposition_season_bloc1 <- function(arg, inflos) {
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
    alpha <- exchange(migration, inflos)
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
    alpha <- exchange(migration, inflos)
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

# # Old ---------------------------------------------------------------------
# 
# ## Paramètres floraison
# moyenne <- data.frame(mean_ER = 31.97826, mean_PS = 28.65, mean_EH = 36.25301)
# stddev <- data.frame(sd_ER = 11.47071, sd_PS = 13.35809, sd_EH = 15.01044)
# inflo_attractive <- function(bursts, sousbloc, delta_t) {
#     
#     mean_hat <- moyenne %>% select(ends_with(sousbloc)) %>% pull
#     sd_hat <- stddev %>% select(ends_with(sousbloc)) %>% pull
#     FdR <- pnorm(1:50, mean_hat - delta_t, sd_hat)
#     
#     # browser()
#     inflo <- rep(NA, 80)
#     inflo[1] <- bursts[1]
#     for (t in 2:80) {
#         inflo[t] <- bursts[t] + sum(bursts[(t-1):max(1, t - 50)] *
#                                         (1 - FdR[1:min(50, t-1)]))
#     }
#     
#     inflo
# }
# 
# 
# decomposition <- function(arg, inflos) {
#     ## Calcule le nombre de larves (inch'allah)
#     gamma <- arg[1]
#     proba_migration <- arg[2]
#     mu_ER <- arg[3]
#     mu_EH <- arg[4]
#     inflo_capacity <- arg[5]
#     
#     alpha <- exchange(proba_migration, inflos)
#     females_exo <- incoming(gamma, inflos)
#     # females_exo <- matrix(20, nrow = 80, ncol = 3)
#     larves <- matrix(0, nrow = nb_jours, ncol = 3)
#     females_endo <- matrix(0, nrow = nb_jours, ncol = 3)
#     females <- matrix(0, nrow = nb_jours, ncol = 3)
#     mu_sol <- c(mu_ER, mu_PS, mu_EH)
#     prop_endo <- matrix(NA, nrow = nb_jours, ncol = 3)
#     prop_exo <- matrix(NA, nrow = nb_jours, ncol = 3)
#     prop_side <- matrix(NA, nrow = nb_jours, ncol = 3)
#     larves_endo <- matrix(NA, nrow = nb_jours, ncol = 3)
#     larves_exo <- matrix(NA, nrow = nb_jours, ncol = 3)
#     larves_side <- matrix(NA, nrow = nb_jours, ncol = 3)
#     female_side <- matrix(NA, nrow = nb_jours, ncol = 3)
#     for (jour in 1:nb_jours) {
#         larves[jour, ] <- larvaes_count(jour, inflo_capacity, inflos, females)
#         females_endo[jour, ] <- emerging(jour, larves, mu_sol)
#         
#         larves_exo[jour, ] <- larvaes_count2(jour, inflo_capacity, inflos, females_exo)
#         larves_endo[jour, ] <- larvaes_count2(jour, inflo_capacity, inflos,
#                                               females_endo * (1 - proba_migration))
#         
#         female_side[jour, 1] <- females_side(jour, alpha[[1]], cbind(0, females_endo[, 2:3]))
#         female_side[jour, 2] <- females_side(jour, alpha[[2]], 
#                                              cbind(females_endo[, 1], 0, females_endo[, 3]))
#         female_side[jour, 3] <- females_side(jour, alpha[[3]], cbind(females_endo[, 1:2], 0))
#         
#         larves_side[jour, ] <- larvaes_count2(jour, inflo_capacity, inflos, female_side)
#         
#         females[jour, 1] <- females_count(jour, alpha[[1]], females_exo[, 1], females_endo)
#         females[jour, 2] <- females_count(jour, alpha[[2]], females_exo[, 2], females_endo)
#         females[jour, 3] <- females_count(jour, alpha[[3]], females_exo[, 3], females_endo)
#     }
#     prop_endo <- larves_endo / (larves_endo + larves_side + larves_exo)
#     prop_side <- larves_side / (larves_endo + larves_side + larves_exo)
#     prop_exo <- larves_exo / (larves_endo + larves_side + larves_exo)
#     
#     list(larves, prop_exo * larves, prop_endo * larves, prop_side * larves)
# }
# 
# 
# females_count3 <- function(day, alpha, females_exo, females_endo, saison_end) {
#     ## Nombre total de femelles
#     if (day <= 59)
#         return(females_exo[day] + alpha[day, ] %*% females_endo[day, ])
#     if (day > 59)
#         return((females_exo[day] + alpha[day, ] %*% females_endo[day, ]) * saison_end)
# }
# 
# dynamics <- function(arg, inflos) {
#     ## Calcule le nombre de larves (inch'allah)
#     gamma <- arg[1]
#     proba_migration <- arg[2]
#     mu_ER <- arg[3]
#     mu_EH <- arg[4]
#     inflo_capacity <- arg[5]
#     
#     
#     alpha <- exchange(proba_migration, inflos)
#     females_exo <- incoming(gamma, inflos)
#     # females_exo <- matrix(20, nrow = 80, ncol = 3)
#     larves <- matrix(0, nrow = nb_jours, ncol = 3)
#     females_endo <- matrix(0, nrow = nb_jours, ncol = 3)
#     females <- matrix(0, nrow = nb_jours, ncol = 3)
#     mu_sol <- c(mu_ER, mu_PS, mu_EH)
#     for (jour in 1:nb_jours) {
#         larves[jour, ] <- larvaes_count(jour, inflo_capacity, inflos, females)
#         females_endo[jour, ] <- emerging(jour, larves, mu_sol)
#         females[jour, 1] <- females_count(jour, alpha[[1]], 
#                                           females_exo[, 1], females_endo)
#         females[jour, 2] <- females_count(jour, alpha[[2]],
#                                           females_exo[, 2], females_endo)
#         females[jour, 3] <- females_count(jour, alpha[[3]],
#                                           females_exo[, 3], females_endo)
#     }
#     
#     larves
# }
# 
# 
# disponibility2 <- function(day, inflo_capacity, inflos, females) {
#     ## Disponibilité des ressources
#     ans <- rep(NA, 3)
#     ans[females[day, ] <= inflos[day, ]] <- 1
#     ans[females[day, ] > inflos[day, ]] <-
#         inflos[day, which(females[day, ] > inflos[day, ])] /
#         females[day, which(females[day, ] > inflos[day, ])]
#     
#     ans
# }
# 
# 
# # Test modif proc
# exchange3 <- function(leaving_to_ps, inflos) {
#     ## Echange entre les sous-blocs
#     alpha_all <- cbind(inflos[, 1] / (inflos[, 1] + inflos[, 2] + inflos[, 3]),
#                        inflos[, 2] / (inflos[, 1] + inflos[, 2] + inflos[, 3]),
#                        inflos[, 3] / (inflos[, 1] + inflos[, 2] + inflos[, 3]))
#     alphaER <- alphaPS <- alphaEH <- matrix(1/3, nrow = 80, ncol = 3)
#     
#     return(list(alphaER, alphaPS, alphaEH))
# }
# 
# 
# exchange <- function(proba_migration, inflos) {
#     ## Echange entre les sous-blocs
#     alphaER <- alphaB <- alphaEH <- matrix(0, nrow = nb_jours, ncol = 3)
#     alphaER[, 1] <- alphaEH[, 3] <- 1 - proba_migration
#     alphaER[, 3] <- proba_migration * inflos[, 1] / (inflos[, 2] + inflos[, 1])
#     alphaEH[, 1] <- proba_migration * inflos[, 3] / (inflos[, 2] + inflos[, 3])
#     alphaB[, 1] <- proba_migration * inflos[, 2] / (inflos[, 2] + inflos[, 3])
#     alphaB[, 3] <- proba_migration * inflos[, 2] / (inflos[, 2] + inflos[, 1])
#     
#     return(list(alphaER, alphaB, alphaEH))
# }
# 
# 
# dynamics_isa <- function(arg, inflos) {
#     ## Calcule le nombre de larves (inch'allah)
#     gamma <- arg[1]
#     leaving <- arg[2]
#     mu_ER <- arg[3]
#     mu_EH <- arg[4]
#     inflo_capacity <- arg[5]
#     
#     alpha <- exchange_isa(leaving, inflos)
#     females_exo <- incoming(gamma, inflos)
#     # females_exo <- matrix(20, nrow = 80, ncol = 3)
#     larves <- matrix(0, nrow = nb_jours, ncol = 3)
#     females_endo <- matrix(0, nrow = nb_jours, ncol = 3)
#     females <- matrix(0, nrow = nb_jours, ncol = 3)
#     mu_sol <- c(mu_ER, mu_PS, mu_EH)
#     for (jour in 1:nb_jours) {
#         larves[jour, ] <- larvaes_count(jour, inflo_capacity, inflos, females)
#         females_endo[jour, ] <- emerging(jour, larves, mu_sol)
#         females[jour, 1] <- females_count(jour, alpha[[1]],
#                                           females_exo[, 1], females_endo)
#         females[jour, 2] <- females_count(jour, alpha[[2]],
#                                           females_exo[, 2], females_endo)
#         females[jour, 3] <- females_count(jour, alpha[[3]],
#                                           females_exo[, 3], females_endo)
#     }
#     
#     larves
# }
# 
# dynamics_fred <- function(arg, inflos) {
#     ## Calcule le nombre de larves (inch'allah)
#     gamma <- arg[1]
#     leaving <- arg[2]
#     mu_ER <- arg[3]
#     mu_EH <- arg[4]
#     inflo_capacity <- arg[5]
#     
#     alpha <- exchange_fred(leaving, inflos)
#     females_exo <- incoming(gamma, inflos)
#     # females_exo <- matrix(20, nrow = 80, ncol = 3)
#     larves <- matrix(0, nrow = nb_jours, ncol = 3)
#     females_endo <- matrix(0, nrow = nb_jours, ncol = 3)
#     females <- matrix(0, nrow = nb_jours, ncol = 3)
#     mu_sol <- c(mu_ER, mu_PS, mu_EH)
#     for (jour in 1:nb_jours) {
#         larves[jour, ] <- larvaes_count(jour, inflo_capacity, inflos, females)
#         females_endo[jour, ] <- emerging(jour, larves, mu_sol)
#         females[jour, 1] <- females_count(jour, alpha[[1]],
#                                           females_exo[, 1], females_endo)
#         females[jour, 2] <- females_count(jour, alpha[[2]],
#                                           females_exo[, 2], females_endo)
#         females[jour, 3] <- females_count(jour, alpha[[3]],
#                                           females_exo[, 3], females_endo)
#     }
#     
#     larves
# }
# 
# 
# decomposition_isa <- function(arg, inflos) {
#     ## Calcule le nombre de larves (inch'allah)
#     gamma <- arg[1]
#     leaving <- arg[2]
#     mu_ER <- arg[3]
#     mu_EH <- arg[4]
#     inflo_capacity <- arg[5]
#     
#     alpha <- exchange_isa(leaving, inflos)
#     females_exo <- incoming(gamma, inflos)
#     # females_exo <- matrix(20, nrow = 80, ncol = 3)
#     larves <- matrix(0, nrow = nb_jours, ncol = 3)
#     females_endo <- matrix(0, nrow = nb_jours, ncol = 3)
#     females <- matrix(0, nrow = nb_jours, ncol = 3)
#     mu_sol <- c(mu_ER, mu_PS, mu_EH)
#     prop_endo <- matrix(NA, nrow = nb_jours, ncol = 3)
#     prop_exo <- matrix(NA, nrow = nb_jours, ncol = 3)
#     prop_side <- matrix(NA, nrow = nb_jours, ncol = 3)
#     larves_endo <- matrix(NA, nrow = nb_jours, ncol = 3)
#     larves_exo <- matrix(NA, nrow = nb_jours, ncol = 3)
#     larves_side <- matrix(NA, nrow = nb_jours, ncol = 3)
#     female_side <- matrix(NA, nrow = nb_jours, ncol = 3)
#     female_endo <- matrix(NA, nrow = nb_jours, ncol = 3)
#     for (jour in 1:nb_jours) {
#         larves[jour, ] <- larvaes_count(jour, inflo_capacity, inflos, females)
#         females_endo[jour, ] <- emerging(jour, larves, mu_sol)
#         
#         female_endo[jour, 1] <- females_side(jour, alpha[[1]], cbind(females_endo[, 1], 0, 0))
#         female_endo[jour, 2] <- females_side(jour, alpha[[2]], matrix(0, nrow = nb_jours, ncol = 3))
#         female_endo[jour, 3] <- females_side(jour, alpha[[3]], cbind(0, 0, females_endo[, 3]))
#         
#         female_side[jour, 1] <- females_side(jour, alpha[[1]], cbind(0, females_endo[, 2:3]))
#         female_side[jour, 2] <- females_side(jour, alpha[[2]], 
#                                              cbind(females_endo[, 1], 0, females_endo[, 3]))
#         female_side[jour, 3] <- females_side(jour, alpha[[3]], cbind(females_endo[, 1:2], 0))
#         
#         larves_side[jour, ] <- larvaes_count2(jour, inflo_capacity, inflos, female_side)
#         larves_exo[jour, ] <- larvaes_count2(jour, inflo_capacity, inflos, females_exo)
#         larves_endo[jour, ] <- larvaes_count2(jour, inflo_capacity, inflos, female_endo)
#         
#         females[jour, 1] <- females_count(jour, alpha[[1]], females_exo[, 1], females_endo)
#         females[jour, 2] <- females_count(jour, alpha[[2]], females_exo[, 2], females_endo)
#         females[jour, 3] <- females_count(jour, alpha[[3]], females_exo[, 3], females_endo)
#     }
#     prop_endo <- larves_endo / (larves_endo + larves_side + larves_exo)
#     prop_side <- larves_side / (larves_endo + larves_side + larves_exo)
#     prop_exo <- larves_exo / (larves_endo + larves_side + larves_exo)
#     
#     list(larves, prop_exo * larves, prop_endo * larves, prop_side * larves)
# }
# 
# 
# decomposition_fred <- function(arg, inflos) {
#     ## Calcule le nombre de larves (inch'allah)
#     gamma <- arg[1]
#     leaving <- arg[2]
#     mu_ER <- arg[3]
#     mu_EH <- arg[4]
#     inflo_capacity <- arg[5]
#     
#     alpha <- exchange_fred(leaving, inflos)
#     females_exo <- incoming(gamma, inflos)
#     # females_exo <- matrix(20, nrow = 80, ncol = 3)
#     larves <- matrix(0, nrow = nb_jours, ncol = 3)
#     females_endo <- matrix(0, nrow = nb_jours, ncol = 3)
#     females <- matrix(0, nrow = nb_jours, ncol = 3)
#     mu_sol <- c(mu_ER, mu_PS, mu_EH)
#     prop_endo <- matrix(NA, nrow = nb_jours, ncol = 3)
#     prop_exo <- matrix(NA, nrow = nb_jours, ncol = 3)
#     prop_side <- matrix(NA, nrow = nb_jours, ncol = 3)
#     larves_endo <- matrix(NA, nrow = nb_jours, ncol = 3)
#     larves_exo <- matrix(NA, nrow = nb_jours, ncol = 3)
#     larves_side <- matrix(NA, nrow = nb_jours, ncol = 3)
#     female_side <- matrix(NA, nrow = nb_jours, ncol = 3)
#     female_endo <- matrix(NA, nrow = nb_jours, ncol = 3)
#     for (jour in 1:nb_jours) {
#         larves[jour, ] <- larvaes_count(jour, inflo_capacity, inflos, females)
#         females_endo[jour, ] <- emerging(jour, larves, mu_sol)
#         
#         female_endo[jour, 1] <- females_side(jour, alpha[[1]], cbind(females_endo[, 1], 0, 0))
#         female_endo[jour, 2] <- females_side(jour, alpha[[2]], matrix(0, nrow = nb_jours, ncol = 3))
#         female_endo[jour, 3] <- females_side(jour, alpha[[3]], cbind(0, 0, females_endo[, 3]))
#         
#         female_side[jour, 1] <- females_side(jour, alpha[[1]], cbind(0, females_endo[, 2:3]))
#         female_side[jour, 2] <- females_side(jour, alpha[[2]], 
#                                              cbind(females_endo[, 1], 0, females_endo[, 3]))
#         female_side[jour, 3] <- females_side(jour, alpha[[3]], cbind(females_endo[, 1:2], 0))
#         
#         larves_side[jour, ] <- larvaes_count2(jour, inflo_capacity, inflos, female_side)
#         larves_exo[jour, ] <- larvaes_count2(jour, inflo_capacity, inflos, females_exo)
#         larves_endo[jour, ] <- larvaes_count2(jour, inflo_capacity, inflos, female_endo)
#         
#         females[jour, 1] <- females_count(jour, alpha[[1]], females_exo[, 1], females_endo)
#         females[jour, 2] <- females_count(jour, alpha[[2]], females_exo[, 2], females_endo)
#         females[jour, 3] <- females_count(jour, alpha[[3]], females_exo[, 3], females_endo)
#     }
#     prop_endo <- larves_endo / (larves_endo + larves_side + larves_exo)
#     prop_side <- larves_side / (larves_endo + larves_side + larves_exo)
#     prop_exo <- larves_exo / (larves_endo + larves_side + larves_exo)
#     
#     list(larves, prop_exo * larves, prop_endo * larves, prop_side * larves)
# }
# 
# 
# dynamics21j <- function(arg, inflos) {
#     ## Calcule le nombre de larves (inch'allah)
#     gamma <- arg[1]
#     leaving <- arg[2]
#     mu_ER <- arg[3]
#     mu_EH <- arg[4]
#     inflo_capacity <- arg[5]
#     
#     alpha <- exchange2(leaving, inflos)
#     females_exo <- incoming(gamma, inflos)
#     # females_exo <- matrix(20, nrow = 80, ncol = 3)
#     larves <- matrix(0, nrow = nb_jours, ncol = 3)
#     females_endo <- matrix(0, nrow = nb_jours, ncol = 3)
#     females <- matrix(0, nrow = nb_jours, ncol = 3)
#     mu_sol <- c(mu_ER, mu_PS, mu_EH)
#     for (jour in 1:nb_jours) {
#         larves[jour, ] <- larvaes_count(jour, inflo_capacity, inflos, females)
#         females_endo[jour, ] <- emerging1j(jour, larves, mu_sol)
#         females[jour, 1] <- females_count(jour, alpha[[1]],
#                                           females_exo[, 1], females_endo)
#         females[jour, 2] <- females_count(jour, alpha[[2]],
#                                           females_exo[, 2], females_endo)
#         females[jour, 3] <- females_count(jour, alpha[[3]],
#                                           females_exo[, 3], females_endo)
#     }
#     
#     larves
# }
# 
# dynamics215j <- function(arg, inflos) {
#     ## Calcule le nombre de larves (inch'allah)
#     gamma <- arg[1]
#     leaving <- arg[2]
#     mu_ER <- arg[3]
#     mu_EH <- arg[4]
#     inflo_capacity <- arg[5]
#     
#     alpha <- exchange2(leaving, inflos)
#     females_exo <- incoming(gamma, inflos)
#     # females_exo <- matrix(20, nrow = 80, ncol = 3)
#     larves <- matrix(0, nrow = nb_jours, ncol = 3)
#     females_endo <- matrix(0, nrow = nb_jours, ncol = 3)
#     females <- matrix(0, nrow = nb_jours, ncol = 3)
#     mu_sol <- c(mu_ER, mu_PS, mu_EH)
#     for (jour in 1:nb_jours) {
#         larves[jour, ] <- larvaes_count(jour, inflo_capacity, inflos, females)
#         females_endo[jour, ] <- emerging15j(jour, larves, mu_sol)
#         females[jour, 1] <- females_count(jour, alpha[[1]],
#                                           females_exo[, 1], females_endo)
#         females[jour, 2] <- females_count(jour, alpha[[2]],
#                                           females_exo[, 2], females_endo)
#         females[jour, 3] <- females_count(jour, alpha[[3]],
#                                           females_exo[, 3], females_endo)
#     }
#     
#     larves
# }


dynamics_ressources <- function(arg, inflos) {
    ## Renvoie la dynamique de larves pour les trois sous-blocs
    gamma <- arg[1]
    migration <- arg[2]
    mu_ER <- arg[3]
    mu_EH <- arg[4]
    stock <- arg[5]
    reproduction <- arg[6]
    k_er <- arg[7:86]
    k_ps <- arg[87:166]
    k_eh <- arg[167:246]
    inflo_capacity <- cbind(k_er, k_ps, k_eh)
    
    alpha <- exchange(migration, inflos)
    females_exo <- incoming(gamma, inflos)
    # females_exo <- matrix(20, nrow = 80, ncol = 3)
    larves <- matrix(0, nrow = nb_jours, ncol = 3)
    females_endo <- matrix(0, nrow = nb_jours, ncol = 3)
    females <- matrix(0, nrow = nb_jours, ncol = 3)
    mu_sol <- c(mu_ER, mu_PS, mu_EH)
    for (jour in 1:nb_jours) {
        larves[jour, ] <- larvaes_count_ressources(jour, inflo_capacity, inflos, females,
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

larvaes_count_ressources <- function(day, inflo_capacity, inflos, females, mu_sol, reproduction) {
    ## Nombre de larves qui s'éjectent des inflorescences
    beta7 <-  0.025
    beta8 <-  0.075
    beta9 <-  0.400
    beta10 <- 0.400
    beta11 <- 0.075
    beta12 <- 0.025
    
    larvae7 <- larvae8 <- larvae9 <- larvae10 <- larvae11 <- larvae12 <- 0
    if (day > 7) {
        R <- inflo_capacity[day - 7, ]
        larvae7 <- females[day - 7, ] * R * reproduction
    }
    
    if (day > 8) {
        R <- inflo_capacity[day - 8, ]
        larvae8 <- females[day - 8, ] * R * reproduction
    }
    
    if (day > 9) {
        R <- inflo_capacity[day - 9, ]
        larvae9 <- females[day - 9, ] * R * reproduction
    }
    
    if (day > 10) {
        R <- inflo_capacity[day - 10, ]
        larvae10 <- females[day - 10, ] * R * reproduction
    }
    
    if (day > 11) {
        R <- inflo_capacity[day - 11, ]
        larvae11 <- females[day - 11, ] * R * reproduction
    }
    
    if (day > 12) {
        R <- inflo_capacity[day - 12, ]
        larvae12 <- females[day - 12, ] * R * reproduction
    }
    
    
    beta7 * larvae7 + beta8 * larvae8 + beta9 * larvae9 +
        beta10 * larvae10 + beta11 * larvae11 + beta12 * larvae12
}

femelles_ressources <- function(arg, inflos) {
    ## Renvoie la dynamique de larves pour les trois sous-blocs
    gamma <- arg[1]
    migration <- arg[2]
    mu_ER <- arg[3]
    mu_EH <- arg[4]
    inflo_capacity <- arg[5]
    stock <- arg[6]
    reproduction <- arg[7]
    k_er <- arg[8:87]
    k_ps <- arg[88:167]
    k_eh <- arg[168:247]
    inflo_capacity <- cbind(k_er, k_ps, k_eh)
    
    alpha <- exchange(migration, inflos)
    females_exo <- incoming(gamma, inflos)
    # females_exo <- matrix(20, nrow = 80, ncol = 3)
    larves <- matrix(0, nrow = nb_jours, ncol = 3)
    females_endo <- matrix(0, nrow = nb_jours, ncol = 3)
    females <- matrix(0, nrow = nb_jours, ncol = 3)
    mu_sol <- c(mu_ER, mu_PS, mu_EH)
    for (jour in 1:nb_jours) {
        larves[jour, ] <- larvaes_count_ressources(jour, inflo_capacity, inflos, females,
                                                   mu_sol, reproduction)
        females_endo[jour, ] <- emerging(jour, larves, mu_sol, stock)
        females[jour, 1] <- females_count(jour, alpha[[1]],
                                          females_exo[, 1], females_endo)
        females[jour, 2] <- females_count(jour, alpha[[2]],
                                          females_exo[, 2], females_endo)
        females[jour, 3] <- females_count(jour, alpha[[3]],
                                          females_exo[, 3], females_endo)
    }
    
    females
}
