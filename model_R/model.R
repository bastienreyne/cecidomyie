## Script implémentant le sous-modèle 1

## Paramètres fixes
sex_ratio <- 0.5
mu_PS <- 0
proba_pupe <- 0.77
pupe_duration <- 5
eggs <- 150
mu_larvation <- 0.04
nb_jours <- 80

## Fonctions du modèle

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
    beta9 <-  0.4
    beta10 <- 0.4
    beta11 <- 0.075
    beta12 <- 0.025
    
    larvae7 <- larvae8 <- larvae9 <- larvae10 <- larvae11 <- larvae12 <- 0
    if (day > 7) {
        R <- disponibility(day - 7, inflo_capacity, inflos, females)
        larvae7 <- females[day - 7, ] * R * eggs * mu_larvation
    }
    
    if (day > 8) {
        R <- disponibility(day - 8, inflo_capacity, inflos, females)
        larvae8 <- females[day - 8, ] * R * eggs * mu_larvation
    }
    
    if (day > 9) {
        R <- disponibility(day - 9, inflo_capacity, inflos, females)
        larvae9 <- females[day - 9, ] * R * eggs * mu_larvation
    }
    
    if (day > 10) {
        R <- disponibility(day - 10, inflo_capacity, inflos, females)
        larvae10 <- females[day - 10, ] * R * eggs * mu_larvation
    }
    
    if (day > 11) {
        R <- disponibility(day - 11, inflo_capacity, inflos, females)
        larvae11 <- females[day - 11, ] * R * eggs * mu_larvation
    }
    
    if (day > 12) {
        R <- disponibility(day - 12, inflo_capacity, inflos, females)
        larvae12 <- females[day - 12, ] * R * eggs * mu_larvation
    }
    
    
    beta7 * larvae7 + beta8 * larvae8 + beta9 * larvae9 +
        beta10 * larvae10 + beta11 * larvae11 + beta12 * larvae12
}


emerging <- function(day, larves, mu_sol) {
    ## Femelles endogenes
    if (day <= 7)
        c(0, 0, 0)
    else
        larves[day - pupe_duration, ] * mu_sol * proba_pupe * sex_ratio
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
    larves <- matrix(0, nrow = nb_jours, ncol = 3)
    females_endo <- matrix(0, nrow = nb_jours, ncol = 3)
    females <- matrix(0, nrow = nb_jours, ncol = 3)
    mu_sol <- c(mu_ER, mu_PS, mu_EH)
    for (jour in 1:nb_jours) {
        larves[jour, ] <- larvaes_count(jour, inflo_capacity, inflos, females)
        females_endo[jour, ] <- emerging(jour, larves, mu_sol)
        females[jour, 1] <- females_count(jour, alpha[[1]], females_exo[, 1], females_endo)
        females[jour, 2] <- females_count(jour, alpha[[2]], females_exo[, 2], females_endo)
        females[jour, 3] <- females_count(jour, alpha[[3]], females_exo[, 3], females_endo)
    }
    
    larves
}