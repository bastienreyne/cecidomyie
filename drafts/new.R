
# Data --------------------------------------------------------------------

inflos_ER <- read.csv("r1.csv")$inflos_vivantes
inflos_BA <- read.csv("b1.csv")$inflos_vivantes
inflos_EH <- read.csv("h1.csv")$inflos_vivantes
larves_ER <- read.csv("r1.csv")$larves
larves_BA <- read.csv("b1.csv")$larves
larves_EH <- read.csv("h1.csv")$larves
inflos2017 <- cbind(inflos_ER, inflos_BA, inflos_EH)
larves_obs <- cbind(larves_ER, larves_BA, larves_EH)

# Indices -----------------------------------------------------------------

ER <- 1
BA <- 2
EH <- 3

# Fixed parameters --------------------------------------------------------

sex_ratio <- 0.5
mu_BA <- 0
proba_pupaison <- 0.77
duree_larvation <- 7
duree_pupaison <- 5
eggs <- 150
mu_larvation <- 0.04
nb_jours <- 80

# Parameters to be determined ---------------------------------------------

gamma <- 0.1
capacite_inflo <- 0.1
proba_migration <- 0.4
mu_ER <- 0.9
mu_EH <- 0.1
larves_init <- 12
femelles_endo_init <- 5

# Functions ---------------------------------------------------------------

income <- function(gamma, inflos) {
    gamma * inflos
}

exchange <- function(proba_migration, inflos) {
    alpha <- matrix(0, nrow = nb_jours, ncol = 9)
    alpha[, c(1, 9)] <- 1 - proba_migration
    alpha[, 3] <- proba_migration * inflos[, ER] / (inflos[, BA] + inflos[, ER])
    alpha[, 4] <- proba_migration * inflos[, BA] / (inflos[, BA] + inflos[, EH])
    alpha[, 6] <- proba_migration * inflos[, BA] / (inflos[, BA] + inflos[, ER])
    alpha[, 7] <- proba_migration * inflos[, EH] / (inflos[, BA] + inflos[, EH])
    alpha
}

disponibility <- function(day, capacite_inflo, females, inflos) {
    ans <- rep(NA, 3)
    ans[females[day, ] <= capacite_inflo * inflos[day,]] <- 1
    ans[females[day, ] > capacite_inflo * inflos[day,]] <- capacite_inflo *
        inflos[day, which(females[day, ] > capacite_inflo * inflos[day, ])] /
        females[day, which(females[day, ] > capacite_inflo * inflos[day, ])]
    ans
}

larvae <- function(day, capacite_inflo, larvae_init, females, inflos) {
    if (day - duree_larvation <= 0)
        return(c(larvae_init[1], larvae_init[2], larvae_init[3]))
    else {
        R <- disponibility(day - duree_larvation, capacite_inflo, females, inflos)
        return(females[day - duree_larvation, ] * R * eggs * mu_larvation)
    }
}

endogenous <- function(day, f_endo_init, larves, mu_sol) {
    if (day - duree_pupaison <= 0)
        return(c(f_endo_init[1], 0, f_endo_init[2]))
    else {
        return(larves[day - duree_pupaison, ] * mu_sol * proba_pupaison * sex_ratio)
    }
}

females_total <- function(day, alpha, femelles_exo, femelles_endo) {
    femelles_exo[day] + alpha[day, ] %*% femelles_endo[day, ]
}

dynamics <- function(arg, inflos = inflos2017) {
    ## args = gamma, capacite_inflo, proba_migration, muER, muEH, L_ERinit,
    ## L_Binit, L_EHinit, N_ERendo_init, N_EHinit 
    alpha <- exchange(arg[3], inflos)
    femelles_exo <- income(arg[1], inflos)
    larves <- matrix(0, nrow = nb_jours, ncol = 3)
    femelles_endo <- matrix(0, nrow = nb_jours, ncol = 3)
    femelles <- matrix(0, nrow = nb_jours, ncol = 3)
    mu_sol <- c(arg[4], mu_BA, arg[5])
    for (jour in 1:nb_jours) {
        larves[jour, ] <- larvae(jour, arg[2], arg[6:8], femelles, inflos)
        
        femelles_endo[jour, ] <- endogenous(jour, arg[9:10], larves, mu_sol)
        
        femelles[jour, ER] <- females_total(jour, alpha[, 1:3], femelles_exo[, ER], femelles_endo)
        femelles[jour, BA] <- females_total(jour, alpha[, 4:6], femelles_exo[, BA], femelles_endo)
        femelles[jour, EH] <- females_total(jour, alpha[, 7:9], femelles_exo[, EH], femelles_endo)
    }
    larves
}