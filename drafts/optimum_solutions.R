
# Packages ----------------------------------------------------------------

library(tidyverse)
library(magrittr)
library(mco)
source("model1.R")


# Functions ---------------------------------------------------------------

my_mae <- function(x, y) {
    n <- length(x)
    return( sum(abs(x - y)) / n )
}

objective <- function(x, my_function) {
    larves_estimees <- dynamiques(x[1], x[2], x[3], x[4], x[5], inflos)
    larvesER <- larves_estimees[, ER]
    larvesB <- larves_estimees[, B]
    larvesEH <- larves_estimees[, EH]
    
    return(my_function(larvesER, larves.obs[, ER]) +
               my_function(larvesB, larves.obs[, B]) + 
               my_function(larvesEH, larves.obs[, EH]))
}


# Run optimum solutions ---------------------------------------------------

n_simu <- 3000
parameters_opt <- matrix(NA, nrow = n_simu, ncol = 5)
for (iter in 1:n_simu) {
    set.seed(iter)
    res <- nsga2(objective, 5, 1, my_mae, lower.bounds = c(0, 0, 0.75, 0, 0),
                 upper.bounds = c(0.1, 0.5, 1, 0.1, 0.2),
                 popsize = 200, generations = 50)
    
    ind_min <- which.min(res$value & res$pareto.optimal == TRUE)[1]
    parameters_opt[iter, ] <- res$par[ind_min, ]
}

# Optimal trajectories ----------------------------------------------------

trajectories <- matrix(NA, nrow = 80, ncol = 3 * n_simu)
# traj_norm2 <- matrix(NA, nrow = 80, ncol = 3 * n_simu)
# traj_normmax <- matrix(NA, nrow = 80, ncol = 3 * n_simu)
for (iter in 1:n_simu) {
    trajectories[, c(iter, iter + n_simu, iter + 2 * n_simu)] <- dynamiques(parameters_opt[iter, 1],
                                                                            parameters_opt[iter, 2],
                                                                            parameters_opt[iter, 3],
                                                                            parameters_opt[iter, 4],
                                                                            parameters_opt[iter, 5],
                                                                            inflos)
}