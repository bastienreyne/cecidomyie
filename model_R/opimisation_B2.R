## Script executant NSGA2 pour calibrer notre modele

# Packages ----------------------------------------------------------------

library(mco) ## Pour nsga2
library(emoa) ## Pour is_dominated
library(snow) ## Pour la parallelisation
library(snowfall) ## Pour la parallelisation
source("/home/bastien/cecidomyie/model_R/objectif.R")

# NSGA-II -----------------------------------------------------------------

to_optimize <- function(x, fun_obj) {
    ## x servira pour la parallelisation
    ## fun_obj correspond a notre fonction objectif
    
    binf <- c(0, 0, 0, 0, 0.1, 500, 1, 1, 0)
    bsup <- c(1, 1, 1, 1, 2, 20300, 11, 4, 1)
    
    res <- nsga2(fun_obj,
                 idim = 9,
                 odim = 3,
                 lower.bounds = binf,
                 upper.bounds = bsup,
                 popsize = 200,
                 generations = 200)
}

# Parallelisation ---------------------------------------------------------

sfInit(parallel = TRUE, cpus = 3, type = "SOCK")
sfLibrary(mco)
sfLibrary(hydroGOF)
sfExportAll()
res <- sfClusterApplyLB(1:30, to_optimize, obj_B2)
sfStop()

# Solutions ---------------------------------------------------------------

pareto_B2 <- res[[1]]$value
params_B2 <- res[[1]]$par
for (i in 2:length(res)) {
    pareto_B2 <- rbind(pareto_B2, res[[i]]$value)
    params_B2 <- rbind(params_B2, res[[i]]$par)
}

pareto_front_B2 <- pareto_B2[!is_dominated(t(pareto_B2)), ]
params_front_B2 <- params_B2[!is_dominated(t(pareto_B2)), ]


save(pareto_B2, params_B2, pareto_front_B2, params_front_B2, file = "calibration_B2.Rdata")
