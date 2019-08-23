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
    
    binf <- c(0, 0, 0, 0, 0.1, 500, 1, 1)
    bsup <- c(1, 1, 1, 1, 2, 20300, 11, 4)
    
    res <- nsga2(fun_obj,
                 idim = 8,
                 odim = 3,
                 lower.bounds = binf,
                 upper.bounds = bsup,
                 popsize = 200,
                 generations = 200)
}

# Parallelisation ---------------------------------------------------------

sfInit(parallel = TRUE, cpus = 2, type = "SOCK")
sfLibrary(mco)
sfLibrary(hydroGOF)
sfExportAll()
res <- sfClusterApplyLB(1:2, to_optimize, obj_B)
sfStop()

# Solutions ---------------------------------------------------------------

pareto_B <- res[[1]]$value
params_B <- res[[1]]$par
for (i in 2:length(res)) {
    pareto_B <- rbind(pareto_B, res[[i]]$value)
    params_B <- rbind(params_B, res[[i]]$par)
}

pareto_front_B <- pareto_B[!is_dominated(t(pareto_B)), ]
params_front_B <- params_B[!is_dominated(t(pareto_B)), ]


# save(pareto_B, params_B, pareto_front_B, params_front_B, file = "calibration_B.Rdata")
