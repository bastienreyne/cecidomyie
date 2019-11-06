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
    bsup <- c(1, 1, 1, 1, 2, 20300, 11, 10)
    
    res <- nsga2(fun_obj,
                 idim = 8,
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
res <- sfClusterApplyLB(1:30, to_optimize, obj)
sfStop()

# Solutions ---------------------------------------------------------------

pareto_E <- res[[1]]$value
params_E <- res[[1]]$par
for (i in 2:length(res)) {
    pareto_E <- rbind(pareto_E, res[[i]]$value)
    params_E <- rbind(params_E, res[[i]]$par)
}

pareto_front_E <- pareto_E[!is_dominated(t(pareto_E)), ]
params_front_E <- params_E[!is_dominated(t(pareto_E)), ]


save(pareto_E, params_E, pareto_front_E, params_front_E, file = "calibration_E.Rdata")
