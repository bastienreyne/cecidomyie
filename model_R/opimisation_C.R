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
    
    binf <- c(0, 0, 0, 0, 0.1, 500, 1)
    bsup <- c(1, 1, 1, 1, 2, 20300, 11)
    
    res <- nsga2(fun_obj,
                 idim = 7,
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
res <- sfClusterApplyLB(1:2, to_optimize, obj0)
sfStop()

# Solutions ---------------------------------------------------------------

pareto_C <- res[[1]]$value
params_C <- res[[1]]$par
for (i in 2:length(res)) {
    pareto_C <- rbind(pareto_C, res[[i]]$value)
    params_C <- rbind(params_C, res[[i]]$par)
}

pareto_front_C <- pareto_C[!is_dominated(t(pareto_C)), ]
params_front_C <- params_C[!is_dominated(t(pareto_C)), ]


save(pareto_C, params_C, pareto_front_C, params_front_C, file = "calibration_C.Rdata")
