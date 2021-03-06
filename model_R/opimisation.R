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
    bsup <- c(0.1, 1, 1, 1, 2, 20300, 11)
    
    res <- nsga2(fun_obj,
                 idim = 7,
                 odim = 3,
                 lower.bounds = binf,
                 upper.bounds = bsup,
                 popsize = 200,
                 generations = 200)
}

# Parallelisation ---------------------------------------------------------

sfInit(parallel = TRUE, cpus = 11, type = "SOCK")
sfLibrary(mco)
sfLibrary(hydroGOF)
sfExportAll()
res <- sfClusterApplyLB(1:30, to_optimize, obj0)
sfStop()

# Solutions ---------------------------------------------------------------

pareto <- res[[1]]$value
params <- res[[1]]$par
for (i in 2:length(res)) {
    pareto <- rbind(pareto, res[[i]]$value)
    params <- rbind(params, res[[i]]$par)
}

pareto_front <- pareto[!is_dominated(t(pareto)), ]
params_front <- params[!is_dominated(t(pareto)), ]


save(pareto, params, pareto_front, params_front, file = "calibration.Rdata")
