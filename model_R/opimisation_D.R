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
    
    binf <- c(0, 0, 0, 0, 0.1, 500, 1, 0)
    bsup <- c(1, 1, 1, 1, 2, 20300, 11, 1)
    
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
res <- sfClusterApplyLB(1:2, to_optimize, obj0_season)
sfStop()

# Solutions ---------------------------------------------------------------

pareto_D <- res[[1]]$value
params_D <- res[[1]]$par
for (i in 2:length(res)) {
    pareto_D <- rbind(pareto_D, res[[i]]$value)
    params_D <- rbind(params_D, res[[i]]$par)
}

pareto_front_D <- pareto_D[!is_dominated(t(pareto_D)), ]
params_front_D <- params_D[!is_dominated(t(pareto_D)), ]


save(pareto_D, params_D, pareto_front_D, params_front_D, file = "calibration_D.Rdata")
