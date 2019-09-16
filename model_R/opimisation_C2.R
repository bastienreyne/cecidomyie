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
    
    binf <- c(0, 0, 0, 0, 0.1, 500, 1, 3)
    bsup <- c(1, 1, 1, 1, 2, 20300, 11, 40)
    
    res <- nsga2(fun_obj,
                 idim = 8,
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
res <- sfClusterApplyLB(1:30, to_optimize, obj_C)
sfStop()

# Solutions ---------------------------------------------------------------

pareto_C2 <- res[[1]]$value
params_C2 <- res[[1]]$par
for (i in 2:length(res)) {
    pareto_C2 <- rbind(pareto_C2, res[[i]]$value)
    params_C2 <- rbind(params_C2, res[[i]]$par)
}

pareto_front_C2 <- pareto_C2[!is_dominated(t(pareto_C2)), ]
params_front_C2 <- params_C2[!is_dominated(t(pareto_C2)), ]


save(pareto_C2, params_C2, pareto_front_C2, params_front_C2, file = "calibration_C2.Rdata")
