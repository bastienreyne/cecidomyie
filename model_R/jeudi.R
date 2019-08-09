## Script executant NSGA2 pour calibrer notre modele

# Packages ----------------------------------------------------------------

library(mco) ## Pour nsga2
library(emoa) ## Pour is_dominated
library(magrittr)
source("/home/bastien/cecidomyie/model_R/objectif.R")
source("/home/bastien/cecidomyie/model_R/plot_res.R")
inflosCDE_b2 <- as.matrix(read.csv(file = "/home/bastien/cecidomyie/data/2017_inflosCDE_bloc2.csv",
                                row.names = "id"))

# Objectifs ER, PS, EH ----------------------------------------------------

binf <- c(0, 0, 0, 0, 0.01, 500, 1)
bsup <- c(0.1, 1, 1, 1, 100, 20300, 11)
res <- nsga2(obj,
             idim = 7, odim = 3,
             lower.bounds = binf,
             upper.bounds = bsup,
             popsize = 200,
             generations = 200)

plot_decompo(res$par[1,], inflosCDE)


res1 <- nsga2(obj0,
              idim = 7, odim = 3,
              lower.bounds = binf,
              upper.bounds = bsup,
              popsize = 200,
              generations = 200)
ind1 <- res1$value %>%
    as_tibble %>% 
    mutate(norm = abs(V1) + abs(V2) + abs(V3)) %$%
    which.min(norm)
plot_decompo(res1$par[77,], inflosCDE)

res2 <- nsga2(obj0_season,
              idim = 8, odim = 3,
              lower.bounds = c(binf, 0),
              upper.bounds = c(bsup, 1),
              popsize = 200,
              generations = 200)

res3 <- nsga2(obj0_season_inflos,
              idim = 8, odim = 3,
              lower.bounds = c(binf, 0),
              upper.bounds = c(bsup, 1),
              popsize = 200,
              generations = 200)


for (i in 1:66) {
    gridExtra::grid.arrange(plot_decompo_season(res2$par[(i-1) * 3 + 1, ], inflosCDE + 1),
                            plot_decompo_season(res2$par[(i-1) * 3 + 2, ], inflosCDE + 1),
                            plot_decompo_season(res2$par[(i-1) * 3 + 3, ], inflosCDE + 1),
                            nrow = 3)
    readline(prompt="Press [enter] to continue")
}
