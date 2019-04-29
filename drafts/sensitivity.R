# Packages ----------------------------------------------------------------
library(tidyverse)
library(magrittr)
library(multisensi)
library(lhs)
source("model1-calibration.R")


# Expand grid -------------------------------------------------------------

## Grille
p1 <- seq(0, 0.1, length.out = 10)
p2 <- seq(0, 0.5, length.out = 10)
p3 <- seq(0.75, 1, length.out = 5)
p4 <- seq(0, 0.1, length.out = 5)
p5 <- seq(0, 0.25, length.out = 10)
grille <- expand.grid(p1, p2, p3, p4, p5)

## Model output
res <- matrix(NA, nrow = 25000, ncol = 3) %>% as.data.frame
for (i in 1:25000) {
    res[i, ] <- objectif(grille[i, ], my.mae)
}

ans <- multisensi(design = grille, model = res, reduction = NULL)


# plot --------------------------------------------------------------------

to_plot <- rbind(ans$mSI %>% as.data.frame %>% mutate(Effect = "Main effect"),
                 ans$tSI %>% as.data.frame %>% mutate(Effect = "Total effect"))
to_plot %<>% mutate(Parameter = rep(c("gamma", "pm", "muER", "muEH", "k"), 2)) %>% 
    gather(V1, V2, V3, GSI, key = Impact, value = Valeur)

plot2 <- to_plot %>% ggplot(aes(x = Parameter, y = Valeur, fill = Effect)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_grid(. ~ Impact)

# # LHS initialisation ------------------------------------------------------
# nb <- 600
# cube <- randomLHS(n = nb, k = 5)
# cube[, c(1,4)] <- cube[, c(1,4)] * 0.1
# cube[, 2] <- cube[, 2] * 0.5
# cube[, 3] <- cube[, 3] * 0.25 + 0.75
# cube[, 5] <- cube[, 5] * 0.25
# 
# colnames(cube) <- c("gamma", "pm", "muER", "muEH", "k")
# cube %<>% as.data.frame
# 
# res <- matrix(NA, nrow = nb, ncol = 3) %>% as.data.frame
# for (i in 1:nb) {
#     res[i, ] <- objectif(cube[i, ], my.mae)
# }
# 
# ans2 <- multisensi(design = cube, model = res, reduction = NULL)












