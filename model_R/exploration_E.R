## Script pour explorer les solutions

# Packages / data ---------------------------------------------------------

library(tidyverse)
library(corrplot)
source("model_new.R")
source("objectif.R")
source("plot_res.R")
load("/home/bastien/cecidomyie/model_R/calibration_E.Rdata")

# Distrib duree dvpmt -----------------------------------------------------

data.frame(duree_dvpmt = round(params_front_E[, 8])) %>% ggplot +
    aes(x = duree_dvpmt) +
    geom_histogram(binwidth = 0.1) +
    theme_bw() +
    ylab("Nombre d'occurrences") +
    xlab("Nombre de jours entre la ponte et les premières sorties de pupaison") +
    scale_x_continuous(breaks = 1:10)

# Corrélations ------------------------------------------------------------

## Choisir ici la durée de dvpmt voulue (premier jour d'éjection)
ind_dvmpt <- which(round(params_front_E[, 8]) == 9)

X <- scale(params_front_E[ind_dvmpt, ])
Y <- scale(pareto_front_E[ind_dvmpt, ])

matcor <- cor(cbind(X, Y), method = "spearman")
rownames(matcor) <- c(":gamma", ":p[m]", ":mu[ER]", ":mu[EH]",
                      ":k", "stock", ":E[0] * mu[l]", "dA", "ER", "PS", "EH")
colnames(matcor) <- c(":gamma", ":p[m]", ":mu[ER]", ":mu[EH]",
                      ":k", "stock", ":E[0] * mu[l]", "dA", "ER", "PS", "EH")
corrplot.mixed(matcor, tl.pos = "lt")

# Classe de solutions -----------------------------------------------------

## On effectue une CAH
hca <- hclust(dist(X), method = "ward.D2")

inertie <- data.frame(height = rev(hca$height), nb_class = 1:(nrow(X)-1))
ggplot(inertie[1:25, ]) +
    aes(y = height, x = nb_class) +
    geom_step(lwd = 0.75) + 
    theme_bw() +
    ylab("Inertie interne") +
    xlab("Nombre de classes")

## Pour duree = 5, on prends 9 classes
## Pour duree = 9, on prends 8 classes
classes <- cutree(hca, 6)

# Affichage des résultats -------------------------------------------------

classe <- 1
plot_decompo_E(params_front_E[ind_dvmpt, ][which(classes == classe)[1], ], inflosCDE)

