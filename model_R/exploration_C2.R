## Script pour explorer les solutions

# Packages / data ---------------------------------------------------------

library(tidyverse)
library(corrplot)
source("model_new.R")
source("objectif.R")
source("plot_res.R")
load("/home/bastien/cecidomyie/model_R/calibration_C2.Rdata")

# Corrélations ------------------------------------------------------------
ind_dA <- which(9 <= params_front_C2[, 8] & params_front_C2[, 8] <= 16)
params_dA_C2 <- params_front_C2[ind_dA, ]

X <- scale(params_dA_C2)
Y <- scale(pareto_front_C2[ind_dA, ])

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

classes <- cutree(hca, 12)

    # Affichage des résultats -------------------------------------------------

classe <- 1
plot_decompo_C2(params_dA_C2[which(classes == classe)[1], ], inflosCDE)

