## Script pour explorer les solutions

# Packages / data ---------------------------------------------------------

library(tidyverse)
library(corrplot)
source("model_new.R")
source("objectif.R")
source("plot_res.R")
load("/home/bastien/cecidomyie/model_R/calibration_C.Rdata")

# Corrélations ------------------------------------------------------------

X <- scale(params_front_C)
Y <- scale(pareto_front_C)

matcor <- cor(cbind(X, Y), method = "spearman")
rownames(matcor) <- c(":gamma", ":p[m]", ":mu[ER]", ":mu[EH]",
                      ":k", "stock", ":E[0] * mu[l]", "ER", "PS", "EH")
colnames(matcor) <- c(":gamma", ":p[m]", ":mu[ER]", ":mu[EH]",
                      ":k", "stock", ":E[0] * mu[l]", "ER", "PS", "EH")
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

classes <- cutree(hca, 13)

# Affichage des résultats -------------------------------------------------

classe <- 1
plot_decompo_C(params_front_C[which(classes == classe)[1], ], inflos_obs)

