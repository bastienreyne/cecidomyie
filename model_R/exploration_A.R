## Script pour explorer les solutions

# Packages / data ---------------------------------------------------------

library(tidyverse)
library(corrplot)
source("model_new.R")
source("objectif.R")
source("plot_res.R")
load("/home/bastien/cecidomyie/model_R/calibration_A3.Rdata")

# Corrélations ------------------------------------------------------------
    
X <- scale(params_front_A)
Y <- scale(pareto_front_A)

matcor <- cor(cbind(X, Y), method = "spearman")
rownames(matcor) <- c(":gamma", ":p[m]", ":mu[ER]", ":mu[EH]",
                      ":k", "stock", ":E[0] * mu[l]", "ER", "PS", "EH")
colnames(matcor) <- c(":gamma", ":p[m]", ":mu[ER]", ":mu[EH]",
                      ":k", "stock", ":E[0] * mu[l]", "ER", "PS", "EH")
corrplot.mixed(matcor, tl.pos = "lt", mar = c(0, 0, 0.2, 0))

# Classe de solutions -----------------------------------------------------

## On effectue une CAH
hca <- hclust(dist(X), method = "ward.D2")

inertie <- data.frame(height = rev(hca$height), nb_class = 1:841)
ggplot(inertie[1:25, ]) +
    aes(y = height, x = nb_class) +
    geom_step(lwd = 0.75) + 
    theme_bw() +
    ylab("Inertie interne") +
    xlab("Nombre de classes")

classes <- cutree(hca, 16)

# Affichage des résultats -------------------------------------------------

classe <- 3
plot_decompo_A(params_front_A[which(classes == classe)[1], ], inflos_obs)
