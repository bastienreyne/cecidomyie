## Script qui effectue une ACP-IV des solutions du modèle A afin de trouver les 3 scénarios envisagés

# Packages / data ---------------------------------------------------------

library(tidyverse)
library(ade4)
load("/home/bastien/cecidomyie/model_R/calibration_A.Rdata")

# Allons-y ! --------------------------------------------------------------

X <- scale(params_front_A)
Y <- scale(pareto_front_A)

pcaX <- dudi.pca(X, scannf = FALSE, nf = 3)
pcaivY <- pcaiv(pcaX, Y, scannf = FALSE, nf = 2)

pcaY <- dudi.pca(Y, scannf = FALSE, nf = 2)
pcaivX <- pcaiv(pcaY, X, scannf = FALSE, nf = 2)
