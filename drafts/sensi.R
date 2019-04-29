library(tidyverse)
library(magrittr)

source("model1-calibration.R")
load("test3")

X <- test.mae$par
Y <- test.mae$value

n.obs <- nrow(X)

sg <- matrix(NA, nrow = n.obs, ncol = 3)
spm <- matrix(NA, nrow = n.obs, ncol = 3)
smuer <- matrix(NA, nrow = n.obs, ncol = 3)
smueh <- matrix(NA, nrow = n.obs, ncol = 3)
sk <- matrix(NA, nrow = n.obs, ncol = 3)
for (i in 1:n.obs){
  gamma <- X[i,1]
  pm <- X[i,2]
  muer <- X[i,3]
  mueh <- X[i,4]
  k <- X[i, 5]
  yg <- ypm <- ymuer <- ymueh <- yk <- matrix(NA, nrow = n.obs, ncol = 3)
  for (j in 1:n.obs){
    yg[j, ] <- objectif(c(gamma, X[j,2:5]), my.mae)
    ypm[j, ] <- objectif(c(X[j,1], pm, X[j,3:5]), my.mae)
    ymuer[j, ] <- objectif(c(X[j,1:2], muer, X[j,4:5]), my.mae)
    ymueh[j, ] <- objectif(c(X[j, 1:3], mueh, X[j,5]), my.mae)
    yk[j, ] <- objectif(c(X[j,1:4], k), my.mae)
  }
  sg[i, ] <- colMeans(yg)
  spm[i, ] <- colMeans(ypm)
  smuer[i, ] <- colMeans(ymuer)
  smueh[i, ] <- colMeans(ymueh)
  sk[i, ] <- colMeans(yk)
}

vary <- diag(var(Y))
SG <- diag(var(sg)) / vary
SPM <- diag(var(spm)) / vary
SMUER <- diag(var(smuer)) / vary
SMUEH <- diag(var(smueh)) / vary
SK <- diag(var(sk)) / vary
