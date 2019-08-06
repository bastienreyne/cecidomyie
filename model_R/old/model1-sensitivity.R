library(tidyverse)
library(magrittr)

source("model1-calibration.R")
load("def.Rdata")

X <- def$par
Y <- def$value

nb <- 10000
m <- 300

gamma.dens <- density(def$par[,1])
pm.dens <- density(def$par[,2])
muer.dens <- density(def$par[,3])
mueh.dens <- density(def$par[,4])
k.dens <- density(def$par[,5])

gammas <- sample(gamma.dens$x, size=nb, replace = TRUE, prob = gamma.dens$y)
pms <- sample(pm.dens$x, size=nb, replace = TRUE, prob = pm.dens$y)
muers <- sample(muer.dens$x, size=nb, replace = TRUE, prob = muer.dens$y)
muehs <- sample(mueh.dens$x, size=nb, replace = TRUE, prob = mueh.dens$y)
ks <- sample(k.dens$x, size=nb, replace = TRUE, prob = k.dens$y)

sg <- spm <- smuer <- smueh <- sk <- matrix(NA, nrow = nb, ncol = 3)
sgt <- spmt <- smuert <- smueht <- skt <- matrix(NA, nrow = nb, ncol = 3)
for (i in 1:nb){
    ## Main effect
    gamma <- gammas[i]
    pm <- pms[i]
    muer <- muers[i]
    mueh <- muehs[i]
    k <- ks[i]
    
    yg <- ypm <- ymuer <- ymueh <- yk <- matrix(NA, nrow = m, ncol = 3)
    
    ## Total effect
    index <- sample(1:nb, 5, replace = TRUE)
    gamma.star <- gammas[index[1]]
    pm.star <- pms[index[2]]
    muer.star <- muers[index[3]]
    mueh.star <- muehs[index[4]]
    k.star <- ks[index[5]]
    
    ygt <- ypmt <- ymuert <- ymueht <- ykt <- matrix(NA, nrow = m, ncol = 3)
    
    ## Hop hop hop
    for (j in 1:m){
        ## Main effect
        ind <- sample(1:nb, 4, replace = TRUE)
        yg[j, ] <- objectif(c(gamma, pms[ind[1]], muers[ind[2]], muehs[ind[3]], ks[ind[4]]), my.mae)
        ypm[j, ] <- objectif(c(gammas[ind[1]], pm, muers[ind[2]], muehs[ind[3]], ks[ind[4]]), my.mae)
        ymuer[j, ] <- objectif(c(gammas[ind[1]], pms[ind[2]], muer, muehs[ind[3]], ks[ind[4]]), my.mae)
        ymueh[j, ] <- objectif(c(gammas[ind[1]], pms[ind[2]], muers[ind[3]], mueh, ks[ind[4]]), my.mae)
        yk[j, ] <- objectif(c(gammas[ind[1]], pms[ind[2]], muers[ind[3]], muehs[ind[4]], k), my.mae)
        
        ## Total effect
        ind2 <- sample(1:nb, 1)
        ygt[j, ] <- objectif(c(gammas[ind2], pm.star, muer.star, mueh.star, k.star), my.mae)
        ypmt[j, ] <- objectif(c(gamma.star, pms[ind2], muer.star, mueh.star, k.star), my.mae)
        ymuert[j, ] <- objectif(c(gamma.star, pm.star, muers[ind2], mueh.star, k.star), my.mae)
        ymueht[j, ] <- objectif(c(gamma.star, pm.star, muer.star, muehs[ind2], k.star), my.mae)
        ykt[j, ] <- objectif(c(gamma.star, pm.star, muer.star, mueh.star, ks[ind2]), my.mae)
        
    }
    ## Main effect
    sg[i, ] <- colMeans(yg)
    spm[i, ] <- colMeans(ypm)
    smuer[i, ] <- colMeans(ymuer)
    smueh[i, ] <- colMeans(ymueh)
    sk[i, ] <- colMeans(yk)
    
    ## Total effects
    sgt[i, ] <- colMeans(ygt)
    spmt[i, ] <- colMeans(ypmt)
    smuert[i, ] <- colMeans(ymuert)
    smueht[i, ] <- colMeans(ymueht)
    skt[i, ] <- colMeans(ykt)
}


vary <- diag(var(Y))

## Main effect
SG <- diag(var(sg)) / vary
SPM <- diag(var(spm)) / vary
SMUER <- diag(var(smuer)) / vary
SMUEH <- diag(var(smueh)) / vary
SK <- diag(var(sk)) / vary

## Total effect
SGt <- 1 - diag(var(sgt)) / vary
SPMt <- 1 - diag(var(spmt)) / vary
SMUERt <- 1 - diag(var(smuert)) / vary
SMUEHt <- 1 - diag(var(smueht)) / vary
SKt <- 1 - diag(var(skt)) / vary

save(SG, SPM, SMUER, SMUEH, SK, SGt, SPMt, SMUERt, SMUEHt, SKt, file="soboled.Rdata")
