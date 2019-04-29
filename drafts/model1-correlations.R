library(tidyverse)
library(magrittr)
source("model1.R")

my.rmse <- function(x, y){
    n <- length(x)
    ans <- sqrt(sum((x-y)^2) / n)
    return(ans)
}


my.rrmse <- function(x, y){
    ans <- sqrt(sum((x-y)^2)) / mean(y)
    return(ans)
}


my.mae <- function(x, y){
    n <- length(x)
    return( sum(abs(x-y)) / n )
}


my.mse <- function(x, y){
    n <- length(x)
    return( sum((x-y)^2) / n )
}


objectif <- function(x, ma.fonction){
    larves_estimees <- dynamiques(x[1], x[2], x[3], x[4], x[5], inflos)
    # index <- 13:80
    larvesER <- larves_estimees[, ER]
    larvesB <- larves_estimees[, B]
    larvesEH <- larves_estimees[, EH]
    
    return(c(ma.fonction(larvesER, larves.obs[, ER]), 
             ma.fonction(larvesB, larves.obs[, B]),
             ma.fonction(larvesEH, larves.obs[, EH])))
}



# test.rmse <- nsga2(objectif, 5, 3, my.rmse, lower.bounds = rep(0,5), upper.bounds = c(1000,1,1,1,1000), popsize = 1000, generations = 1000)
number <- 500
norme1 <- matrix(NA, nrow = number, ncol = 5)
norme2 <- matrix(NA, nrow = number, ncol = 5)
normemax <- matrix(NA, nrow = number, ncol = 5)
for (i in 1:number) {
    res <- nsga2(objectif, 5, 3, my.mae, lower.bounds = c(0, 0, 0.75, 0, 0),
                 upper.bounds = c(0.1, 0.5, 1, 0.1, 0.2),
                 popsize = 100, generations = 50)
    val <- res$value %>% as.data.frame
    colnames(val) <- c("ER", "B", "EH")
    val %<>% mutate(norm1 = abs(ER) + abs(B) + abs(EH),
                    norm2 = sqrt(ER^2 + B^2 + EH^2),
                    normmax = pmax(ER, B, EH))
    min1 <- which.min(val$norm1)
    min2 <- which.min(val$norm2)
    minmax <- which.min(val$normmax)
    norme1[i, ] <- res$par[min1, ]
    norme2[i, ] <- res$par[min2, ]
    normemax[i, ] <- res$par[minmax, ]
}
