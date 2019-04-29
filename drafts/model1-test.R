library(tidyverse) 
library(magrittr)
library(mco)

sex.ratio <- 0.5
mu.B <- 0
proba.pupaison <- 0.77
duree.larvation <- 7
duree.pupaison <- 5
eggs <- 150
mu.larvation <- 0.04
nb.jours <- 80

## Paramètres à calibrer
gamma <- 1.5
mu.ER <- 0.8
mu.EH <- 0.4
proba.migration <- 0.5
capacite.inflo <- 4.5

## Datas
inflos.ER <- read.csv("r1.csv")$inflos_vivantes
inflos.B <- read.csv("b1.csv")$inflos_vivantes
inflos.EH <- read.csv("h1.csv")$inflos_vivantes
larves.ER <- read.csv("r1.csv")$larves
larves.B <- read.csv("b1.csv")$larves
larves.EH <- read.csv("h1.csv")$larves

## Fonctions
exogene <- function(gamma, inflosER = inflos.ER, inflosB = inflos.B, inflosEH = inflos.EH){
    return(gamma * cbind(inflosER, inflosB, inflosEH))
}

alpha <- function(proba.migration, inflosER = inflos.ER, inflosB = inflos.B, inflosEH = inflos.EH){
    alphaER <- alphaB <- alphaEH <- matrix(0, nrow = nb.jours, ncol = 3)
    alphaER[,1] <- alphaB[,2] <- alphaEH[,3] <- 1 - proba.migration
    alphaER[,3] <- proba.migration * inflosER / (inflosB+inflosER)
    alphaEH[,1] <- proba.migration * inflosEH / (inflosB+inflosEH)
    
    return( list(alphaER, alphaB, alphaEH))
}

dispo.ressources <- function(jour, capacite.inflo, inflos, femelles){
    # browser()
    if (femelles[jour] <= capacite.inflo * inflos[jour])
        return(1)
    else
        capacite.inflo * inflos[jour] / femelles[jour]
}

larves <- function(jour, capacite.inflo, inflos, femelles){
    # browser()
    if (jour - duree.larvation <= 0)
        return(0)
    else{
        # browser()
        R <- dispo.ressources(jour, capacite.inflo, inflos, femelles)
        return(femelles[jour-duree.larvation] * R * eggs * mu.larvation)
    }
}

femelles.endogene <- function(jour, larves, mu.sol){
    if (jour - duree.larvation - duree.pupaison <= 0)
        return(0)
    else
        return(larves[jour - duree.larvation - duree.pupaison] * mu.sol * proba.pupaison)
}

femelles.total <- function(jour, alpha, femelles, femelles.endo){
    # browser()
    return(femelles[jour] + alpha[jour,] %*% femelles.endo)
}

dynamiques <- function(gamma, proba.migration, mu.ER, mu.EH, capacite.inflo, inflosER, inflosB, inflosEH){
    # browser()
    alphas <- alpha(proba.migration, inflosER, inflosB, inflosEH)
    alphaER <- alphas[[1]]
    alphaB <- alphas[[2]]
    alphaEH <- alphas[[3]]
    
    femelles <- exogene(gamma, inflosER, inflosB, inflosEH)
    femellesER <- femelles[,1]
    femellesB <- femelles[,2]
    femellesEH <- femelles[,3]
    
    larvesER <- larvesB <- larvesEH <- rep(0, nb.jours)
    femelles.endoER <- femelles.endoB <- femelles.endoEH <- rep(0, nb.jours)
    
    for (jour in 1:nb.jours){
        larvesER[jour] <- larves(jour, capacite.inflo, inflosER, femellesER)
        larvesB[jour] <- larves(jour, capacite.inflo, inflosB, femellesB)
        larvesEH[jour] <- larves(jour, capacite.inflo, inflosEH, femellesEH)
        
        femelles.endoER[jour] <- femelles.endogene(jour, larvesER, mu.ER)
        femelles.endoB[jour] <- femelles.endogene(jour, larvesB, mu.B)
        femelles.endoEH[jour] <- femelles.endogene(jour, larvesEH, mu.EH)
        femelle.endo <- c(femelles.endoER[jour], femelles.endoB[jour], femelles.endoEH[jour])
        
        # browser()
        femellesER[jour] <- femelles.total(jour, alphaER, femellesER, femelle.endo)
        femellesB[jour] <- femelles.total(jour, alphaB, femellesB, femelle.endo)
        femellesEH[jour] <- femelles.total(jour, alphaEH, femellesEH, femelle.endo)
    }
    
    return(cbind(larvesER, larvesB, larvesEH))
}

my.rmse <- function(x, y){
    return(sum((x-y)^2))
}

objectif <- function(x){
    larves <- dynamiques(x[1], x[2], x[3], x[4], x[5], inflos.ER, inflos.B, inflos.EH)
    larvesER <- larves[,1]
    larvesB <- larves[,2]
    larvesEH <- larves[,3]
    
    return(c(my.rmse(larvesER, larves.ER), my.rmse(larvesB, larves.B), my.rmse(larvesEH, larves.EH)))
}

tmp <- matrix(NA, nrow = 1, ncol = 5)
for(test in 1){
    tmp <- nsga2(objectif, 5, 3, lower.bounds = rep(0,5), upper.bounds = c(150,1,1,1,150), generations = 2)$par[100,]
}
