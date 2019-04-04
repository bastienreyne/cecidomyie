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
inflos <- cbind(inflos.ER, inflos.B, inflos.EH)
larves.obs <- cbind(larves.ER, larves.B, larves.EH)

## Indices
ER <- 1
B <- 2
EH <- 3

## Fonctions
exogene <- function(gamma, inflos){
    return(gamma * inflos)
}

echange.dans.bloc <- function(proba.migration, inflos){
    alphaER <- alphaB <- alphaEH <- matrix(0, nrow = nb.jours, ncol = 3)
    alphaER[,ER] <- alphaEH[,EH] <- 1 - proba.migration
    alphaER[,EH] <- proba.migration * inflos[,ER] / (inflos[,B]+inflos[,ER])
    alphaEH[,ER] <- proba.migration * inflos[,EH] / (inflos[,B]+inflos[,EH])
    alphaB[,ER] <- proba.migration * inflos[,B] / (inflos[,B]+inflos[,EH])
    alphaB[,EH] <- proba.migration * inflos[,B] / (inflos[,B]+inflos[,ER])
    
    return(list(alphaER, alphaB, alphaEH))
}

dispo.ressources <- function(jour, capacite.inflo, inflos, femelles){
    # browser()
    ans <- rep(NA, 3)
    ans[femelles[jour,] <= capacite.inflo * inflos[jour,]] <- 1
    ans[femelles[jour,] > capacite.inflo * inflos[jour,]] <- capacite.inflo * inflos[jour,which(femelles[jour,] > capacite.inflo * inflos[jour,])] / femelles[jour, which(femelles[jour,] > capacite.inflo * inflos[jour,])]
    return(ans)
}

larves <- function(jour, capacite.inflo, inflos, femelles){
    if (jour - duree.larvation <= 0)
        return(c(0,0,0))
    else{
        R <- dispo.ressources(jour-duree.larvation, capacite.inflo, inflos, femelles)
        return(femelles[jour-duree.larvation,] * R * eggs * mu.larvation)
    }
}

femelles.endogene <- function(jour, larves, mu.sol){
    if (jour - duree.pupaison <= 0)
        return(c(0,0,0))
    else
        return(larves[jour - duree.pupaison,] * mu.sol * proba.pupaison * sex.ratio)
}

femelles.total <- function(jour, alpha, femelles.exo, femelles.endo){
    return(femelles.exo[jour] + alpha[jour,] %*% femelles.endo[jour,])
}

dynamiques <- function(gamma, proba.migration, mu.ER, mu.EH, capacite.inflo, inflos){
    alpha <- echange.dans.bloc(as.numeric(proba.migration), inflos)
    
    femelles.exo <- exogene(as.numeric(gamma), inflos)
    larves <- matrix(0, nrow = nb.jours, ncol = 3)
    femelles.endo <- matrix(0, nrow = nb.jours, ncol = 3)
    femelles <- matrix(0, nrow = nb.jours, ncol = 3)
    mu.sol <- c(as.numeric(mu.ER), mu.B, as.numeric(mu.EH))
    for (jour in 1:nb.jours){
        # browser()
        femelles[jour,] <- femelles.exo[jour,]
        larves[jour,] <- larves(jour, as.numeric(capacite.inflo), inflos, femelles)
        
        femelles.endo[jour,] <- femelles.endogene(jour, larves, mu.sol)
        
        femelles[jour, ER] <- femelles.total(jour, alpha[[ER]], femelles.exo[,ER], femelles.endo)
        femelles[jour, B] <- femelles.total(jour, alpha[[B]], femelles.exo[,B], femelles.endo)
        femelles[jour, EH] <- femelles.total(jour, alpha[[EH]], femelles.exo[,EH], femelles.endo)
    }
    
    return(larves)
}

