# Script pour calculer la duree des des stades 

library(plyr)
library(dplyr)
library(readr)
library(xlsxjars)
library(xlsx)
library(float)

setwd("../../Fichiers de donnees/Donnees brutes")
data = read.xlsx2("dynamique.cecido.modif0.xlsx",1)

stade = c('C','D1','D2','E','F','G')

data0 = data[data$stade!='NA' & data$stade!='Fruit',]

data0_split = split(data0, list(data0$bloc,data0$trait,data0$arbre,data0$piege,data0$num.etiq))

NbGroup = length(data0_split)

duree_stade = matrix(rep(0,10),2,5)
colnames(duree_stade) = stade[2:6]


########### Toutes les inflorescences à tous les stades 

stadeC  = c()
stadeD1 = c()
stadeD2 = c()
stadeE  = c()
stadeF  = c()
stadeG  = c()

for (i in 1:NbGroup) {
  tmp = data.frame(data0_split[i])
  end = dim(tmp)[1]
  if (end>1) {
    colnames(tmp) = colnames(data)
    tmp$date = as.numeric(as.character(tmp$date))
    tmp$stade = as.character(tmp$stade)
    tmp$etat.panicule = as.numeric(as.character(tmp$etat.panicule))
    if (!('V' %in% tmp$stade)) {
      if (5 %in% tmp$etat.panicule)
        end = which(5 == tmp$etat.panicule)[1]
      j = 1
      stade_now = tmp$stade[j]
      stade_now_index = which(tmp$stade[j] == stade)
      stade_beg = tmp$date[j]
      while (j<end) {
        if (tmp$stade[j+1]==stade_now) {
          j = j+1
        }
        else {
          if (tmp$stade[j+1]==stade[stade_now_index+1]) {
            duree = tmp$date[j]-stade_beg + round((tmp$date[j+1]-tmp$date[j])/2)
            if (stade_now_index==1) 
              stadeC = c(stadeC,duree)
            if (stade_now_index==2) 
              stadeD1 = c(stadeD1,duree)
            if (stade_now_index==3) 
              stadeD2 = c(stadeD2,duree)
            if (stade_now_index==4) 
              stadeE = c(stadeE,duree)
            if (stade_now_index==5) 
              stadeF = c(stadeF,duree)
            if (stade_now_index==6) 
              stadeG = c(stadeG,duree)
          }
          j = j+1
          stade_now = tmp$stade[j]
          stade_now_index = which(tmp$stade[j] == stade)
          stade_beg = tmp$date[j]
        }
      }
      duree = tmp$date[j]-stade_beg
      if (stade_now_index==1) 
        stadeC = c(stadeC,duree)
      if (stade_now_index==2) 
        stadeD1 = c(stadeD1,duree)
      if (stade_now_index==3) 
        stadeD2 = c(stadeD2,duree)
      if (stade_now_index==4) 
        stadeE = c(stadeE,duree)
      if (stade_now_index==5) 
        stadeF = c(stadeF,duree)
      if (stade_now_index==6) 
        stadeG = c(stadeG,duree)
    }
  }
}

stadeD1 = c(stadeD1,stadeC)

# On enlève les valeurs aberrantes et les 0
stadeD1 = stadeD1[stadeD1!=0]
stadeD1 = stadeD1[stadeD1!=11]
stadeD2 = stadeD2[stadeD2!=0]
stadeD2 = stadeD2[stadeD2!=32]
stadeE =  stadeE[stadeE!=0]
stadeF =  stadeF[stadeF!=0]
stadeG =  stadeG[stadeG!=0]

duree_stade[1,1] = round(mean(stadeD1))
duree_stade[1,2] = round(mean(stadeD2))
duree_stade[1,3] = round(mean(stadeE))
duree_stade[1,4] = round(mean(stadeF))
duree_stade[1,5] = round(mean(stadeG))

duree_stade[2,1] = sd(stadeD1)
duree_stade[2,2] = sd(stadeD2)
duree_stade[2,3] = sd(stadeE)
duree_stade[2,4] = sd(stadeF)
duree_stade[2,5] = sd(stadeG)

View (duree_stade)



########### Uniquement les stades où les inflorescences sont vivantes

duree_stade_alive = matrix(rep(0,10),2,5)
colnames(duree_stade_alive) = stade[2:6]

stadeC_alive  = c()
stadeD1_alive = c()
stadeD2_alive = c()
stadeE_alive  = c()
stadeF_alive  = c()
stadeG_alive  = c()

for (i in 1:NbGroup) {
  tmp = data.frame(data0_split[i])
  end = dim(tmp)[1]
  if (end>1) {
    colnames(tmp) = colnames(data)
    tmp$date = as.numeric(as.character(tmp$date))
    tmp$stade = as.character(tmp$stade)
    tmp$etat.panicule = as.numeric(as.character(tmp$etat.panicule))
    if (!('V' %in% tmp$stade)) {
      if (5 %in% tmp$etat.panicule)
        end = which(5 == tmp$etat.panicule)[1]
      j = 1
      stade_now = tmp$stade[j]
      stade_now_index = which(tmp$stade[j] == stade)
      stade_beg = tmp$date[j]
      while (j<end) {
        if (tmp$stade[j+1]==stade_now) {
          j = j+1
        }
        else {
          if (tmp$stade[j+1]==stade[stade_now_index+1]) {
            duree = tmp$date[j]-stade_beg + round((tmp$date[j+1]-tmp$date[j])/2)
            if (!(5 %in% tmp[tmp$stade==stade_now,]$etat.panicule)) {
              if (stade_now_index==1) 
                stadeC_alive = c(stadeC_alive,duree)
              if (stade_now_index==2) 
                stadeD1_alive = c(stadeD1_alive,duree)
              if (stade_now_index==3) 
                stadeD2_alive = c(stadeD2_alive,duree)
              if (stade_now_index==4) 
                stadeE_alive = c(stadeE_alive,duree)
              if (stade_now_index==5) 
                stadeF_alive = c(stadeF_alive,duree)
              if (stade_now_index==6) 
                stadeG_alive = c(stadeG_alive,duree)
            }
          }
          j = j+1
          stade_now = tmp$stade[j]
          stade_now_index = which(tmp$stade[j] == stade)
          stade_beg = tmp$date[j]
        }
      }
      duree = tmp$date[j]-stade_beg
      if (!(5 %in% tmp[tmp$stade==stade_now,]$etat.panicule)) {
        if (stade_now_index==1) 
          stadeC_alive = c(stadeC_alive,duree)
        if (stade_now_index==2) 
          stadeD1_alive = c(stadeD1_alive,duree)
        if (stade_now_index==3) 
          stadeD2_alive = c(stadeD2_alive,duree)
        if (stade_now_index==4) 
          stadeE_alive = c(stadeE_alive,duree)
        if (stade_now_index==5) 
          stadeF_alive = c(stadeF_alive,duree)
        if (stade_now_index==6) 
          stadeG_alive = c(stadeG_alive,duree)
      }
    }
  }
}

stadeD1_alive = c(stadeD1_alive,stadeC_alive)

# On enlève les 0
stadeD1_alive = stadeD1_alive[stadeD1_alive!=0]
stadeD1_alive = stadeD1_alive[stadeD1_alive!=11]
stadeD2_alive = stadeD2_alive[stadeD2_alive!=0]
stadeE_alive =  stadeE_alive[stadeE_alive!=0]
stadeF_alive =  stadeF_alive[stadeF_alive!=0]
stadeG_alive =  stadeG_alive[stadeG_alive!=0]

duree_stade_alive[1,1] = round(mean(stadeD1_alive))
duree_stade_alive[1,2] = round(mean(stadeD2_alive))
duree_stade_alive[1,3] = round(mean(stadeE_alive))
duree_stade_alive[1,4] = round(mean(stadeF_alive))
duree_stade_alive[1,5] = round(mean(stadeG_alive))

duree_stade_alive[2,1] = sd(stadeD1_alive)
duree_stade_alive[2,2] = sd(stadeD2_alive)
duree_stade_alive[2,3] = sd(stadeE_alive)
duree_stade_alive[2,4] = sd(stadeF_alive)
duree_stade_alive[2,5] = sd(stadeG_alive)


View (duree_stade_alive)