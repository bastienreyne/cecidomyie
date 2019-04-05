## Récupération des données
library (xlsx)
setwd("C:/Users/Laurie/Dropbox/Cecidomyie/Modèle Paul")
databis = read.table(file = "../Fichiers de donnees/Donnees brutes/donnees.floraison.csv",header=TRUE,sep=";")

data1 = databis[databis$Bloc=='1',]
dataAp = data1[data1$ApdateC!="" & !is.na(data1$ApdateC),]
dataAp = dataAp[, c(15,17)]
colnames(dataAp) = c("birth","dead")
dataLat1 = data1[data1$Lat1dateC!="" & !is.na(data1$Lat1dateC),]
dataLat1 = dataLat1[,c(19,21)]
colnames(dataLat1) = c("birth","dead")
dataLat2 = data1[data1$Lat2dateC!="" & !is.na(data1$Lat2dateC),]
dataLat2 = dataLat2[,c(23,25)]
colnames(dataLat2) = c("birth","dead")
dataLat3 = data1[data1$Lat3dateC!="" & !is.na(data1$Lat3dateC),]
dataLat3 = dataLat3[,c(27,29)]
colnames(dataLat3) = c("birth","dead")
dataLat4 = data1[data1$Lat4dateC!="" & !is.na(data1$Lat4dateC),]
dataLat4 = dataLat4[,c(31,33)]
colnames(dataLat4) = c("birth","dead")
dataLat5 = data1[data1$Lat5dateC!="" & !is.na(data1$Lat5dateC),]
dataLat5 = dataLat5[,c(35,37)]
colnames(dataLat5) = c("birth","dead")
dataLat6 = data1[data1$Lat6dateC!="" & !is.na(data1$Lat6dateC),]
dataLat6 = dataLat6[,c(39,41)]
colnames(dataLat6) = c("birth","dead")
dataLat7 = data1[data1$Lat7dateC!="" & !is.na(data1$Lat7dateC),]
dataLat7 = dataLat7[,c(43,45)]
colnames(dataLat7) = c("birth","dead")
dataLat8 = data1[data1$Lat8dateC!="" & !is.na(data1$Lat8dateC),]
dataLat8 = dataLat8[,c(47,49)]
colnames(dataLat8) = c("birth","dead")
dataLat9 = data1[data1$Lat9dateC!="" & !is.na(data1$Lat9dateC),]
dataLat9 = dataLat9[,c(51,53)]
colnames(dataLat9) = c("birth","dead")
data_ = rbind(dataAp,dataLat1,dataLat2,dataLat2,dataLat3,dataLat4,dataLat5,dataLat6,dataLat7,dataLat8,dataLat9)

stade = c('D','E','F','PF','G')
stade_duree = cumsum(c(7,7,14,14,7))

data_$birth = as.Date(data_$birth,"%d/%m/%Y")
data_$dead = as.Date(data_$dead,"%d/%m/%Y")
debut = min(min(data_$birth),min(data_$dead[!is.na(data_$dead)]))
fin = max(max(data_$birth),max(data_$dead[!is.na(data_$dead)]))
NbJours = as.integer(fin - debut)
date = debut + c(0:NbJours)

stade_1 = matrix(0,5,NbJours)
rownames(stade_1) = stade

for (i in 1:dim(data_)[1]) {
  days = stade_duree[5]
  kk = which(data_$birth[i]==date)
  if (!is.na(data_$dead[i]))
    days = as.integer(data_$dead[i] - data_$birth[i])
  z = 0
  j = 1
  while (j<=5 & z==0) {
    cpt = kk + stade_duree[j]
    if (cpt>=NbJours)
      z = j
    j = j+1
  }
  if (z==0)
    z = 5
  stade_1[1,kk:min(NbJours,(kk+stade_duree[1]))] = stade_1[1,kk:min(NbJours,(kk+stade_duree[1]))] + 1
  if (z>1) {
    for (j in 2:z) {
      stade_1[j,(kk+stade_duree[j-1]+1):min((kk+stade_duree[j]),NbJours)] = stade_1[j,(kk+stade_duree[j-1]+1):min((kk+stade_duree[j]),NbJours)] + 1 
    }
  }
}

data1 = databis[databis$Bloc=='2',]
dataAp = data1[data1$ApdateC!="" & !is.na(data1$ApdateC),]
dataAp = dataAp[, c(15,17)]
colnames(dataAp) = c("birth","dead")
dataLat1 = data1[data1$Lat1dateC!="" & !is.na(data1$Lat1dateC),]
dataLat1 = dataLat1[,c(19,21)]
colnames(dataLat1) = c("birth","dead")
dataLat2 = data1[data1$Lat2dateC!="" & !is.na(data1$Lat2dateC),]
dataLat2 = dataLat2[,c(23,25)]
colnames(dataLat2) = c("birth","dead")
dataLat3 = data1[data1$Lat3dateC!="" & !is.na(data1$Lat3dateC),]
dataLat3 = dataLat3[,c(27,29)]
colnames(dataLat3) = c("birth","dead")
dataLat4 = data1[data1$Lat4dateC!="" & !is.na(data1$Lat4dateC),]
dataLat4 = dataLat4[,c(31,33)]
colnames(dataLat4) = c("birth","dead")
dataLat5 = data1[data1$Lat5dateC!="" & !is.na(data1$Lat5dateC),]
dataLat5 = dataLat5[,c(35,37)]
colnames(dataLat5) = c("birth","dead")
dataLat6 = data1[data1$Lat6dateC!="" & !is.na(data1$Lat6dateC),]
dataLat6 = dataLat6[,c(39,41)]
colnames(dataLat6) = c("birth","dead")
dataLat7 = data1[data1$Lat7dateC!="" & !is.na(data1$Lat7dateC),]
dataLat7 = dataLat7[,c(43,45)]
colnames(dataLat7) = c("birth","dead")
dataLat8 = data1[data1$Lat8dateC!="" & !is.na(data1$Lat8dateC),]
dataLat8 = dataLat8[,c(47,49)]
colnames(dataLat8) = c("birth","dead")
dataLat9 = data1[data1$Lat9dateC!="" & !is.na(data1$Lat9dateC),]
dataLat9 = dataLat9[,c(51,53)]
colnames(dataLat9) = c("birth","dead")
data_ = rbind(dataAp,dataLat1,dataLat2,dataLat2,dataLat3,dataLat4,dataLat5,dataLat6,dataLat7,dataLat8,dataLat9)

stade = c('D','E','F','PF','G')
stade_duree = cumsum(c(7,7,14,14,7))

data_$birth = as.Date(data_$birth,"%d/%m/%Y")
data_$dead = as.Date(data_$dead,"%d/%m/%Y")
debut = min(min(data_$birth),min(data_$dead[!is.na(data_$dead)]))
fin = max(max(data_$birth),max(data_$dead[!is.na(data_$dead)]))
NbJours = as.integer(fin - debut)
date = debut + c(0:NbJours)

stade_2 = matrix(0,5,NbJours)
rownames(stade_2) = stade

for (i in 1:dim(data_)[1]) {
  days = stade_duree[5]
  kk = which(data_$birth[i]==date)
  if (!is.na(data_$dead[i]))
    days = as.integer(data_$dead[i] - data_$birth[i])
  z = 0
  j = 1
  while (j<=5 & z==0) {
    cpt = kk + stade_duree[j]
    if (cpt>=NbJours)
      z = j
    j = j+1
  }
  if (z==0)
    z = 5
  stade_2[1,kk:min(NbJours,(kk+stade_duree[1]))] = stade_2[1,kk:min(NbJours,(kk+stade_duree[1]))] + 1
  if (z>1) {
    for (j in 2:z) {
      stade_2[j,(kk+stade_duree[j-1]+1):min((kk+stade_duree[j]),NbJours)] = stade_2[j,(kk+stade_duree[j-1]+1):min((kk+stade_duree[j]),NbJours)] + 1 
    }
  }
}