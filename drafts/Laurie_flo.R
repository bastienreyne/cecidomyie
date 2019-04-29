# library (plyr)
library (dplyr)
library (readr)
##library (xlsxjars)
##library (xlsx)
##library (rJava)
library(lubridate)
#library (float)
library(tidyverse)
library(magrittr)


databis <- read_csv2("/home/bastien/Stage/Moi/donnees.floraison.csv")
Bloc=1
Trait=0

data1 = databis
if (Bloc!=0)
    data1 = data1[data1$Bloc==Bloc,]
if (Trait!=0)
    data1 = data1[data1$Traitm==Modalité,]
# On ne garde que les lignes contenant des dates de naissance d'inflos 
# (c'est à dire les lignes contenant une date de stade C)
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

##data_[,1] = as.Date(data_[,1], '%d/%m')
##data_[,2] = as.Date(data_[,2], '%d/%m')
data_ %<>% mutate_at(c("birth", "dead"), list(as.Date), "%d/%m/%Y")

duree = cumsum(c(7,9,34))

z = data_[,1] + duree[3] - 1

diff = z - data_[,2]

data_ = data.frame(data_,z,diff)

date_ = sort(unique(c(data_[,1],data_[,2],as.Date("06/10","%d/%m"))))
inflos_vivantes = array(0,length(date_))
inflos_mortes = array(0,length(date_))
inflos_nouvelles = array(0,length(date_))
inflos_vivantes_theorie = array(0,length(date_))
inflos_mortes_theorie = array(0,length(date_))
inflos_CD_theo = array(0,length(date_))
inflos_E_theo = array(0,length(date_))
inflos_F_theo = array(0,length(date_))
for (i in 1:dim(data_)[1]) {
    
    j = 1
    while (data_[i,1]>date_[j])
        j = j+1
    inflos_nouvelles[j] = inflos_nouvelles[j] + 1
    if (is.na(data_[i,2])) {
        inflos_vivantes[j:length(date_)] = inflos_vivantes[j:length(date_)] + 1
    }
    else {
        k = j 
        while (data_[i,2]>date_[k])
            k = k+1
        inflos_vivantes[j:k] = inflos_vivantes[j:k] + 1
        inflos_mortes[(k+1):length(date_)] = inflos_mortes[(k+1):length(date_)] + 1
    }
    
    if (data_[i,3]>date_[length(date_)]) {
        inflos_vivantes_theorie[j:length(date_)] = inflos_vivantes_theorie[j:length(date_)] + 1
        if ((j+duree[1]-1)<length(date_)) {
            inflos_CD_theo[j:(j+duree[1]-1)] = inflos_CD_theo[j:(j+duree[1]-1)] + 1
            if ((j+duree[2]-1)<length(date_)) {
                inflos_E_theo[(j+duree[1]):(j+duree[2]-1)] = inflos_E_theo[(j+duree[1]):(j+duree[2]-1)] + 1
                inflos_F_theo[(j+duree[2]):length(date_)] = inflos_F_theo[(j+duree[2]):length(date_)] + 1
            }
            else {
                inflos_E_theo[(j+duree[1]):length(date_)] = inflos_E_theo[(j+duree[1]):length(date_)] + 1
            }
        }
        else {
            inflos_CD_theo[j:length(date_)] = inflos_CD_theo[j:length(date_)] + 1
        }
    }
    else {
        k = j 
        while (data_[i,3]>date_[k])
            k = k+1
        inflos_vivantes_theorie[j:k] = inflos_vivantes_theorie[j:k] + 1
        inflos_CD_theo[j:(j+duree[1]-1)] = inflos_CD_theo[j:(j+duree[1]-1)] + 1
        inflos_E_theo[(j+duree[1]):(j+duree[2]-1)] = inflos_E_theo[(j+duree[1]):(j+duree[2]-1)] + 1
        inflos_F_theo[(j+duree[2]):k] = inflos_F_theo[(j+duree[2]):k] + 1
        inflos_mortes_theorie[(k+1):length(date_)] = inflos_mortes_theorie[(k+1):length(date_)] + 1
    }
}
inflos_CD_theo = inflos_CD_theo[1:length(date_)]
inflos_E_theo = inflos_E_theo[1:length(date_)]
inflos_F_theo = inflos_F_theo[1:length(date_)]
inflos_mortes = inflos_mortes[1:length(date_)]
inflos_mortes_theorie = inflos_mortes_theorie[1:length(date_)]


