###############Chargement des fichiers de température
# Pour chaque lieu (chaque fichier), on calcule la température moyenne de deux manières
# différentes : 
## Moyenne des températures sur la journée
## (Tmin+Tmax)/2

path <- "C:/Users/Laurie/Dropbox/Cecidomyie/thermaltimeFred/VergerBassinMartin.txt"
TBM <- read.table(path, sep="\t",dec=".", h<-T)
#TBM[1:10,]
date <- vector()
date <- TBM$date
dates <- vector()
dates <- strptime(date,"%d/%m/%y")   #On transforme en date comprise par R. On ne tient pas compte des heures
TBM <- data.frame(TBM,dates)
#TBM[1:10,]
#On utilise les températures moyennes par jour: 
datesUnik <- unique(TBM$dates)
#length(datesUnik)
#datesUnik[1:10]
#On a 2 façon de calculer la moyenne : 
	#Par moyennes de toutes les températures à chaque quart d'heure pendant la journée
	#Par min+max/2 : On préviligiera l'utilisation de cette moyenne
#Calcul du premier type de moyenne:
moyBMheure <- tapply(TBM$temp,TBM$dates,mean)
#length(moyBMheure)
#moyBMheure[1:10]
#Calcul du deuxième type de moyenne:
compt <- 0
moyBM <- NA
for (i in datesUnik) {
  compt <- compt+1
  moyBM[compt] <- ((min(TBM$temp[TBM$dates==i])+max(TBM$temp[TBM$dates==i]))/2) 
  }
#length(moyBM)
#moyBM[1:10]
TBMmoy <- data.frame(datesUnik,moyBMheure,moyBM)
colnames(TBMmoy) <- c("datesUnik","tempMoyheure", "tempMoy")
TBMmoy[1:10,]



path <- "C:/Users/Laurie/Dropbox/Cecidomyie/thermaltimeFred/VergerGrandFond.txt"
TGF <- read.table(path, sep="\t",dec=".", h<-T)
#TGF[1:10,]
date <- vector()
date <- TGF$date
dates <- vector()
dates <- strptime(date,"%d/%m/%y")   #On transforme en date comprise par R
TGF <- data.frame(TGF,dates)
#TGF[1:10,]
#On utilise les températures moyennes par jour: 
datesUnik <- unique(TGF$dates)
#length(datesUnik)
#datesUnik[1:10]
#Calcul du premier type de moyenne:
moyGFheure <- tapply(TGF$temp,TGF$dates,mean)
#length(moyGFheure)
#moyGFheure[1:10]
#Calcul du deuxième type de moyenne:
compt <- 0
moyGF <- NA
for (i in datesUnik) {
  compt <- compt+1
  moyGF[compt] <- ((min(TGF$temp[TGF$dates==i])+max(TGF$temp[TGF$dates==i]))/2) 
  }
#length(moyGF)
#moyGF[1:10]
TGFmoy <- data.frame(datesUnik,moyGFheure,moyGF)
colnames(TGFmoy) <- c("datesUnik","tempMoyheure", "tempMoy")
TGFmoy[1:10,]


path <- "C:/Users/Laurie/Dropbox/Cecidomyie/thermaltimeFred/VergerSaintGillesHauts.txt"
TGH <- read.table(path, sep="\t",dec=".", h<-T)
#TGH[1:10,]
date <- vector()
date <- TGH$date
dates <- vector()
dates <- strptime(date,"%d/%m/%y")   #On transforme en date comprise par R
TGH <- data.frame(TGH,dates)
#TGH[1:10,]
#On utilise les températures moyennes par jour: 
datesUnik <- unique(TGH$dates)
#length(datesUnik)
#datesUnik[1:10]
#Calcul du premier type de moyenne:
moyGHheure <- tapply(TGH$temp,TGH$dates,mean)
#length(moyGHheure)
#moyGHheure[1:10]
#Calcul du deuxième type de moyenne:
compt <- 0
moyGH <- NA
for (i in datesUnik) {
  compt <- compt+1
  moyGH[compt] <- ((min(TGH$temp[TGH$dates==i])+max(TGH$temp[TGH$dates==i]))/2) 
  }
#length(moyGH)
#moyGH[1:10]
TGHmoy <- data.frame(datesUnik,moyGHheure,moyGH)
colnames(TGHmoy) <- c("datesUnik","tempMoyheure", "tempMoy")
#TGHmoy[1:10,]



path <- "C:/Users/Laurie/Dropbox/Cecidomyie/thermaltimeFred/VergerBassinPlat.txt"
TBP <- read.table(path, sep="\t",dec=".", h<-T)
#TBP[1:10,]
date <- vector()
date <- TBP$date
dates <- vector()
dates <- strptime(date,"%d/%m/%y")   #On transforme en date comprise par R
TBP <- data.frame(TBP,dates)
#TBP[1:10,]
#On utilise les températures moyennes par jour:
datesUnik <- unique(TBP$dates)
#length(datesUnik)
#datesUnik[1:10]
#Calcul du premier type de moyenne:
moyBPheure <- tapply(TBP$temp,TBP$dates,mean)
#length(moyBPheure)
#moyBPheure[1:10]
#Calcul du deuxième type de moyenne:
compt <- 0
moyBP <- NA
for (i in datesUnik) {
  compt <- compt+1
  moyBP[compt] <- ((min(TBP$temp[TBP$dates==i])+max(TBP$temp[TBP$dates==i]))/2) 
  }
#length(moyBP)
#moyBP[1:10]
TBPmoy <- data.frame(datesUnik,moyBPheure,moyBP)
colnames(TBPmoy) <- c("datesUnik","tempMoyheure", "tempMoy")
#TBPmoy[1:10,]



#Plot rapide pour vérifier la cohérence des températures
plot(TBMmoy$datesUnik, TBMmoy$tempMoy, col="green", ylim=c(0,40), type="o" , xlab="dates", ylab="températures (°C)", main="Evolution des températures sur les 4 vergers")
points(TGFmoy$datesUnik, TGFmoy$tempMoy, col="red", type="o")
points(TGHmoy$datesUnik, TGHmoy$tempMoy, col="blue", type="o")
points(TBPmoy$datesUnik, TBPmoy$tempMoy, col="orange", type="o")
legend("topleft", c("BM","GF","GH","BP"), col<-c("green","red","blue","orange"), pch=1) 



#########################Calcul de la durée de croissance des inflos
#n en nombre de jours depuis le 1er jour du débourrement à 00:00 au dernier jour ou la croissance s'arrete à 23:59
                        #On ne tient pas compte des heures ici

#On charge la base
source("C:/Users/Laurie/Dropbox/Cecidomyie/thermaltimeFred/Preparation Base Inflo.r")
#BaseDeCroissanceInflo[1:10,]

library(lubridate) # Pour transformer l'année en 2010 au lieu de 10 pour les dates

a <- vector()
a <- unique(BaseDeCroissanceInflo$codeUC)

nFD1UC <- vector()
nFD2UC <- vector()
nFEUC <- vector()
nPFUC <- vector()
nFFUC <- vector()


TMFD1UC <- vector()
TMFD2UC <- vector()
TMFEUC <- vector() 
TMPFUC <- vector() 
TMFFUC <- vector() 

compt <- 0

for (j in a) {             #Ouverture de la boucle
  
  compt <- compt+1

  #Création des n:

  D <- NULL          
  FD1 <- NULL  
  FD2 <- NULL  
  FE <- NULL  
  PF <- NULL 
  FF <- NULL   
       
  D <- (BaseDeCroissanceInflo$date[codeUC==j & DFInflo=="D"])
  D <- as.Date(strptime(D,"%d/%m/%Y"))   #On ne garde que les jours : du premier au dernier jour : de 00:00 à 23:59
  year(D) <- 2010                                    #Comme pour les températures: on prend les moyennes journalières
  D <- as.Date(D)
  
  #Pour FD1: la fin du stade D1
  FD1 <- (BaseDeCroissanceInflo[codeUC==j & DFInflo=="FD1",])
  FD1$date <- strptime(FD1$date,"%d/%m/%Y")
  year(FD1$date) <- 2010 
  FD1$date <- as.Date(FD1$date)
  ifelse(nrow(FD1)==0, nFD1UC[compt]<-NA, nFD1UC[compt]<-(FD1$date-D))

  #Pour FD2: la fin du stade D2 . Des fois on n'a pas pu noter cette fin
  FD2 <- (BaseDeCroissanceInflo[codeUC==j & DFInflo=="FD2",])
  FD2$date <- strptime(FD2$date,"%d/%m/%Y")
  year(FD2$date) <- 2010 
  FD2$date <- as.Date(FD2$date)
  ifelse(nrow(FD2)==0|nrow(FD1)==0, nFD2UC[compt]<-NA,nFD2UC[compt]<- (FD2$date-FD1$date))

  #Pour FE: la fin du stade E
  FE <- (BaseDeCroissanceInflo[codeUC==j & DFInflo=="FE",])
  FE$date <- strptime(FE$date,"%d/%m/%Y")
  year(FE$date) <- 2010 
  FE$date <- as.Date(FE$date)
  ifelse(nrow(FE)==0|nrow(FD2)==0, nFEUC[compt]<-NA,nFEUC[compt]<- (FE$date-FD2$date))

  #Pour la pleine floraison : ouverture de la dernière fleur
  PF <- (BaseDeCroissanceInflo[codeUC==j & BaseDeCroissanceInflo$stadeInflo=="PF",])
  PF$date <- strptime(PF$date,"%d/%m/%Y")
  year(PF$date) <- 2010 
  PF$date <- as.Date(PF$date)
  ifelse(nrow(PF)==0|nrow(FE)==0, nPFUC[compt]<-NA,nPFUC[compt]<- (PF$date-FE$date))

  #Pour FF: la fin du stade F : chute de la dernière fleur
  #Comme on passait une fois par semaine pour voir le G, pour déterminer la fin du F on prend la date de G - 1 jour
  FF <- (BaseDeCroissanceInflo[codeUC==j & BaseDeCroissanceInflo$stadeInflo=="G",])
  FF$date <- strptime(FF$date,"%d/%m/%Y")
  year(FF$date) <- 2010 
  FF$date <- as.Date(FF$date)
  ifelse(nrow(FF)==0|nrow(PF)==0,nFFUC[compt]<-NA,nFFUC[compt]<-FF$date-PF$date-3) 


  #Dans la continuité de la boucle :
  ##########Creation des températures moyennes pendant la phase de croissance :
  dataT<-NULL    #On prend que le 1er car répétitions du mm verger!
                 #On choisi le fichier de température qu'il faut selon les vergers
  if(BaseDeCroissanceInflo$verger[codeUC==j][1]=="BM") {dataT<-TBMmoy}     
  if(BaseDeCroissanceInflo$verger[codeUC==j][1]=="BP") {dataT<-TBPmoy}
  if(BaseDeCroissanceInflo$verger[codeUC==j][1]=="GH") {dataT<-TGHmoy}
  if(BaseDeCroissanceInflo$verger[codeUC==j][1]=="GF") {dataT<-TGFmoy}

  ifelse(nrow(FD1)==0,TMFD1UC[compt]<-NA,TMFD1UC[compt]<-mean(dataT$tempMoy[which(dataT$datesUnik==D+1):which(dataT$datesUnik==FD1$date)]) )
  ifelse(nrow(FD2)==0|nrow(FD1)==0,TMFD2UC[compt]<-NA,TMFD2UC[compt]<- mean(dataT$tempMoy[which(dataT$datesUnik==FD1$date+1):which(dataT$datesUnik==FD2$date)]))
  ifelse(nrow(FE)==0|nrow(FD2)==0,TMFEUC[compt]<-NA,TMFEUC[compt]<- mean(dataT$tempMoy[which(dataT$datesUnik==FD2$date+1):which(dataT$datesUnik==FE$date)]))
  ifelse(nrow(PF)==0|nrow(FE)==0,TMPFUC[compt]<-NA,TMPFUC[compt]<- mean(dataT$tempMoy[which(dataT$datesUnik==FE$date+1):which(dataT$datesUnik==PF$date)]))
  ifelse(nrow(FF)==0|nrow(PF)==0,TMFFUC[compt]<-NA,TMFFUC[compt]<- mean(dataT$tempMoy[which(dataT$datesUnik==PF$date+1):which(dataT$datesUnik==FF$date-1)]))
																																#-1 car on prend la veille du stade G					
  }   #Fermeture de la boucle



nTInflo <- data.frame(a, nFD1UC,nFD2UC,nFEUC,nPFUC,nFFUC,TMFD1UC,TMFD2UC,TMFEUC,TMPFUC,TMFFUC)
#nTInflo[1:10,]
#nTInflo

a <- vector()
a <- unique(BaseDeCroissanceInflo$codeUC)

#Ajout de variete dans le tableau:
var <- NULL
compt <- 0

for (i in a) {
  compt <- compt+1
  var[compt] <- as.character(unique(variete[codeUC==i]))   
  }
nTInflo <- data.frame(nTInflo,var)

#Ajout du verger dans le tableau
ver <- NULL
compt <- 0

for (i in a) {
  compt <- compt+1
  ver[compt] <- as.character(unique(verger[codeUC==i]))   
  }

nTInflo <- data.frame(nTInflo,ver)
#nTInflo[1:10,]
#nTInflo

#Ajout des position
pos <- NULL
compt <- 0
for (i in a) {
  compt <- compt+1
  pos[compt] <- as.character(unique(positionUC[codeUC==i]))   
  }

nTInflo <- data.frame(nTInflo,pos)
#nTInflo[1:10,]

#On rajoute aussi les longueurs et diamètres finaux
#Longueur inflo
longF <- NULL
compt <- 0
for (i in (unique(codeUC))) {
  compt <- compt+1
  ligne <- NULL
  ligne <- BaseDeCroissanceInflo[BaseDeCroissanceInflo$codeUC==i & BaseDeCroissanceInflo$DFInflo=="F",] 
  ifelse(nrow(ligne)==0,longF[compt]<-NA,longF[compt]<-ligne$longueurInflo)  
  }

#Diamètre inflo
diamInflo <- NULL
compt <- 0
for (i in a) {
  compt <- compt+1
  diamInflo[compt] <- unique(BaseDeCroissanceInflo$diamAxeI[codeUC==i])   
  }

#Longueur axe secondaire max
longAxeII <- NULL
compt <- 0
for (i in a) {
  compt <- compt+1
  longAxeII[compt] <- unique(BaseDeCroissanceInflo$longueurAxeSec[codeUC==i])   
  }

#Diamètre moyen de l'UC mère
diamUCM <- NULL
compt <- 0
for (i in a) {
  compt <- compt+1
  diamUCM[compt] <- mean(c(unique(BaseDeCroissanceInflo$diamUCM1[codeUC==i]),unique(BaseDeCroissanceInflo$diamUCM2[codeUC==i]) ))   
  }

nTInflo <- data.frame(nTInflo,longF,diamInflo,longAxeII,diamUCM)
#nTInflo[1:10,]

#Ajout des arbres
arbre <- NULL
compt <- 0
for (i in a) {
  compt <- compt+1
  arbre[compt] <- as.character(unique(BaseDeCroissanceInflo$arbre[codeUC==i]))   
  }

#Ajout du nb apicale UCmere
nbApUCm <- NULL
compt <- 0
for (i in a) {
  compt <- compt+1
  nbApUCm[compt] <- as.character(unique(BaseDeCroissanceInflo$nombreApicale[codeUC==i]))   
  }

#Ajout du nb latérales UCmere
nbLatUCm <- NULL
compt <- 0
for (i in a) {
  compt <- compt+1
  nbLatUCm[compt] <- as.numeric(as.character(unique(BaseDeCroissanceInflo$nombreLaterale[codeUC==i])))   
  }

#Ajout du niveau de croissance de l'arbre (vigeur)
vigArbre <- NULL
compt <- 0
for (i in a) {
  compt <- compt+1
  vigArbre[compt] <- as.character(unique(BaseDeCroissanceInflo$croissance[codeUC==i]))   
  }

#Ajout de l'orientation de l'UC suivie
or <- NULL
compt <- 0
for (i in a) {
  compt <- compt+1
  or[compt] <- as.character(unique(BaseDeCroissanceInflo$orientation[codeUC==i]))   
  }

#Ajout de la hauteur de l'UC suivie
haut <- NULL
compt <- 0
for (i in a) {
  compt <- compt+1
  haut[compt] <- as.numeric(as.character(unique(BaseDeCroissanceInflo$hauteur[codeUC==i])))  
  }

#Ajout de la position de l'UC mère
posUCm <- NULL
compt <- 0
for (i in a) {
  compt <- compt+1
  posUCm[compt] <- as.character(unique(BaseDeCroissanceInflo$PUCM[codeUC==i]))  
  }

#Ajout de la position de l'UC grand-mère
posUCGM <- NULL
compt <- 0
for (i in a) {
  compt <- compt+1
  posUCGM[compt] <- as.character(unique(BaseDeCroissanceInflo$PUCGM[codeUC==i]))   
  }

nTInflo <- data.frame(nTInflo,arbre,nbApUCm,nbLatUCm,vigArbre,or,haut,posUCm,posUCGM)
nTInflo[1:10,]