#########################BASE DE CROISSANCE INFLO

####On importe le tableau
path <- "C:/Users/Laurie/Dropbox/Cecidomyie/thermaltimeFred/BaseDeCroissanceInflo.csv"
BaseDeCroissanceInflo <- read.csv(path, h<-T, sep<-";", quote="", dec<-".")
#BaseDeCroissanceInflo[1:10,]

######On défini les variables
codeUC <- BaseDeCroissanceInflo$codeUC
saison <- BaseDeCroissanceInflo$saison
verger <-BaseDeCroissanceInflo$verger
variete <- BaseDeCroissanceInflo$variete
nature <- BaseDeCroissanceInflo$nature
arbre <- BaseDeCroissanceInflo$arbre
UCmere <- BaseDeCroissanceInflo$UCmere
nbApicale <- BaseDeCroissanceInflo$nombreApicale
nbLaterale <- BaseDeCroissanceInflo$nombreLaterale
croissance <- BaseDeCroissanceInflo$croissance
orientation <- BaseDeCroissanceInflo$orientation
hauteur <- BaseDeCroissanceInflo$hauteur
positionUC <- BaseDeCroissanceInflo$positionUC
PUCM <- BaseDeCroissanceInflo$PUCM
PUCGM <- BaseDeCroissanceInflo$PUCGM
diamUCM1 <- BaseDeCroissanceInflo$diamUCM1
diamUCM2 <- BaseDeCroissanceInflo$diamUCM2
diamAxeI <- BaseDeCroissanceInflo$diamAxeI
longueurAxeProx <- BaseDeCroissanceInflo$longueurAxeProx
date <- BaseDeCroissanceInflo$date
stadeInflo <- BaseDeCroissanceInflo$stadeInflo
longueurInflo <- BaseDeCroissanceInflo$longueurInflo
DFInflo <-BaseDeCroissanceInflo$DFInflo
ceci <- BaseDeCroissanceInflo$ceci
obs <- BaseDeCroissanceInflo$obs

nbreLigne <- nrow(BaseDeCroissanceInflo)
#nbreLigne

jose <- BaseDeCroissanceInflo$variete=="jose"
cog  <- BaseDeCroissanceInflo$variete=="cog"

#On converti nos dates dans un format compréhensible par R:

dates <- strptime(date,"%d/%m/%Y %H:%M")
BaseDeCroissanceInflo <- data.frame(BaseDeCroissanceInflo,dates)
dates <- BaseDeCroissanceInflo$dates
#BaseDeCroissanceInflo[1:10,]