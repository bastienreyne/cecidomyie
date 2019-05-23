# 27 mars 2013

library(gplots)
library(agricolae)
library(MASS)
library(multcomp)


################################################################################
################################################################################

#            DONNEES VARIATION TEMPERATURE 2010-2012

################################################################################
################################################################################

var11.df <- read.table("data3_2011.txt", header=T, sep="\t")
tot11 <- tapply(var11.df[!duplicated(paste(var11.df$Pro,var11.df$boite)),]$nblarv,
              var11.df[!duplicated(paste(var11.df$Pro,var11.df$boite)),]$Pro,sum,na.rm=T)

# Protocole
# V1 changement precoce de temperature
# V2 changement tardif de temperature
# Nat simulation des conditions naturelles
# Cst20 temperature constante de 20°
# Cst26 temperature constante de 26°

# Format date
var11.df$datprelev <- as.Date(var11.df$datprelev,format="%d/%m/%Y")
var11.df$dattemp2 <- as.Date(var11.df$dattemp2,format="%d/%m/%Y")
var11.df$dattemp3 <- as.Date(var11.df$dattemp3,format="%d/%m/%Y")
var11.df$dateme <- as.Date(var11.df$dateme,format="%d/%m/%Y")

# Colonne semaine
var11.df$Sem <- cut(c(var11.df$datprelev[1],var11.df$dateme),"week",labels=FALSE)[-1]


# Pour 2011
# Enleve les emergences dans les 3 premieres semaines qui correspondent à des émergences immédiates sans diapause
# cad enelve les emergences <=21 jours
# Ensuite regroupe les donnees par arbre en 4 périodes
# Periode 1 : semaine 4 à 1er changement de temperature SEMAINE 6
# cut(c(var11.df$datprelev[1],var11.df$dattemp2),"week",labels=FALSE)
# Periode 2 : entre 1er et 2eme changement de temperature SEMAINE 16
# Periode 3 : entre 2eme changement de temperature et fin des manip Variations SEMAINE 21
# Periode 4 : uniquement pour les temperatures constantes: entre fin manips V et fin manip Cst SEMAINE 48


# Ne travaille pas sur simulations naturelles pour le moment
per.df <- var11.df[var11.df$Nbj>21 & var11.df$Pro!= "Nat",]
per.df$Pro <- factor(per.df$Pro)
per.df$Arb <- substr(per.df$boite,4,6)   # colonne arbre
per.df$Lot <- substr(per.df$boite,4,8)
per.df$Pctcec <- per.df$cecido/per.df$nblarv

per11.df <- per.df
c2011.df <- per11.df[per11.df$Pro=="Cst20",]
c2611.df <- per11.df[per11.df$Pro=="Cstt26",]
V111.df <- per11.df[per11.df$Pro=="V1",]
V211.df <- per11.df[per11.df$Pro=="V2",]

## Nombre d'individus en diapause par protocole
ndia.f <- function(x)
{
  sum(x[which(x$Sem>3),"cecido"])
}

ndia.f(c2011.df)
ndia.f(c2611.df)
ndia.f(V111.df)
ndia.f(V211.df)



################################################################################
################################################################################

#                      Variation temperature 2012

################################################################################
################################################################################


var12.df <- read.table("data3_2012.txt", header=T, sep="\t")
tot12 <- tapply(var12.df[!duplicated(paste(var12.df$Pro,var12.df$boite)),]$nblarv,
              var12.df[!duplicated(paste(var12.df$Pro,var12.df$boite)),]$Pro,sum,na.rm=T)

# Protocole
# V1 changement precoce de temperature
# V2 changement tardif de temperature
# Cst20 temperature constante de 20°
# Cst26 temperature constante de 26°

# Format date
var12.df$datprelev <- as.Date(var12.df$datprelev,format="%d/%m/%Y")
var12.df$dattemp2 <- as.Date(var12.df$dattemp2,format="%d/%m/%Y")
var12.df$dateme <- as.Date(var12.df$dateme,format="%d/%m/%Y")

# Colonne semaine
var12.df$Sem <- cut(c(var12.df$datprelev[1],var12.df$dateme),"week",labels=FALSE)[-1]

plot(tapply(var12.df[var12.df$Pro=="Cst20","cecido"],var12.df[var12.df$Pro=="Cst20","Sem"],sum))

# Pour 2012
# Enleve les emergences dans les 3 premieres semaines qui correspondent à des émergences immédiates sans diapause
# cad enelve les emergences <=21 jours
# Ensuite regroupe les donnees par arbre en 3 périodes
# Periode 1 : semaine 4 à 1er changement de temperature SEMAINE 9
# cut(c(var12.df$datprelev[1],var12.df$dattemp2),"week",labels=FALSE)
# Periode 2 : entre 1er et 2eme changement de temperature SEMAINE 13
# Periode 3 : entre 2eme changement de temperature et fin des manip Variations SEMAINE 21

per12.df <- var12.df[var12.df$Nbj>21,]
per12.df$Pro <- factor(per12.df$Pro)

# 1 seul arbre
# colonne boite = Lot
per12.df$Pctcec <- per12.df$cecido/per12.df$nblarv

## Fichier par Protocole
c2012.df <- split(per12.df,per12.df$Pro)[["Cst20"]]
c2612.df <- split(per12.df,per12.df$Pro)[["Cst26"]]
V112.df <- per12.df[per12.df$Pro=="V1",]
V212.df <- per12.df[per12.df$Pro=="V2",]

## Nombre d'individus en diapause par protocole
ndia.f <- function(x)
{
  sum(x[which(x$Sem>3),"cecido"])
}

ndia.f(c2012.df)
ndia.f(c2612.df)
ndia.f(V112.df)
ndia.f(V212.df)




################################################################################
################################################################################

####    GRAPHIQUES par semaine en temperature variable

################################################################################
################################################################################

# 1 graphique unique :
# 2 colonnes : Hiver / Ete
# 3 lignes : CST26, V1, V2

#tiff(filename = "FigureX_echdif.tiff", width = 1300, height = 1640 ,compression="lzw", res=100 )




par(mfcol=c(3,2),mar=c(4,4,3,1),cex=1.2)

## 2011

# Temperature constante
ess <- tapply(c2611.df$cecido,c2611.df$Sem,sum,na.rm=T)[1:7]
test <- c(replace(rep(0,21),as.numeric(names(ess)),ess),7)[4:22]  # 7 emergence apres la semaine 22
sum(test)
names(test) <-  c(4:22)

barplot(test,xlab="Week",ylab="Number of adults emerged from diapause",names.arg=c(as.character(4:21),">=22"),col="black",
        ylim=c(0,7),main="Larvae collected on winter 2011")
text(3,6.5,"Temperature constant at 26°C ( n=15 )",pos=4)

# Protocole V1
ess <- tapply(V111.df$cecido,V111.df$Sem,sum,na.rm=T)[1:8]
test <- c(replace(rep(0,21),as.numeric(names(ess)),ess),1)[4:22]  # une larve en diapause pdans le sable semaine 22
sum(test)
names(test) <-  c(4:22)

barplot(test,xlab="Week",ylab="Number of adults emerged from diapause",names.arg=c(as.character(4:21),">=22"),col="grey30",ylim=c(0,12))
arrows(3,7,3,1,length=0.15)
text(2.8,8.1,"Transfer to 20°C",pos=1)
text(19,11.5,"( n=29 )",pos=1)


# Protocole V2
ess <- tapply(V211.df$cecido,V211.df$Sem,sum,na.rm=T)
test <- c(replace(rep(0,21),as.numeric(names(ess)),ess),0)[4:22]
names(test) <-  c(4:22)

barplot(test,xlab="Week",ylab="Number of adults emerged from diapause",names.arg=c(as.character(4:21),">=22"),col="grey80",ylim=c(0,14))
arrows(15.1,8,15.1,2.5,length=0.15)
text(13.5,9.5,"Transfer to 20°C",pos=1)
text(19.5,13.5,"( n=34 )",pos=4)


## 2012
# Temperature constante
ess <- tapply(c2612.df$cecido,c2612.df$Sem,sum,na.rm=T)[1:11]
test <- c(replace(rep(0,21),as.numeric(names(ess)),ess),22)[4:22]  #
sum(test)
names(test) <-  c(4:22)

barplot(test,xlab="Week",ylab="Number of adults emerged from diapause",names.arg=c(as.character(4:21),">=22"),col="black",
        ylim=c(0,22),main="Larvae collected on summer 2012")
text(3,20,"Temperature constant at 26°C ( n=68 )",pos=4)



# Protocole V1
ess <- tapply(V112.df$cecido,V112.df$Sem,sum,na.rm=T)
test <- c(replace(rep(0,21),as.numeric(names(ess)),ess),0)[4:22]
names(test) <-  c(4:22)

barplot(test,xlab="Week",ylab="Number of adults emerged from diapause",names.arg=c(as.character(4:21),">=22"),col="grey30",ylim=c(0,25))
arrows(6.7,18,6.7,10,length=0.15)
text(7.5,20,"Transfer to 20°C",pos=1)
text(19,24,"( n=70 )",pos=4)

# Protocole V2
ess <- tapply(V212.df$cecido,V212.df$Sem,sum,na.rm=T)
test <- c(replace(rep(0,21),as.numeric(names(ess)),ess),0)[4:22]
names(test) <-  c(4:22)

barplot(test,xlab="Week",ylab="Number of adults emerged from diapause",names.arg=c(as.character(4:21),">=22"),col="grey80",ylim=c(0,14))
arrows(11.5,8,11.5,1.5,length=0.15)
text(10.5,9.5,"Transfer to 20°C",pos=1)
text(19,13,"( n=48 )",pos=4)

dev.off()






         ####################  changement de l echelle


#tiff(filename = "FigureX_MemeEch.tiff", width = 1300, height = 1640 ,compression="lzw", res=100 )




par(mfcol=c(3,2),mar=c(3,3,1,0),cex=0.9,las=1)
############################## 2011
# Temperature constante
ess <- tapply(c2611.df$cecido,c2611.df$Sem,sum,na.rm=T)[1:7]
test <- c(replace(rep(0,21),as.numeric(names(ess)),ess),7)[4:22]  # 7 emergence apres la semaine 22
sum(test)
names(test) <-  c(4:22)
barplot(test,xlab="",ylab="",names.arg=c(as.character(4:21),">=22"),col="black",
        ylim=c(0,25),main="Larvae from winter 2011",cex.main=1)
text(3,22,"26°C temperature constant",pos=4)
text(19,19,"n=15")

# Protocole V1
ess <- tapply(V111.df$cecido,V111.df$Sem,sum,na.rm=T)[1:8]
test <- c(replace(rep(0,21),as.numeric(names(ess)),ess),1)[4:22]  # une larve en diapause pdans le sable semaine 22
sum(test)
names(test) <-  c(4:22)
barplot(test,xlab="",ylab="",names.arg=c(as.character(4:21),">=22"),col="grey30",ylim=c(0,25))
par(las=0)
mtext("Number of adults emerged from diapause                                          ",2,outer=T,adj=1,line=-1,cex=1)
par(las=1)
arrows(3,13,3,1,length=0.15)
text(5,17,"Transfer to 20°C",pos=1)
text(19,24,"n=29")

# Protocole V2
ess <- tapply(V211.df$cecido,V211.df$Sem,sum,na.rm=T)
test <- c(replace(rep(0,21),as.numeric(names(ess)),ess),0)[4:22]
names(test) <-  c(4:22)
barplot(test,xlab="",ylab="",names.arg=c(as.character(4:21),">=22"),col="grey80",ylim=c(0,25))
arrows(15.1,12.5,15.1,2.5,length=0.15)
text(12.5,16,"Transfer to 20°C",pos=1)
text(19,24,"n=34")

################################## 2012
# Temperature constante
ess <- tapply(c2612.df$cecido,c2612.df$Sem,sum,na.rm=T)[1:11]
test <- c(replace(rep(0,21),as.numeric(names(ess)),ess),22)[4:22]  #
sum(test)
names(test) <-  c(4:22)
barplot(test,xlab="",ylab="",names.arg=c(as.character(4:21),">=22"),col="black",
        ylim=c(0,25),main="Larvae from summer 2012",cex.main=1)
text(3,22,"26°C temperature constant",pos=4)
text(19,19,"n=68")

# Protocole V1
ess <- tapply(V112.df$cecido,V112.df$Sem,sum,na.rm=T)
test <- c(replace(rep(0,21),as.numeric(names(ess)),ess),0)[4:22]
names(test) <-  c(4:22)
barplot(test,xlab="",ylab="Number of adults emerged from diapause",names.arg=c(as.character(4:21),">=22"),col="grey30",ylim=c(0,25))
arrows(6.7,18,6.7,10,length=0.15)
text(9.5,21.5,"Transfer to 20°C",pos=1)
text(19,24,"n=70")

# Protocole V2
ess <- tapply(V212.df$cecido,V212.df$Sem,sum,na.rm=T)
test <- c(replace(rep(0,21),as.numeric(names(ess)),ess),0)[4:22]
names(test) <-  c(4:22)
barplot(test,xlab="Week",ylab="",names.arg=c(as.character(4:21),">=22"),col="grey80",ylim=c(0,25))
arrows(11.5,13,11.5,1.5,length=0.15)
text(9,17,"Transfer to 20°C",pos=1)
text(19,24,"n=48")


mtext("Week                                                                      ",1,outer=T,line=-1)
mtext("                                                                                                 Week",1,outer=T,line=-1)



dev.off()







################################################################################
################################################################################

####    GRAPHIQUES COMBINANT 2011 et 2012 en temperature constante

################################################################################
################################################################################


## NE TRAVAILLE QUE SUR LES INDIVIDUS EN DIAPAUSE
# POURCENTAGE base sur le total d individus en diapause


# # # #  2011
ess1 <- tapply(c2011.df[,"cecido"],c2011.df[,"Sem"],sum,na.rm=T)
test1120 <- replace(rep(0,48),as.numeric(names(ess1)),ess1)[4:48]
names(test1120) <-  c(4:48)
ess2 <- tapply(c2611.df[,"cecido"],c2611.df[,"Sem"],sum,na.rm=T)
test1126 <- replace(rep(0,48),as.numeric(names(ess2)),ess2)[4:48]
names(test1126) <-  c(4:48)
# censure a 22 semaines
test1120c <- c(test1120[1:18],sum(test1120[19:45]))
names(test1120c) <- c(4:21,22)
test1126c <- c(test1126[1:18],sum(test1126[19:45]))
names(test1126c) <- c(4:21,22)



# # # #   2012
ess1 <- tapply(c2012.df[,"cecido"],c2012.df[,"Sem"],sum,na.rm=T)
test1220 <- replace(rep(0,22),as.numeric(names(ess1)),ess1)[4:22]
names(test1220) <-  c(4:22)
ess2 <- tapply(c2612.df[,"cecido"],c2612.df[,"Sem"],sum,na.rm=T)
test1226 <- c(replace(rep(0,21),as.numeric(names(ess2)),ess2)[4:21],22)  # 22 individus en diapause après tamisage
names(test1226) <-  c(4:22)




######## Graphiques

# Hiver 20°C et 26 °C
#tiff(filename = "FigureZ.tiff", width = 1300, height = 1640 ,compression="lzw", res=100 )



par(mfrow=c(2,1),mar=c(2,4,2,1),cex=1.2)

barplot(rbind(test1120c,test1126c),beside=T,xlab="",ylab="",
              ylim=c(0,45),names.arg=c(as.character(4:21),">=22"),col=c("orange","darkgreen"))
text(56,35,"A",cex=2)
legend(35,40,c("20°C ( n=71 )","26°C ( n=15 )"),fill=c("orange","darkgreen"),bty="n")
mtext("Number of adults emerged from diapause",2,outer=T,padj=2,line=0,cex=1.2)
# Ete 20°C et 26 °C
par(mar=c(4,4,0,1),cex=1.2)
barplot(rbind(test1220,test1226),beside=T,xlab="Weeks",ylab="",
              ylim=c(0,45),names.arg=c(as.character(4:21),">=22"),col=c("orange","darkgreen"))

legend(35,40,c("20°C ( n=55 )","26°C ( n=68 )"),fill=c("orange","darkgreen"),bty="n")
text(56,35,"B",cex=2)





###################################   Couleur pour ISHS

par(mar=c(4,4,2,2),cex.lab=1.4,cex.axis=1.4)
layout(matrix(c(1,2,1,2),ncol=2))
barplot(rbind(test1120c,test1126c),beside=T,xlab="Weeks",ylab="",
              ylim=c(0,45),names.arg=c(as.character(4:21),">=22"),col=c("blue","red"))
legend(35,40,c("20°C (n = 71)","26°C (n = 15)"),fill=c("blue","red"),bty="n",cex=1.4,title="Constant temperature ")

# Protocole V2
ess <- tapply(V211.df$cecido,V211.df$Sem,sum,na.rm=T)
test <- c(replace(rep(0,21),as.numeric(names(ess)),ess),0)[4:22]
names(test) <-  c(4:22)
barplot(test,xlab="",ylab="",names.arg=c(as.character(4:21),">=22"),col=c(rep("red",13),rep("blue",6)),ylim=c(0,25))
arrows(15.1,12.5,15.1,2.5,length=0.15,lwd=2)
text(12.5,16,"Transfer to 20°C",pos=1,cex=1.6)
text(19,24,"n = 34",cex=1.4)
mtext("Number of adults emerged from diapause                           ",2,outer=T,adj=1,line=-1.5,cex=1.4)



###################################   Couleur pour soutenance

par(mfcol=c(3,1),mar=c(3,4,1,0),cex=0.9,las=1)
################################## 2012
# Temperature constante
ess <- tapply(c2612.df$cecido,c2612.df$Sem,sum,na.rm=T)[1:11]
test <- c(replace(rep(0,21),as.numeric(names(ess)),ess),22)[4:22]  #
sum(test)
names(test) <-  c(4:22)
barplot(test,xlab="",ylab="",names.arg=c(as.character(4:21),">=22"),col="red",
        ylim=c(0,25),main="Larvae from summer 2012",cex.main=1)
text(3,22,"Temperature constante de 26°C",pos=4)
text(19,19,"n=68")

# Protocole V1
ess <- tapply(V112.df$cecido,V112.df$Sem,sum,na.rm=T)
test <- c(replace(rep(0,21),as.numeric(names(ess)),ess),0)[4:22]
names(test) <-  c(4:22)
barplot(test,xlab="",ylab="Nombre d'adultes émergés",names.arg=c(as.character(4:21),">=22"),col=c(rep("red",6),rep("blue",16)),ylim=c(0,25))
arrows(6.7,18,6.7,10,length=0.15)
text(8,23,"Transfert à 20°C",pos=1)
text(19,24,"n=70")

# Protocole V2
ess <- tapply(V212.df$cecido,V212.df$Sem,sum,na.rm=T)
test <- c(replace(rep(0,21),as.numeric(names(ess)),ess),0)[4:22]
names(test) <-  c(4:22)
barplot(test,xlab="Week",ylab="",names.arg=c(as.character(4:21),">=22"),col=c(rep("red",10),rep("blue",10)),ylim=c(0,25))
arrows(11.5,13,11.5,1.5,length=0.15)
text(10,17,"Transfert à 20°C",pos=1)
text(19,24,"n=48")

mtext("                 Semaine",1,outer=T,line=-1)



#############################

par(mfrow=c(2,1),mar=c(2,4,2,1),cex=1.3,las=1)

barplot(rbind(test1120c,test1220),beside=T,xlab="",ylab="",
              ylim=c(0,45),names.arg=c(as.character(4:21),">=22"),col=c("orange","darkgreen"))

legend(35,40,c("Larves d'hiver ( n=71 )","Larves d'été ( n=55 )"),fill=c("orange","darkgreen"),bty="n")
mtext("Nombre d'adultes émergés après diapause",2,outer=T,padj=2,line=0,cex=1.2,las=0)
# Ete 20°C et 26 °C
par(mar=c(4,4,0,1),cex=1.3)
barplot(rbind(test1126c,test1226),beside=T,xlab="Semaine",ylab="",
              ylim=c(0,45),names.arg=c(as.character(4:21),">=22"),col=c("orange","darkgreen"))

legend(35,40,c("Larves d'hiver ( n=68 )","Larves d'été ( n=15 )"),fill=c("orange","darkgreen"),bty="n")




