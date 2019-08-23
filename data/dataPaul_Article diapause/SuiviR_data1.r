# 8 avril 2013

# ANALYSE DES DONNEES D EMERGENCE D ADULTES DE CECIDOMYIE SOUS LES TENTES DE LA
# PARCELLE TEMOIN DU CPEA

data.df <- read.table("data1.txt", header=T, sep="\t")
meteo.df <- read.table("data1_meteo.txt", header=T, sep="\t")

data.df$Date <- as.Date(data.df$Date,"%d/%m/%Y")

# "Date"      "Piege"     "Nbad"      "Bourgeons" "FlorC"     "FlorD"     "FlorE"
# [8] "FlorF"     "FlorG"     "VegC"      "VegD"      "VegE"
#[13] "VegF"      "VegG"

# Moment de mise en place du piege
# 1ere lettre du numero du piege

data.df$Intro <- substr(data.df$Piege,1,1)


# Supprime les lignes avec NA dans Nbad
res <- which(is.na(data.df$Nbad)==T)
data.df <- data.df[-res,]





library(gplots)     # pour barplot2

# Rajoute Colonne Semaine
data.df$Sem <- cut(data.df$Date,"week",labels=FALSE)

# Faire un dataframe a l echelle de la semaine
data2.df <- aggregate(data.df$Nbad, 
                      list(data.df$Piege, data.df$Sem, data.df$Intro),
                      sum,
                      simplify=F)
data2.df <- as.data.frame(data2.df)
names(data2.df) <- c("Piege", "Sem", "Intro", "Nbad")
data2.df$Sem <- as.factor(data2.df$Sem)
data2.df$Nbad <- as.numeric(data2.df$Nbad)


# Donnees de temperatures du CPEA
meteo.df[,1] <- as.Date(meteo.df[,1], format="%d/%m/%Y")
meteo.df$an <- as.factor(meteo.df$an)
# pour Saint-Paul au lyc?e agricole  LEGTA
XX <- meteo.df[meteo.df$lieu=="LEGTA",]
# Rajoute une colonne semaine
XX$sem <- cut(XX$date,"week",labels=FALSE) + 21
XX$sem <- factor(XX$sem)
# Temperature moyenne hebdomadaire
mXX <- tapply(XX$tempm, XX$sem, mean, na.rm=T)
nXX <- tapply(XX$tempn, XX$sem, mean, na.rm=T)
xXX <- tapply(XX$tempx, XX$sem, mean, na.rm=T)
rXX <- tapply(XX$rr, XX$sem, sum, na.rm=T)



################################################# Graphique avec des barplots




#tiff(filename = "test.tif", width = 2400, height = 1600,
#     units = "px", pointsize = 12,
#     compression = "lzw",
#     bg = "white", res = 300,
#     restoreConsole = TRUE)




par(mfrow=c(4,1),mar=c(4,5,1,0),cex.axis=1.4,cex.lab=1.8,las=1,oma=c(0,0,0,0))


# Piege sans inflorescence ni pousse vegetative recente
m  <- tapply(data2.df[data2.df$Intro=="A",]$Nbad,data2.df[data2.df$Intro=="A",]$Sem,mean)
sd <- tapply(data2.df[data2.df$Intro=="A",]$Nbad,data2.df[data2.df$Intro=="A",]$Sem,sd)
sd <- replace(rep(0,max(as.numeric(names(sd)))), as.numeric(names(sd)), sd)
test <- replace(rep(0,max(as.numeric(names(m)))), as.numeric(names(m)), m)
names(test) <-  c(29:52,1:52)[1:length(test)]

barplot2(log10(test+1),plot.ci=T,ci.u=log10(test+sd+1),ci.l=log10(test+1),xlab="",ylab="",ylim=c(0,2.5))
text(75,2,"A",cex=3)
abline(v=29,lty=2)
text(25,2.2,"2011",cex=1.8)
text(33,2.2,"2012",cex=1.8)

# Piege avec inflorescences infestees en juillet
m  <- tapply(data2.df[data2.df$Intro=="B",]$Nbad,data2.df[data2.df$Intro=="B",]$Sem,mean)
sd <- tapply(data2.df[data2.df$Intro=="B",]$Nbad,data2.df[data2.df$Intro=="B",]$Sem,sd)
sd <- replace(rep(0,max(as.numeric(names(sd)))),as.numeric(names(sd)),sd)
test <- replace(rep(0,max(as.numeric(names(m)))),as.numeric(names(m)),m)
names(test) <-  c(29:52,1:52)[1:length(test)]

          barplot2(log10(test+1),plot.ci=T,ci.u=log10(test+sd+1),ci.l=log10(test+1),xlab="",ylab="",ylim=c(0,2.5))
text(75,2,"B",cex=3)
par(las=0)
mtext("log10 ( Adults emerged per m2 )                          ",2,outer=T,adj=1,line=-2,cex=1.2)
par(las=1)
abline(v=29,lty=2)
text(25,2.3,"2011",cex=1.8)
text(33,2.3,"2012",cex=1.8)


# Piege avec inflorescences infestees en septembre
m  <- tapply(data2.df[data2.df$Intro=="C",]$Nbad,data2.df[data2.df$Intro=="C",]$Sem,mean)
sd <- tapply(data2.df[data2.df$Intro=="C",]$Nbad,data2.df[data2.df$Intro=="C",]$Sem,sd)
sd <- replace(rep(0,max(as.numeric(names(sd)))),as.numeric(names(sd)),sd)
test <- replace(rep(0,max(as.numeric(names(m)))),as.numeric(names(m)),m)
names(test) <-  c(29:52,1:52)[1:length(test)]

          barplot2(log10(test+1),plot.ci=T,ci.u=log10(test+sd+1),ci.l=log10(test+1),xlab="",ylab="",ylim=c(0,2.5))
text(75,2,"C",cex=3)
abline(v=29,lty=2)
text(25,2.3,"2011",cex=1.8)
text(33,2.3,"2012",cex=1.8)

plot(mXX[8:71],type="b",ylim=c(20,28),xaxt="n",xlab="Week",bty="n",ylab="Temperature")
axis(1,at=1:64,labels=as.character(c(29:52,1:40)))
abline(v=24.5,lty=2)
text(21,27.5,"2011",cex=1.8)
text(28,27.5,"2012",cex=1.8)
text(63,27.4,"D",cex=3)



