# 18 fevrier 2013

# regarde les emergences immediates
# A partir des fichiers source de 2010-2011


################################################################################
################################################################################

#    DONNEES 2010-2011

################################################################################
library(gplots)
library(agricolae)
library(MASS)
library(multcomp)

eme10.df <- read.table("supp_data3.txt", header=T, sep="\t")

# Prise en compte seulement
eme10.df <- eme10.df[eme10.df$boite!="GrosLarv" & eme10.df$boite!="PetitLarv",]
eme10.df <- eme10.df[-which(eme10.df$nblarv <30),]

# Enleve les mois de juin 11 et juil11 dont les suivis ont été faits en tamisant (données pour 2011-2012)
eme10.df$datprelev <- as.Date(eme10.df$datprelev,format="%d/%m/%y")
eme10.df <- eme10.df[-which(eme10.df$datprelev>"2011-06-01"),]

eme10.df$boite <- as.factor(as.character(eme10.df$boite))


# Calculer le pourcentage d'émergence d'adulte de cécidomyie en tenant compte du parasitage total
# déduit le nombre de parasites émergés du nombre total de larves
# calcule le nombre total de parasite par lot
es.l <- split(eme10.df,eme10.df$boite)
totp <- tapply(eme10.df$paras,eme10.df$boite,sum,na.rm=T)
# déduit le nombre de parasite du nombre de larves total par lot
b <- 0
for(i in names(es.l))
  {
  b <- b+1
  es.l[[i]]$nblarv <- es.l[[i]]$nblarv-totp[b]
  }
eme10.df <- do.call("rbind",es.l)


#Mettre le nombre de jour de l'emergence maximale

v0 <- rep(0,max(eme10.df$nbjr)+1)
resu <- NULL
data <- NULL

for(boite in levels(eme10.df$boite))
{
  b <- replace(v0,eme10.df[eme10.df$boite==boite,"nbjr"],eme10.df[eme10.df$boite==boite,"nbademe"])
  resu <- rbind(resu,b)
  data <- rbind(data,eme10.df[eme10.df$boite==boite,][1,c(1:11)])
}
## Ne pas tenir compte du message d erreur

eme10e.df <- cbind(data,resu)

# Nombre de diapausant par boite
eme10e.df$diap <- apply(eme10e.df[,c(32:209)],1,sum)

# Nombre d'emergence immediate
eme10e.df$imm <- apply(eme10e.df[,c(12:31)],1,sum)

# mois annee
eme10e.df$mois <- as.factor(format(eme10e.df$datprelev ,"%b%y"))

# Temperarture de 28°C
t28.df <- eme10e.df[eme10e.df$temp==28,]
res <- apply(t28.df[,c(12:52)],2,sum)
tres <- res/sum(res)
barplot(tres,ylim=c(0,1))


# Temperarture de 20°C
t20.df <- eme10e.df[eme10e.df$temp==20 & eme10e.df$datent=="18/08/10" ,]
res2 <- apply(t20.df[,c(12:52)],2,sum)
tres2 <- res2/sum(res2)
barplot(tres2,ylim=c(0,1))


# combine les deux temperatures
barplot(rbind(tres,tres2),ylim=c(0,1),ylab="Rate of emergence",xlab="Days")
legend(40,0.8,c("28°C (n=854)","20°C (n=1079)"),fill=c("grey10","grey80"),bty="n")



