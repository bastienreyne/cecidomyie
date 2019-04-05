#Mise en relation de la température moyenne pendant la croissance et n

source("C:/Users/Laurie/Dropbox/Cecidomyie/thermaltimeFred/creation base nTInflo dvlpt2.r")
nT <- nTInflo[,1:13]
nT[1:10,]

range(nT[,c("TMFD1UC","TMFD2UC","TMFEUC","TMPFUC","TMFFUC")],na.rm=T)
range(nT[nT$var=='cog','nFD1UC'],na.rm=T)
range(nT[nT$var=='cog','nFD2UC'],na.rm=T)
range(nT[nT$var=='cog','nFEUC'],na.rm=T)
range(nT[nT$var=='cog','nPFUC'],na.rm=T)
range(nT[nT$var=='cog','nFFUC'],na.rm=T)

windows()
par(mfrow=c(2,3))

# stade D1
plot(nFD1UC~TMFD1UC,data=nT[nT$var=="cog",],ylim=c(0,10),xlim=c(15,30),pch=21,col=1,bty="n",main="Cogshall",ylab="Duree du stade D1 (jour)",xlab="Temperature moyenne (°C)",cex.axis=1,cex.lab=1)
fitD1 <- lm(nFD1UC~TMFD1UC,data=nT[nT$var=="cog",])
abline(a=fitD1$coef[1],b=fitD1$coef[2],lty="dashed")
summary(fitD1)
text(x=25,y=10,"y = 14.80 - 0.46 * Tmoy",cex=1)

# stade D2
plot(nFD2UC~TMFD2UC,data=nT[nT$var=="cog",],ylim=c(0,6),xlim=c(15,30),pch=21,col=1,bty="n",main="Cogshall",ylab="Duree du stade D2 (jour)",xlab="Temperature moyenne (°C)",cex.axis=1,cex.lab=1)
fitD2 <- lm(nFD2UC~TMFD2UC,data=nT[nT$var=="cog",])
abline(a=fitD2$coef[1],b=fitD2$coef[2],lty="dashed")
summary(fitD2)
text(x=25,y=6,"y = 6.04 - 0.18 * Tmoy",cex=1)

# stade E
plot(nFEUC~TMFEUC,data=nT[nT$var=="cog",],ylim=c(0,20),xlim=c(15,30),pch=21,col=1,bty="n",main="Cogshall",ylab="Duree du stade E (jour)",xlab="Temperature moyenne (°C)",cex.axis=1,cex.lab=1)
fitE <- lm(nFEUC~TMFEUC,data=nT[nT$var=="cog",])
abline(a=fitE$coef[1],b=fitE$coef[2],lty="dashed")
summary(fitE)
text(x=25,y=20,"y = 25.40 - 0.67 * Tmoy",cex=1)

# stade F (PF)
plot(nPFUC~TMPFUC,data=nT[nT$var=="cog",],ylim=c(0,50),xlim=c(15,30),pch=21,col=1,bty="n",main="Cogshall",ylab="Duree du stade PF (jour)",xlab="Temperature moyenne (°C)",cex.axis=1,cex.lab=1)
fitPF <- lm(nPFUC~TMPFUC,data=nT[nT$var=="cog",])
abline(a=fitPF$coef[1],b=fitPF$coef[2],lty="dashed")
summary(fitPF)
text(x=25,y=50,"y = 78.15 - 2.49 * Tmoy",cex=1)

# stade F (FF)
plot(nFFUC~TMFFUC,data=nT[nT$var=="cog",],ylim=c(0,25),xlim=c(15,30),pch=21,col=1,bty="n",main="Cogshall",ylab="Duree du stade FF (jour)",xlab="Temperature moyenne (°C)",cex.axis=1,cex.lab=1)
fitFF <- lm(nFFUC~TMFFUC,data=nT[nT$var=="cog",])
abline(a=fitFF$coef[1],b=fitFF$coef[2],lty="dashed")
summary(fitFF)
text(x=25,y=25,"y = 31.10 - 0.96 * Tmoy",cex=1)


predict(fitD1,newdata=data.frame(TMFD1UC=22.5))
predict(fitD2,newdata=data.frame(TMFD2UC=22))
predict(fitE,newdata=data.frame(TMFEUC=22))
predict(fitPF,newdata=data.frame(TMPFUC=22.5))
predict(fitFF,newdata=data.frame(TMFFUC=22))


par(mfrow=c(4,2))
plot(nFDUC~TMFDUC,data=nT[nT$vari=="cog",],ylim=c(0,10),xlim=c(15,30),pch=21,col=1,bty="n",main="Cogshall",ylab="Durée du stade D (jour)",xlab="Température moyenne (°C)",cex.axis=1.5,cex.lab=1.5)
abline(a=lm(nFDUC~TMFDUC,data=nT[nT$vari=="cog",])$coef[1],b=lm(nFDUC~TMFDUC,data=nT[nT$vari=="cog",])$coef[2],lty="dashed")
summary(lm(nFDUC~TMFDUC,data=nT[nT$vari=="cog",]))
text(x=22,y=9.5,"S = -0.31, R² = 0.50, P < 0.001",cex=1.5)
plot(nFDUC~TMFDUC,data=nT[nT$vari=="jose",],ylim=c(0,10),xlim=c(15,30),pch=16,bty="n",main="José",ylab="Durée du stade D (jour)",xlab="Température moyenne (°C)",cex.axis=1.5,cex.lab=1.5)
abline(a=lm(nFDUC~TMFDUC,data=nT[nT$vari=="jose",])$coef[1],b=lm(nFDUC~TMFDUC,data=nT[nT$vari=="jose",])$coef[2])
summary(lm(nFDUC~TMFDUC,data=nT[nT$vari=="jose",]))
text(x=22,y=9.5,"S = -0.40, R² = 0.44, P < 0.001",cex=1.5)

plot(nFEUC~TMFEUC,data=nT[nT$vari=="cog",],ylim=c(0,10),xlim=c(15,30),pch=21,col=1,bty="n",main="Cogshall",ylab="Durée du stade E (jour)",xlab="Température moyenne (°C)",cex.axis=1.5,cex.lab=1.5)
abline(a=lm(nFEUC~TMFEUC,data=nT[nT$vari=="cog",])$coef[1],b=lm(nFEUC~TMFEUC,data=nT[nT$vari=="cog",])$coef[2],lty="dashed")
summary(lm(nFEUC~TMFEUC,data=nT[nT$vari=="cog",]))
text(x=22,y=9.5,"S = -0.22, R² = 0.13, P < 0.001",cex=1.5)
plot(nFEUC~TMFEUC,data=nT[nT$vari=="jose",],ylim=c(0,10),xlim=c(15,30),pch=16,bty="n",main="José",ylab="Durée du stade E (jour)",xlab="Température moyenne (°C)",cex.axis=1.5,cex.lab=1.5)
abline(a=lm(nFEUC~TMFEUC,data=nT[nT$vari=="jose",])$coef[1],b=lm(nFEUC~TMFEUC,data=nT[nT$vari=="jose",])$coef[2])
summary(lm(nFEUC~TMFEUC,data=nT[nT$vari=="jose",]))
text(x=22,y=9.5,"S = -0.41, R² = 0.38, P < 0.001",cex=1.5)

plot(nFFUC~TMFFUC,data=nT[nT$vari=="cog",],ylim=c(0,10),xlim=c(15,30),pch=21,col=1,bty="n",main="Cogshall",ylab="Durée du stade F (jour)",xlab="Température moyenne (°C)",cex.axis=1.5,cex.lab=1.5)
abline(a=lm(nFFUC~TMFFUC,data=nT[nT$vari=="cog",])$coef[1],b=lm(nFFUC~TMFFUC,data=nT[nT$vari=="cog",])$coef[2],lty="dashed")
summary(lm(nFFUC~TMFFUC,data=nT[nT$vari=="cog",]))
text(x=22,y=9.5,"S = -0.47,R² = 0.44, P < 0.001",cex=1.5)
plot(nFFUC~TMFFUC,data=nT[nT$vari=="jose",],ylim=c(0,10),xlim=c(15,30),pch=16,bty="n",main="José",ylab="Durée du stade F (jour)",xlab="Température moyenne (°C)",cex.axis=1.5,cex.lab=1.5)
abline(a=lm(nFFUC~TMFFUC,data=nT[nT$vari=="jose",])$coef[1],b=lm(nFFUC~TMFFUC,data=nT[nT$vari=="jose",])$coef[2])
summary(lm(nFFUC~TMFFUC,data=nT[nT$vari=="jose",]))
text(x=22,y=9.5,"S = -0.21, R² = 0.13, P = 0.001",cex=1.5)

plot(nFGUC~TMFGUC,data=nT[nT$vari=="cog"& nT$ver=="BP"& nT$sai=="4",],ylim=c(0,40),xlim=c(15,30),pch=16,col="grey",bty="n",main="Cogshall",ylab="Durée du stade G (jour)",xlab="Température moyenne (°C)",cex.axis=1.5,cex.lab=1.5)
nTbis<-nT[-which(nT$vari=="cog"& nT$ver=="BP"& nT$sai=="4"),]
points(nFGUC~TMFGUC,data=nTbis[nTbis$vari=="cog",],pch=21,col=1)
summary(lm(nFGUC~TMFGUC,data=nTbis[nTbis$vari=="cog",]))
abline(a=lm(nFGUC~TMFGUC,data=nTbis[nTbis$vari=="cog",])$coef[1],b=lm(nFGUC~TMFGUC,data=nTbis[nTbis$vari=="cog",])$coef[2],lty="dashed")
text(x=22,y=3,"S = -1.75, R² = 0.63, P < 0.001",cex=1.5)
plot(nFGUC~TMFGUC,data=nT[nT$vari=="jose",],ylim=c(0,40),xlim=c(15,30),pch=16,bty="n",main="José",ylab="Durée du stade G (jour)",xlab="Température moyenne (°C)",cex.axis=1.5,cex.lab=1.5)
abline(a=lm(nFGUC~TMFGUC,data=nT[nT$vari=="jose",])$coef[1],b=lm(nFGUC~TMFGUC,data=nT[nT$vari=="jose",])$coef[2])
summary(lm(nFGUC~TMFGUC,data=nT[nT$vari=="jose",]))
text(x=22,y=3,"S = -2.42, R² = 0.50, P < 0.001",cex=1.5)


source("creation base nTInflo dvlpt2.r")
nTInflo[1:10,]

par(mfrow=c(3,2))
plot(nFD1UC~TMFD1UC,data=nTInflo[nTInflo$var=="cog",],ylim=c(0,15),xlim=c(15,30),pch=21,col=1,bty="n",main="Cogshall",ylab="Durée du stade D1 (jour)",xlab="Température moyenne (°C)",cex.axis=1.5,cex.lab=1.5)
abline(a=lm(nFD1UC~TMFD1UC,data=nTInflo[nTInflo$var=="cog",])$coef[1],b=lm(nFD1UC~TMFD1UC,data=nTInflo[nTInflo$var=="cog",])$coef[2],lty="dashed")
summary(lm(nFD1UC~TMFD1UC,data=nTInflo[nTInflo$var=="cog",]))
text(x=22,y=14.5,"S = -0.46, R² = 0.22, P < 0.001",cex=1.5)
plot(nFD1UC~TMFD1UC,data=nTInflo[nTInflo$var=="jose",],ylim=c(0,15),xlim=c(15,30),pch=16,bty="n",main="José",ylab="Durée du stade D1 (jour)",xlab="Température moyenne (°C)",cex.axis=1.5,cex.lab=1.5)
abline(a=lm(nFD1UC~TMFD1UC,data=nTInflo[nTInflo$var=="jose",])$coef[1],b=lm(nFD1UC~TMFD1UC,data=nTInflo[nTInflo$var=="jose",])$coef[2])
summary(lm(nFD1UC~TMFD1UC,data=nTInflo[nTInflo$var=="jose",]))
text(x=22,y=14.5,"S = -1.00, R² = 0.42, P < 0.001",cex=1.5)

plot(nFD2UC~TMFD2UC,data=nTInflo[nTInflo$var=="cog",],ylim=c(0,15),xlim=c(15,30),pch=21,col=1,bty="n",main="Cogshall",ylab="Durée du stade D2 (jour)",xlab="Température moyenne (°C)",cex.axis=1.5,cex.lab=1.5)
abline(a=lm(nFD2UC~TMFD2UC,data=nTInflo[nTInflo$var=="cog",])$coef[1],b=lm(nFD2UC~TMFD2UC,data=nTInflo[nTInflo$var=="cog",])$coef[2],lty="dashed")
summary(lm(nFD2UC~TMFD2UC,data=nTInflo[nTInflo$var=="cog",]))
text(x=22,y=14.5,"S = -0.18, R² = 0.22, P = 0.001",cex=1.5)
plot(nFD2UC~TMFD2UC,data=nTInflo[nTInflo$var=="jose",],ylim=c(0,15),xlim=c(15,30),pch=16,bty="n",main="José",ylab="Durée du stade D2 (jour)",xlab="Température moyenne (°C)",cex.axis=1.5,cex.lab=1.5)
#abline(a=lm(nFD2UC~TMFD2UC,data=nTInflo[nTInflo$var=="jose",])$coef[1],b=lm(nFD2UC~TMFD2UC,data=nTInflo[nTInflo$var=="jose",])$coef[2])
summary(lm(nFD2UC~TMFD2UC,data=nTInflo[nTInflo$var=="jose",]))
#NS à 0.03

plot(nFEUC~TMFEUC,data=nTInflo[nTInflo$var=="cog",],ylim=c(0,20),xlim=c(15,30),pch=21,col=1,bty="n",main="Cogshall",ylab="Durée du stade E (jour)",xlab="Température moyenne (°C)",cex.axis=1.5,cex.lab=1.5)
abline(a=lm(nFEUC~TMFEUC,data=nTInflo[nTInflo$var=="cog",])$coef[1],b=lm(nFEUC~TMFEUC,data=nTInflo[nTInflo$var=="cog",])$coef[2],lty="dashed")
summary(lm(nFEUC~TMFEUC,data=nTInflo[nTInflo$var=="cog",]))
text(x=22,y=1,"S = -0.67, R² = 0.31, P < 0.001",cex=1.5)
plot(nFEUC~TMFEUC,data=nTInflo[nTInflo$var=="jose",],ylim=c(0,20),xlim=c(15,30),pch=16,bty="n",main="José",ylab="Durée du stade E (jour)",xlab="Température moyenne (°C)",cex.axis=1.5,cex.lab=1.5)
abline(a=lm(nFEUC~TMFEUC,data=nTInflo[nTInflo$var=="jose",])$coef[1],b=lm(nFEUC~TMFEUC,data=nTInflo[nTInflo$var=="jose",])$coef[2])
summary(lm(nFEUC~TMFEUC,data=nTInflo[nTInflo$var=="jose",]))
text(x=22,y=1,"S = -1.36, R² = 0.64, P < 0.001",cex=1.5)

plot(nPFUC~TMPFUC,data=nTInflo[nTInflo$var=="cog",],ylim=c(0,40),xlim=c(15,30),pch=21,col=1,bty="n",main="Cogshall",ylab="Durée de début F à PF (jour)",xlab="Température moyenne (°C)",cex.axis=1.5,cex.lab=1.5)
abline(a=lm(nPFUC~TMPFUC,data=nTInflo[nTInflo$var=="cog",])$coef[1],b=lm(nPFUC~TMPFUC,data=nTInflo[nTInflo$var=="cog",])$coef[2],lty="dashed")
summary(lm(nPFUC~TMPFUC,data=nTInflo[nTInflo$var=="cog",]))
text(x=22,y=2,"S = -2.48, R² = 0.51, P < 0.001",cex=1.5)
plot(nPFUC~TMPFUC,data=nTInflo[nTInflo$var=="jose",],ylim=c(0,40),xlim=c(15,30),pch=16,bty="n",main="José",ylab="Durée de début F à PF (jour)",xlab="Température moyenne (°C)",cex.axis=1.5,cex.lab=1.5)
abline(a=lm(nPFUC~TMPFUC,data=nTInflo[nTInflo$var=="jose",])$coef[1],b=lm(nPFUC~TMPFUC,data=nTInflo[nTInflo$var=="jose",])$coef[2])
summary(lm(nPFUC~TMPFUC,data=nTInflo[nTInflo$var=="jose",]))
text(x=22,y=2,"S = -2.65, R² = 0.38, P < 0.001",cex=1.5)

#Pour avoir la durée du stade F il faut aditionner nPFUC et nFFUC 
nTInflo$nF<-nTInflo$nPFUC + nTInflo$nFFUC 
nTInflo$TF<-(nTInflo$TMPFUC + nTInflo$TMFFUC )/2

plot(nF~TF,data=nTInflo[nTInflo$var=="cog",],ylim=c(0,65),xlim=c(15,30),pch=21,col=1,bty="n",main="Cogshall",ylab="Durée du stade F (jour)",xlab="Température moyenne (°C)",cex.axis=1.5,cex.lab=1.5)
abline(a=lm(nF~TF,data=nTInflo[nTInflo$var=="cog",])$coef[1],b=lm(nF~TF,data=nTInflo[nTInflo$var=="cog",])$coef[2],lty="dashed")
summary(lm(nF~TF,data=nTInflo[nTInflo$var=="cog",]))
text(x=22,y=2,"S = -3.77, R² = 0.57, P < 0.001",cex=1.5)
plot(nF~TF,data=nTInflo[nTInflo$var=="jose",],ylim=c(0,65),xlim=c(15,30),pch=16,bty="n",main="José",ylab="Durée du stade F (jour)",xlab="Température moyenne (°C)",cex.axis=1.5,cex.lab=1.5)
abline(a=lm(nF~TF,data=nTInflo[nTInflo$var=="jose",])$coef[1],b=lm(nF~TF,data=nTInflo[nTInflo$var=="jose",])$coef[2])
summary(lm(nF~TF,data=nTInflo[nTInflo$var=="jose",]))
text(x=22,y=2,"S = -3.55, R² = 0.32, P < 0.001",cex=1.5)
