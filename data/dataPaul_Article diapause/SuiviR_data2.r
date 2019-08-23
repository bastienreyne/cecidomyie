# 27 mars 2013

library(gplots)

################################################################################
################################################################################

#    DONNEES DIAPAUSE 2011-2012

################################################################################


        eme11.df <- read.table("data2.txt", header = T, sep = "\t")
        temp.df <- read.table("data2_meteo.txt", header = T, sep = "\t")


## longueur moyenne des jours par mois
# par ordre chronologique de janvier ? d?cembre

durjour <- c(
                792.7742,
                766.5714,
                732.6774,
                697.9667,
                669.1935,
                654.9333,
                661.4194,
                685.7742,
                718.6333,
                753.7097,
                784.5667,
                800.7419
        )


# Ne prend que les expe en laboratoire
eme11L.df <- eme11.df[eme11.df$cond == "L",]

# Colonne Pourcentage de diapause (diapause sur nombre total de larves mispar lot)
eme11L.df$Pct <- eme11L.df$totlarviv / (eme11L.df$nblarv)

# Colonne Mois
eme11L.df$mois <- substr(eme11L.df$boite, 1, 3)
eme11L.df$mois <- as.factor(as.character(eme11L.df$mois))
levels(eme11L.df$mois) <-
        c(
                "juin11",
                "juil11",
                "aout11",
                "sept11",
                "janv12",
                "fev12",
                "mars12",
                "avr12",
                "avr12",
                "juin12"
        )

# Fichier du type Data_diapause11
str(eme11L.df)


#############
# Graphique barplot pour article

attach(eme11L.df)


tete <- as.data.frame(cbind(
        pour = tapply(totlarviv, mois, sum),
        nb = tapply(nblarv, mois, sum)
))
tete <-
        rbind(tete[1:4,],
              as.data.frame(cbind(
                      pour = rep(0, 3), nb = rep(0, 3)
              )),
              tete[5:8,],
              as.data.frame(cbind(pour = c(0), nb = c(0))),
              tete[9,])
tete$num <- (tete$pour / tete$nb) * (1 - tete$pour / tete$nb)
tete$errstd <- sqrt(tete$num / tete$nb)
tete$mois <- row.names(tete)


par(mar = c(3, 4, 1, 6), cex = 1.1)
barplot2(
        tete$pour / tete$nb,
        plot.ci = T,
        ci.u = (tete$pour / tete$nb) + 2 * tete$errstd,
        ci.l = (tete$pour / tete$nb) - 2 * tete$errstd,
        ylim = c(-.02, 0.49),
        names.arg = c(
                "June",
                "July",
                "August",
                "September",
                "",
                "",
                "",
                "Juanary",
                "February",
                "March",
                "April",
                "",
                "June"
        ),
        xlab = "Month",
        ylab = "Rate of larvae into diapause"
)
legend(
        0,
        0.4,
        c("Temperature", "Daylength"),
        lty = c(1, 10),
        pch = c(1, 8, 9),
        bty = "n"
)
j <- 0
for (i in c(1:4, 8:11, 13)) {
        j <- j + 1
        valeur <- c("d", "d", "c", "d", "ab", "a", "b", "c", "c")
        ypos  <- seq(0.7, 20, 1.2)
        text(ypos[i], 0.487, valeur[j])
}

j <- 0
for (i in c(1:4, 8:11, 13)) {
        j <- j + 1
        valeur <- tapply(nblarv, mois, sum)
        ypos  <- seq(0.7, 20, 1.2)
        text(ypos[i], -0.01, valeur[j])
}
text(0,-0.01, "n =")

# rajoute la temperature
par(new = T)
yy <- tapply(temp.df$tempm, paste(temp.df$mois, temp.df$an), mean,
             na.rm = T)[c(9, 7, 1, 17, 15, 13, 4, 6, 5, 12, 3, 11, 10)]
plot(
        c(0.5:12.5),
        yy,
        ylim = c(17, 26),
        xlim = c(0, 13),
        axes = F,
        xlab = "",
        ylab = "",
        type = "b"
)
axis(4, line = -1)
mtext("Temperature (°c)", 4, adj = 0.5, line = 1)

# rajoute la duree du jour
par(new = T)
jour <- c(durjour[6:12], durjour[1:6])
plot(
        c(0.5:12.5),
        jour,
        ylim = c(550, 800),
        xlim = c(0, 13),
        axes = F,
        xlab = "",
        ylab = "",
        type = "p",
        pch = 8,
        cex = 1.2
)
par(new = T)
plot(
        c(0.5:12.5),
        jour,
        ylim = c(550, 800),
        xlim = c(0, 13),
        axes = F,
        xlab = "",
        ylab = "",
        type = "b",
        pch = 19,
        lty = 10
)
axis(4, line = 3)
mtext("Daylength (minutes)", 4, adj = 0.5, line = 5)





#####  En enlevant les parasitoides

tete <- as.data.frame(cbind(
        pour = tapply(totlarviv, mois, sum),
        nb = tapply(nblarv, mois, sum) - tapply(paras, mois, sum)
))
tete <- rbind(tete[1:4,],
              as.data.frame(cbind(
                      pour = rep(0, 3),
                      nb = rep(0, 3)
              )),
              tete[5:8,],
              as.data.frame(cbind(pour = c(0), nb = c(0))),
              tete[9,])
tete$num <- (tete$pour / tete$nb) * (1 - tete$pour / tete$nb)
tete$errstd <- sqrt(tete$num / tete$nb)
tete$mois <- row.names(tete)






#tiff("Seasonal_rate_diapause_nblarvssParas.tiff",width=1440,height=960,res=100)

postscript("Figure2.eps", height = 6, width = 4)

par(mar = c(3, 4, 1, 6),
    cex = 1.3,
    las = 1)
barplot2(
        tete$pour / tete$nb,
        plot.ci = T,
        ci.u = (tete$pour / tete$nb) + 2 * tete$errstd,
        ci.l = (tete$pour / tete$nb) - 2 * tete$errstd,
        ylim = c(-.02, 0.49),
        names.arg = c(
                "Jun",
                "Jul",
                "Aug",
                "Sep",
                "",
                "",
                "",
                "Jan",
                "Feb",
                "Mar",
                "Apr",
                "",
                " Jun"
        ),
        xlab = "Month",
        ylab = "Rate of diapause"
)
legend(
        0,
        0.4,
        c("Temperature", "Day length"),
        lty = c(1, 10),
        pch = c(1, 8, 9),
        bty = "n"
)
j <- 0
for (i in c(1:4, 8:11, 13)) {
        j <- j + 1
        valeur <- c("d", "d", "c", "d", "ab", "a", "b", "c", "c")
        xpos  <- seq(0.7, 20, 1.2)
        ypos <-
                c(
                        0.05706527,
                        0.04825663,
                        0.15232907,
                        0.04407272,
                        0.34179349,
                        0.36357806,
                        0.24927134,
                        0.14346253,
                        0.10760459
                ) + 0.015
        text(xpos[i], ypos[j], valeur[j])
}

j <- 0
for (i in c(1:4, 8:11, 13)) {
        j <- j + 1
        valeur <- tapply(nblarv, mois, sum)
        ypos  <- seq(0.7, 20, 1.2)
        text(ypos[i],-0.01, valeur[j])
}
text(0,-0.01, "n =")

# rajoute la temperature
par(new = T)
yy <- tapply(temp.df$tempm, paste(temp.df$mois, temp.df$an), mean,
             na.rm = T)[c(9, 7, 1, 17, 15, 13, 4, 6, 5, 12, 3, 11, 10)]
plot(
        c(0.5:12.5),
        yy,
        ylim = c(17, 26),
        xlim = c(0, 13),
        axes = F,
        xlab = "",
        ylab = "",
        type = "b"
)
axis(4, line = -1)
par(las = 0)
mtext("Temperature (°c)",
      4,
      adj = 0.5,
      line = 1,
      cex = 1.4)
par(las = 1)
# rajoute la duree du jour
par(new = T, las = 0)
jour <- c(durjour[6:12], durjour[1:6])
plot(
        c(0.5:12.5),
        jour,
        ylim = c(550, 800),
        xlim = c(0, 13),
        axes = F,
        xlab = "",
        ylab = "",
        type = "p",
        pch = 8,
        cex = 1.2
)
par(new = T)
plot(
        c(0.5:12.5),
        jour,
        ylim = c(550, 800),
        xlim = c(0, 13),
        axes = F,
        xlab = "",
        ylab = "",
        type = "b",
        pch = 19,
        lty = 10
)
axis(4, line = 3)
mtext("Day length (minutes)",
      4,
      adj = 0.5,
      line = 5,
      cex = 1.4)
par(las = 1)


dev.off()







detach(eme11L.df)

# nombre d arbre par echantillonne par mois
names(eme11.df)
tapply(eme11L.df$arbre, eme11L.df$mois, unique)



########################################   En couleur

postscript("Figure2_couleur.eps", height = 6, width = 4)

par(
        mar = c(3, 4, 1, 6),
        cex = 1.3,
        las = 1,
        col.axis = "black",
        col = "black"
)
barplot2(
        tete$pour / tete$nb,
        plot.ci = T,
        ci.u = (tete$pour / tete$nb) + 2 * tete$errstd,
        ci.l = (tete$pour / tete$nb) - 2 * tete$errstd,
        ylim = c(-.02, 0.49),
        names.arg = c(
                "Jun",
                "Jul",
                "Aug",
                "Sep",
                "",
                "",
                "",
                "Jan",
                "Feb",
                "Mar",
                "Apr",
                "",
                "Jun"
        ),
        xlab = "Month",
        ylab = "Rate of diapause",
        border = "black",
        axes = F
)
axis(2, line = 0, col = "black")
legend(
        0,
        0.4,
        c("Temperature", "Day length") ,
        lty = c(1, 10),
        bty = "n",
        col = c("blue", "red"),
        text.col = c("blue", "red")
)
j <- 0
for (i in c(1:4, 8:11, 13)) {
        j <- j + 1
        valeur <- c("d", "d", "c", "d", "ab", "a", "b", "c", "c")
        xpos  <- seq(0.7, 20, 1.2)
        ypos <-
                c(
                        0.05706527,
                        0.04825663,
                        0.15232907,
                        0.04407272,
                        0.34179349,
                        0.36357806,
                        0.24927134,
                        0.14346253,
                        0.10760459
                ) + 0.015
        text(xpos[i], ypos[j], valeur[j])
}

j <- 0
for (i in c(1:4, 8:11, 13))
{
        j <- j + 1
        valeur <- tapply(nblarv, mois, sum)
        ypos  <- seq(0.7, 20, 1.2)
        text(ypos[i], -0.01, valeur[j])
}
text(0, -0.01, "n =")

# rajoute la temperature
par(new = T, col.axis = "blue")
yy <-
        tapply(temp.df$tempm, paste(temp.df$mois, temp.df$an), mean, na.rm = T)[c(9, 7, 1, 17, 15, 13, 4, 6, 5, 12, 3, 11, 10)]
plot(
        c(0.5:12.5),
        yy,
        ylim = c(17, 26),
        xlim = c(0, 13),
        axes = F,
        xlab = "",
        ylab = "",
        type = "b",
        col = "blue",
        pch = "!"
)
axis(4, line = -1, col = "blue")
par(las = 0)
mtext(
        "Temperature (?c)",
        4,
        adj = 0.5,
        line = 1,
        cex = 1.4,
        col = "blue"
)
par(las = 1)
# rajoute la duree du jour
par(new = T,
    las = 0,
    col.axis = "red")
jour <- c(durjour[6:12], durjour[1:6])
plot(
        c(0.5:12.5),
        jour,
        ylim = c(550, 800),
        xlim = c(0, 13),
        axes = F,
        xlab = "",
        ylab = "",
        type = "p",
        pch = 8,
        cex = 1.2,
        col = "red"
)
par(new = T)
plot(
        c(0.5:12.5),
        jour,
        ylim = c(550, 800),
        xlim = c(0, 13),
        axes = F,
        xlab = "",
        ylab = "",
        type = "b",
        pch = 19,
        lty = 10,
        col = "red"
)
axis(4, line = 3, col = "red")
mtext(
        "Day length (minutes)",
        4,
        adj = 0.5,
        line = 5,
        cex = 1.4,
        col = "red"
)
par(las = 1)


dev.off()
