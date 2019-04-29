library(tidyverse)
library(magrittr)
library(gridExtra)
library(lubridate)

b1.b <- read_csv2("/home/bastien/Stage/Moi/Data/2017_B1_bache.csv") %>% mutate(Sol = factor("Bache"))
b1.h <- read_csv2("/home/bastien/Stage/Moi/Data/2017_B1_enh.haut.csv") %>% mutate(Sol = factor("Enherbement haut"))
b1.r <- read_csv2("/home/bastien/Stage/Moi/Data/2017_B1_enh.ras.csv") %>% mutate(Sol = factor("Enherbement ras"))
b2.b <- read_csv2("/home/bastien/Stage/Moi/Data/2017_B2_bache.csv") %>% mutate(Sol = factor("Bache"))
b2.h <- read_csv2("/home/bastien/Stage/Moi/Data/2017_B2_enh.haut.csv") %>% mutate(Sol = factor("Enherbement haut"))
b2.r <- read_csv2("/home/bastien/Stage/Moi/Data/2017_B2_enh.ras.csv") %>% mutate(Sol = factor("Enherbement ras"))

b1 <- rbind(b1.b, b1.h, b1.r) %>% mutate(Bloc = factor("Bloc 1"))
b2 <- rbind(b2.b, b2.h, b2.r) %>% mutate(Bloc = factor("Bloc 2"))



#### MISE EN FORME

date <- b1.b$date
days <- as_date(date[1]:date[20])
laps <- as.numeric(c(1, date[2:20] - date[1:19]))
values_b1.b <- as_tibble(cbind(date= days, larves = rep(b1.b$larves/laps, laps), inflos_vivantes = approx(b1.b$date, b1.b$inflos_vivantes, days)$y, inflos_mortes = rep(b1.b$inflos_mortes/laps, laps) )) %>% mutate(Bloc = factor("Bloc 1"), Sol = factor("Bache"))
values_b1.h <- as_tibble(cbind(date= days, larves = rep(b1.h$larves/laps, laps), inflos_vivantes = approx(b1.b$date, b1.h$inflos_vivantes, days)$y, inflos_mortes = rep(b1.h$inflos_mortes/laps, laps) )) %>% mutate(Bloc = factor("Bloc 1"), Sol = factor("Enherbement haut"))
values_b1.r <- as_tibble(cbind(date= days, larves = rep(b1.r$larves/laps, laps), inflos_vivantes = approx(b1.b$date, b1.r$inflos_vivantes, days)$y, inflos_mortes = rep(b1.r$inflos_mortes/laps, laps) )) %>% mutate(Bloc = factor("Bloc 1"), Sol = factor("Enherbement ras"))
values_b2.b <- as_tibble(cbind(date= days, larves = rep(b2.b$larves/laps, laps), inflos_vivantes = approx(b1.b$date, b2.b$inflos_vivantes, days)$y, inflos_mortes = rep(b2.b$inflos_mortes/laps, laps) )) %>% mutate(Bloc = factor("Bloc 2"), Sol = factor("Bache"))
values_b2.h <- as_tibble(cbind(date= days, larves = rep(b2.h$larves/laps, laps), inflos_vivantes = approx(b1.b$date, b2.h$inflos_vivantes, days)$y, inflos_mortes = rep(b2.h$inflos_mortes/laps, laps) )) %>% mutate(Bloc = factor("Bloc 2"), Sol = factor("Enherbement haut"))
values_b2.r <- as_tibble(cbind(date= days, larves = rep(b2.r$larves/laps, laps), inflos_vivantes = approx(b1.b$date, b2.r$inflos_vivantes, days)$y, inflos_mortes = rep(b2.r$inflos_mortes/laps, laps) )) %>% mutate(Bloc = factor("Bloc 2"), Sol = factor("Enherbement ras"))

df <- rbind(values_b1.b, values_b1.h, values_b1.r, values_b2.b, values_b2.h, values_b2.r)
df %<>% mutate_at("date", as_date) %>% filter(date != date[1])

couleur = rep(NA, 480)
couleur[which(df$date %in% b1.b$date)] = "black"
## COMPARAISON BLOCS

l <- ggplot(df, aes(date, weight=larves, fill=Sol)) + geom_histogram(position = "identity",  binwidth = 1) + geom_rug(aes(x=date+0.5), sides = "b", colour = couleur) + facet_grid(Sol ~ Bloc) + theme_bw()+ xlab("Date") + ylab("Nombre de larves piégées")
iv <- ggplot(df, aes(date, inflos_vivantes, colour=Sol)) + geom_line(lwd=1) + facet_wrap(. ~ Bloc) + theme_bw()+ xlab("Date") + ylab("Nombre d'inflorescences vivantes observées")+ geom_rug(aes(x=date+0.5), data = b1.b, sides = "b", colour = "black")
im <- ggplot(df, aes(date, weight=inflos_mortes, fill=Sol)) + geom_histogram(position = "identity",  binwidth = 1)+ geom_rug(aes(x=date+0.5), sides = "b", colour = couleur) + facet_grid(Sol ~ Bloc) + theme_bw()+ xlab("Date") + ylab("Nombre d'inflorescences mortes")

grid.arrange(l,iv,im, nrow=3)



### COMPARAISON LARVES / ALIVES

bache <- df %>% filter(Sol=="Bache")

couleur = rep(NA, 80)
couleur[which(bache$date %in% b1.b$date)] = "black"

b <- ggplot(bache)+ geom_rug(aes(x=date+0.5), sides = "b", colour = couleur)  + geom_line(aes(x = date+0.5, y= inflos_vivantes), lwd = 1, colour = "#F8766D") + geom_histogram( mapping = aes(x = date, weight = larves), position = "identity", alpha=0.5, binwidth = 0.5, fill = "#F8766D") + facet_wrap(.~Bloc) + theme_bw() + xlab("Date") + ylab("Nombres de larves piégées et d'inflorescences vivantes")

haut <- df %>% filter(Sol=="Enherbement haut")
h <- ggplot(haut)+ geom_rug(aes(x=date+0.5), sides = "b", colour = couleur) + geom_line(aes(x = date+0.5, y= inflos_vivantes), lwd = 1, colour = "#00BA38") + geom_histogram( mapping = aes(x = date, weight = larves), position = "identity", alpha=0.5, binwidth = 1, fill = "#00BA38") + facet_wrap(.~Bloc) + theme_bw()+ xlab("Date") + ylab("Nombres de larves piégées et d'inflorescences vivantes")

ras <- df %>% filter(Sol=="Enherbement ras")
r <- ggplot(ras) + geom_rug(aes(x=date+0.5), sides = "b", colour = couleur) + geom_line(aes(x = date+0.5, y= inflos_vivantes), lwd = 1, colour = "#619CFF") + geom_histogram( mapping = aes(x = date, weight = larves), position = "identity", alpha=0.5, binwidth = 1, fill = "#619CFF") + facet_wrap(.~Bloc) + theme_bw()+ xlab("Date") + ylab("Nombres de larves piégées et d'inflorescences vivantes")

grid.arrange(b,h,r, nrow=3) 

### MOYENNE MOBILE

moving.mean <- function(x){
    L <- length(x)
    ans <- (x + c(x[2:L], NA) + c(NA, x[1:(L-1)])) / 3
    ans[1] <- (2 * x[1] + x[2]) / 3
    ans[L] <- (x[L-1] + x[L] * 2) / 3
    
    return(ans)
}

mmb1.b <- values_b1.b %>% mutate(larves = moving.mean(larves), inflos_vivantes  = moving.mean(inflos_vivantes), inflos_mortes = moving.mean(inflos_mortes)) %>% mutate(Bloc = factor("Bloc 1"))
mmb1.h <- values_b1.h %>% mutate(larves = moving.mean(larves), inflos_vivantes  = moving.mean(inflos_vivantes), inflos_mortes = moving.mean(inflos_mortes)) %>% mutate(Bloc = factor("Bloc 1"))
mmb1.r <- values_b1.r %>% mutate(larves = moving.mean(larves), inflos_vivantes  = moving.mean(inflos_vivantes), inflos_mortes = moving.mean(inflos_mortes)) %>% mutate(Bloc = factor("Bloc 1"))
mmb2.b <- values_b2.b %>% mutate(larves = moving.mean(larves), inflos_vivantes  = moving.mean(inflos_vivantes), inflos_mortes = moving.mean(inflos_mortes)) %>% mutate(Bloc = factor("Bloc 2"))
mmb2.h <- values_b2.h %>% mutate(larves = moving.mean(larves), inflos_vivantes  = moving.mean(inflos_vivantes), inflos_mortes = moving.mean(inflos_mortes)) %>% mutate(Bloc = factor("Bloc 2"))
mmb2.r <- values_b2.r %>% mutate(larves = moving.mean(larves), inflos_vivantes  = moving.mean(inflos_vivantes), inflos_mortes = moving.mean(inflos_mortes)) %>% mutate(Bloc = factor("Bloc 2"))

mm <- rbind(mmb1.b, mmb1.h, mmb1.r, mmb2.b, mmb2.h, mmb2.r) %>% mutate_at("date", as_date)

couleur = rep("white", 486)
couleur[which(mm$date %in% b1.b$date)] = "black"

mml <- ggplot(mm, aes(x = date, y=larves, colour = Sol)) + geom_rug(aes(x=date+0.5), sides = "b", colour = couleur) + geom_line(lwd=1) + facet_wrap(. ~ Bloc) + theme_bw()+ xlab("Date") + ylab("Nombre de larves piégées")
mmiv <- ggplot(mm, aes(x = date, y=inflos_vivantes, colour = Sol)) + geom_rug(aes(x=date+0.5), sides = "b", colour = couleur) + geom_line(lwd=1) + facet_wrap(. ~ Bloc) + theme_bw()+ xlab("Date") + ylab("Nombre d'inflorescences vivantes")
mmim <- ggplot(mm, aes(x = date, y=inflos_mortes, colour = Sol))+ geom_rug(aes(x=date+0.5), sides = "b", colour = couleur) + geom_line(lwd=1) + facet_wrap(. ~ Bloc) + theme_bw()+ xlab("Date") + ylab("Nombre d'inflorescences mortes")

grid.arrange(mml,mmiv,mmim, nrow=3)


## COMPARAISON ALIVE/LARVES EN MM

mmb <- mm %>% filter(Sol == "Bache")
mmh <- mm %>% filter(Sol == "Enherbement haut")
mmr <- mm %>% filter(Sol == "Enherbement ras")

couleur = rep("white", 162)
couleur[which(mmb$date %in% b1.b$date)] = "black"

mmbp <- ggplot(mmb) + geom_rug(aes(x=date+0.5), sides = "b", colour = couleur) + geom_line(aes(x=date, y = inflos_vivantes), lwd = 1, colour = "#B79F00") + geom_line(aes(date, larves), lwd = 1, colour ="#F564E3") + facet_wrap(.~Bloc) + theme_bw() +ylab("Comparaison des larves piégées et des inflosrescences observées") ##ALPHA = 4
mmhp <- ggplot(mmh) + geom_rug(aes(x=date+0.5), sides = "b", colour = couleur)  + geom_line(aes(x=date, y = inflos_vivantes), lwd = 1, colour = "#B79F00") + geom_line(aes(date, 3.3*larves), lwd = 1, colour ="#F564E3") + facet_wrap(.~Bloc) + theme_bw()+ylab("Comparaison des larves piégées et des inflosrescences observées") ##ALPHA = 3.3
mmrp <- ggplot(mmr) + geom_rug(aes(x=date+0.5), sides = "b", colour = couleur) + geom_line(aes(x=date, y = inflos_vivantes), lwd = 1, colour = "#B79F00") + geom_line(aes(date, 2*larves), lwd = 1, colour ="#F564E3") + facet_wrap(.~Bloc) + theme_bw()+ylab("Comparaison des larves piégées et des inflosrescences observées") ##ALPHA = 2

grid.arrange(mmbp, mmhp, mmrp, nrow=3)






###
alpha.star = seq(1,3,0.1)

##### DIAPO
db1 <- values_b1.b %>% mutate(larves = larves, inflos_vivantes  = moving.mean(inflos_vivantes), inflos_mortes = moving.mean(inflos_mortes)) %>% mutate(Bloc = factor("Bloc 1"))
dh1 <- values_b1.h %>% mutate(larves = larves, inflos_vivantes  = moving.mean(inflos_vivantes), inflos_mortes = moving.mean(inflos_mortes)) %>% mutate(Bloc = factor("Bloc 1"))
dr1 <- values_b1.r %>% mutate(larves = larves, inflos_vivantes  = moving.mean(inflos_vivantes), inflos_mortes = moving.mean(inflos_mortes)) %>% mutate(Bloc = factor("Bloc 1"))
db2 <- values_b2.b %>% mutate(larves = larves, inflos_vivantes  = moving.mean(inflos_vivantes), inflos_mortes = moving.mean(inflos_mortes)) %>% mutate(Bloc = factor("Bloc 2"))
dh2 <- values_b2.h %>% mutate(larves = larves, inflos_vivantes  = moving.mean(inflos_vivantes), inflos_mortes = moving.mean(inflos_mortes)) %>% mutate(Bloc = factor("Bloc 2"))
dr2 <- values_b2.r %>% mutate(larves = larves, inflos_vivantes  = moving.mean(inflos_vivantes), inflos_mortes = moving.mean(inflos_mortes)) %>% mutate(Bloc = factor("Bloc 2"))

d <- rbind(db1,dh1,dr1,db2,dh2,dr2) %>% mutate_at("date", as_date)

rugs <- rep(NA, 81)
rugs[which(d$date %in% b1.b$date)] <- "black"
ggplot(d, aes(color = Sol)) + geom_line(aes(x=date, y=inflos_vivantes), lwd = 1) + geom_point(aes(x = date, y=larves)) + geom_segment(aes(x=date, xend=date, y=0, yend=larves)) + facet_grid(Sol ~ Bloc) + theme_bw() + scale_color_viridis_d()+geom_rug(aes(x=date), color = rugs)+theme(legend.position='none') + ylab("Nombre d'inflorescences et de larves à l'échelle du sous-bloc") + xlab("Date")


rugs2 <- rep(NA, 972)
rugs2[which(d$date %in% b1.b$date)] <- "black"
m <- mm %>% gather(`larves`, `inflos_vivantes`, key = "toto", value = "Nombre")
ggplot(m, aes(x=date, y = Nombre, color = toto)) + geom_line(lwd = 1) + facet_grid(Sol ~ Bloc) + theme_bw() + scale_color_viridis_d()+geom_rug(aes(x=date), color = rugs2, sides="b")+theme(legend.title = element_blank())
