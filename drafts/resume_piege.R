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

# ## LARVES B1/B2
# ggplot(b1, aes(x = date, y=larves, colour=Sol)) + geom_line(lwd=1) + theme_bw() + xlab("Date") + ylab("Estimation du nombre de larves s'éjectant des manguiers")
# ggplot(b2, aes(x = date, y=larves, colour=Sol)) + geom_line(lwd=1) + theme_bw() + xlab("Date") + ylab("Estimation du nombre de larves s'éjectant des manguiers")
# 
# ## INFLOS VIVANTES B1/B2
# ggplot(b1, aes(x = date, y=inflos_vivantes, colour=Sol)) + geom_line(lwd=1) + theme_bw() + xlab("Date") + ylab("Estimation du nombre d'inflorescences vivantes")
# ggplot(b2, aes(x = date, y=inflos_vivantes, colour=Sol)) + geom_line(lwd=1) + theme_bw() + xlab("Date") + ylab("Estimation du nombre de inflorescences vivantes")
# 
# ## INFLOS MORTES
# ggplot(b1, aes(x = date, y=inflos_mortes, colour=Sol)) + geom_line(lwd=1) + theme_bw() + xlab("Date") + ylab("Estimation du nombre de inflorescences mortes")
# ggplot(b2, aes(x = date, y=inflos_mortes, colour=Sol)) + geom_line(lwd=1) + theme_bw() + xlab("Date") + ylab("Estimation du nombre de inflorescences mortes")
# 
# 
# b <- rbind(b1,b2)
# l <- ggplot(b, aes(x = date, y=larves, colour = Sol)) + geom_line(lwd=1) + facet_wrap(. ~ Bloc) + theme_bw()+ xlab("Date") + ylab("Estimation du nombre de larves s'éjectant des manguiers")
# iv <- ggplot(b, aes(x = date, y=inflos_vivantes, colour = Sol)) + geom_line(lwd=1) + facet_wrap(. ~ Bloc) + theme_bw()+ xlab("Date") + ylab("Estimation du nombre d'inflorescences vivantes")
# im <- ggplot(b, aes(x = date, y=inflos_mortes, colour = Sol)) + geom_line(lwd=1) + facet_wrap(. ~ Bloc) + theme_bw()+ xlab("Date") + ylab("Estimation du nombre d'inflorescences mortes")
# 
# grid.arrange(l,iv,im, nrow=3)
#
# ## PALIERS
# date <- b1.b$date
# days <- as_date(date[1]:date[20])
# laps <- as.numeric(c(date[2:20] - date[1:19],1))
# palier <- rep(larves/laps, laps)
# larves <- b1.b$larves
# inflos <- rep(b1.b$inflos_vivantes/laps, laps)
# mml <- (palier + c(palier[2:81], NA) + c(NA,palier[1:80]) + c(palier[3:81], NA, NA) + c(NA, NA, palier[1:79]))/5
# toto <- as_tibble(cbind(date = days, larves = palier, mml = mml))
# ggplot(toto, aes(x=date,ymin=0, ymax = larves)) + geom_ribbon(alpha  = 0.5, fill="blue") + theme_bw() + geom_step(aes(x = date, y= inflos), fill ="green4", alpha = 0.5)
# 
# 
# tutu <- as_tibble(cbind(deb = date, end = date + laps, larves = larves / laps))
# tutu %<>% mutate_at(c("deb", "end"), list(as_date))
# ggplot(tutu, aes(x=deb, xend=end, y=larves, yend=larves)) + theme_bw() + geom_segment(lwd = 1) + geom_point()
#
# ## PROPRE
# date <- b1.b$date
# laps <- as.numeric(c(date[2:20] - date[1:19],1))
# values_b1.b <- as_tibble(cbind(date= date, end = date + laps, b1.b[,c("larves", "inflos_vivantes", "inflos_mortes")]/laps)) %>% mutate(Bloc = factor("Bloc 1"), Sol = factor("Bache"))
# values_b1.h <- as_tibble(cbind(date= date, end = date + laps, b1.h[,c("larves", "inflos_vivantes", "inflos_mortes")]/laps)) %>% mutate(Bloc = factor("Bloc 1"), Sol = factor("Enherbement haut"))
# values_b1.r <- as_tibble(cbind(date= date, end = date + laps, b1.r[,c("larves", "inflos_vivantes", "inflos_mortes")]/laps)) %>% mutate(Bloc = factor("Bloc 1"), Sol = factor("Enherbement ras"))
# values_b2.b <- as_tibble(cbind(date= date, end = date + laps, b2.b[,c("larves", "inflos_vivantes", "inflos_mortes")]/laps)) %>% mutate(Bloc = factor("Bloc 2"), Sol = factor("Bache"))
# values_b2.h <- as_tibble(cbind(date= date, end = date + laps, b2.h[,c("larves", "inflos_vivantes", "inflos_mortes")]/laps)) %>% mutate(Bloc = factor("Bloc 2"), Sol = factor("Enherbement haut"))
# values_b2.r <- as_tibble(cbind(date= date, end = date + laps, b2.r[,c("larves", "inflos_vivantes", "inflos_mortes")]/laps)) %>% mutate(Bloc = factor("Bloc 2"), Sol = factor("Enherbement ras"))
# 
# df <- rbind(values_b1.b, values_b1.h, values_b1.r, values_b1.r, values_b2.b, values_b2.h, values_b2.r)
# 
# l <- ggplot(df, aes(x=date, xend= end, y=larves, yend=larves, colour = Sol)) + geom_segment(lwd = 1) + facet_wrap(.~Bloc) + theme_bw() + xlab("Date") + ylab("Estimation du nombre de larves s'éjectant des manguiers")
# iv <- ggplot(df, aes(x=date, xend= end, y=inflos_vivantes, yend=inflos_vivantes, colour = Sol)) + geom_segment(lwd = 1) + facet_wrap(.~Bloc) + theme_bw() + xlab("Date") + ylab("Estimation du nombre d'inflorescences vivantes")
# im <- ggplot(df, aes(x=date, xend= end, y=inflos_mortes, yend=inflos_mortes, colour = Sol)) + geom_segment(lwd = 1) + facet_wrap(.~Bloc) + theme_bw() + xlab("Date") + ylab("Estimation du nombre d'inflorescences mortes")
# grid.arrange(l,iv,im, nrow=3)
# 
# # l <- ggplot(df, aes(x=date, xend= end, y=larves, yend=larves, colour = Sol)) + geom_step(lwd = 1) + facet_wrap(.~Bloc) + theme_bw() + xlab("Date") + ylab("Estimation du nombre de larves s'éjectant des manguiers")
# # iv <- ggplot(df, aes(x=date, xend= end, y=inflos_vivantes, yend=inflos_vivantes, colour = Sol)) + geom_step(lwd = 1) + facet_wrap(.~Bloc) + theme_bw() + xlab("Date") + ylab("Estimation du nombre d'inflorescences vivantes")
# # im <- ggplot(df, aes(x=date, xend= end, y=inflos_mortes, yend=inflos_mortes, colour = Sol)) + geom_step(lwd = 1) + facet_wrap(.~Bloc) + theme_bw() + xlab("Date") + ylab("Estimation du nombre d'inflorescences mortes")
# # grid.arrange(l,iv,im, nrow=3)

#### MISE EN FORME

date <- b1.b$date
days <- as_date(date[1]:date[20])
laps <- as.numeric(c(1, date[2:20] - date[1:19]))
values_b1.b <- as_tibble(cbind(date= days, larves = rep(b1.b$larves/laps, laps), inflos_vivantes = rep(b1.b$inflos_vivantes / laps, laps), inflos_mortes = rep(b1.b$inflos_mortes/laps, laps) )) %>% mutate(Bloc = factor("Bloc 1"), Sol = factor("Bache"))
values_b1.h <- as_tibble(cbind(date= days, larves = rep(b1.h$larves/laps, laps), inflos_vivantes = rep(b1.h$inflos_vivantes / laps, laps), inflos_mortes = rep(b1.h$inflos_mortes/laps, laps) )) %>% mutate(Bloc = factor("Bloc 1"), Sol = factor("Enherbement haut"))
values_b1.r <- as_tibble(cbind(date= days, larves = rep(b1.r$larves/laps, laps), inflos_vivantes = rep(b1.r$inflos_vivantes / laps, laps), inflos_mortes = rep(b1.r$inflos_mortes/laps, laps) )) %>% mutate(Bloc = factor("Bloc 1"), Sol = factor("Enherbement ras"))
values_b2.b <- as_tibble(cbind(date= days, larves = rep(b2.b$larves/laps, laps), inflos_vivantes = rep(b2.b$inflos_vivantes / laps, laps), inflos_mortes = rep(b2.b$inflos_mortes/laps, laps) )) %>% mutate(Bloc = factor("Bloc 2"), Sol = factor("Bache"))
values_b2.h <- as_tibble(cbind(date= days, larves = rep(b2.h$larves/laps, laps), inflos_vivantes = rep(b2.h$inflos_vivantes / laps, laps), inflos_mortes = rep(b2.h$inflos_mortes/laps, laps) )) %>% mutate(Bloc = factor("Bloc 2"), Sol = factor("Enherbement haut"))
values_b2.r <- as_tibble(cbind(date= days, larves = rep(b2.r$larves/laps, laps), inflos_vivantes = rep(b2.r$inflos_vivantes / laps, laps), inflos_mortes = rep(b2.r$inflos_mortes/laps, laps) )) %>% mutate(Bloc = factor("Bloc 2"), Sol = factor("Enherbement ras"))

df <- rbind(values_b1.b, values_b1.h, values_b1.r, values_b2.b, values_b2.h, values_b2.r)
df2 <- bind_rows(old = df, new = df %>% mutate(larves = lag(larves), inflos_vivantes = lag(inflos_vivantes), inflos_mortes = lag(inflos_mortes)), .id = "sourc") %>% arrange(date, sourc)
df %<>% mutate_at("date", as_date) %>% filter(date != date[1])



## COMPARAISON BLOCS

l <- ggplot(df, aes(date, weight=larves, fill=Sol)) + geom_histogram(position = "identity", alpha=0.5, binwidth = 1) + facet_wrap(. ~ Bloc) + theme_bw()+ xlab("Date") + ylab("Estimation du nombre de larves s'éjectant des manguiers")
iv <- ggplot(df, aes(date, weight=inflos_vivantes, fill=Sol)) + geom_histogram(position = "identity", alpha=0.5, binwidth = 1) + facet_wrap(. ~ Bloc) + theme_bw()+ xlab("Date") + ylab("Estimation du nombre d'inflorescences vivantes")
im <- ggplot(df, aes(date, weight=inflos_vivantes, fill=Sol)) + geom_histogram(position = "identity", alpha=0.5, binwidth = 1) + facet_wrap(. ~ Bloc) + theme_bw()+ xlab("Date") + ylab("Estimation du nombre d'inflorescences mortes")

grid.arrange(l,iv,im, nrow=3)



### COMPARAISON LARVES / ALIVES


df3 <- df %>% gather(`larves`, `inflos_vivantes`, key = "valeurs", value = "cases")

bache <- df3 %>% filter(Sol=="Bache")
b <- ggplot(bache, aes(x=date, weight = cases, fill = valeurs)) + geom_histogram(position = "identity", alpha=0.5, binwidth = 1) + facet_wrap(.~Bloc) + theme_bw() + xlab("Date") + ylab("Bache")

haut <- df3 %>% filter(Sol=="Enherbement haut")
h <- ggplot(haut, aes(x=date, weight = cases, fill = valeurs)) + geom_histogram(position = "identity", alpha=0.5, binwidth = 1) + facet_wrap(.~Bloc) + theme_bw()+ xlab("Date") + ylab("haut")

ras <- df3 %>% filter(Sol=="Enherbement ras")
r <- ggplot(ras, aes(x=date, weight = cases, fill = valeurs)) + geom_histogram(position = "identity", alpha=0.5, binwidth = 1) + facet_wrap(.~Bloc) + theme_bw()+ xlab("Date") + ylab("ras")

grid.arrange(b,h,r, nrow=3) 

### MOYENNE MOBILE

moving.mean <- function(x){
    L <- length(x)
    ans <- (x + c(x[2:L], NA) + c(NA, x[1:(L-1)])) / 3
    ans[1] <- (2 * x[1] + x[2]) / 3
    ans[L] <- (x[L-1] + x[L] * 2) / 3
    
    return(ans)
}

mmb1.b <- b1.b %>% mutate(larves = moving.mean(larves), inflos_vivantes  = moving.mean(inflos_vivantes), inflos_mortes = moving.mean(inflos_mortes)) %>% mutate(Bloc = factor("Bloc 1"))
mmb1.h <- b1.h %>% mutate(larves = moving.mean(larves), inflos_vivantes  = moving.mean(inflos_vivantes), inflos_mortes = moving.mean(inflos_mortes)) %>% mutate(Bloc = factor("Bloc 1"))
mmb1.r <- b1.r %>% mutate(larves = moving.mean(larves), inflos_vivantes  = moving.mean(inflos_vivantes), inflos_mortes = moving.mean(inflos_mortes)) %>% mutate(Bloc = factor("Bloc 1"))
mmb2.b <- b2.b %>% mutate(larves = moving.mean(larves), inflos_vivantes  = moving.mean(inflos_vivantes), inflos_mortes = moving.mean(inflos_mortes)) %>% mutate(Bloc = factor("Bloc 2"))
mmb2.h <- b2.h %>% mutate(larves = moving.mean(larves), inflos_vivantes  = moving.mean(inflos_vivantes), inflos_mortes = moving.mean(inflos_mortes)) %>% mutate(Bloc = factor("Bloc 2"))
mmb2.r <- b2.r %>% mutate(larves = moving.mean(larves), inflos_vivantes  = moving.mean(inflos_vivantes), inflos_mortes = moving.mean(inflos_mortes)) %>% mutate(Bloc = factor("Bloc 2"))

mm <- rbind(mmb1.b, mmb1.h, mmb1.r, mmb2.b, mmb2.h, mmb2.r)

mml <- ggplot(mm, aes(x = date, y=larves, colour = Sol)) + geom_line(lwd=1) + facet_wrap(. ~ Bloc) + theme_bw()+ xlab("Date") + ylab("Estimation du nombre de larves s'éjectant des manguiers")
mmiv <- ggplot(mm, aes(x = date, y=inflos_vivantes, colour = Sol)) + geom_line(lwd=1) + facet_wrap(. ~ Bloc) + theme_bw()+ xlab("Date") + ylab("Estimation du nombre d'inflorescences vivantes")
mmim <- ggplot(mm, aes(x = date, y=inflos_mortes, colour = Sol)) + geom_line(lwd=1) + facet_wrap(. ~ Bloc) + theme_bw()+ xlab("Date") + ylab("Estimation du nombre d'inflorescences mortes")

grid.arrange(mml,mmiv,mmim, nrow=3)


## COMPARAISON ALIVE/LARVES EN MM

mmb <- mm %>% filter(Sol == "Bache") %>% gather(`larves`, `inflos_vivantes`, key = "valeurs", value = "cases")
mmh <- mm %>% filter(Sol == "Enherbement haut") %>% gather(`larves`, `inflos_vivantes`, key = "valeurs", value = "cases")
mmr <- mm %>% filter(Sol == "Enherbement ras") %>% gather(`larves`, `inflos_vivantes`, key = "valeurs", value = "cases")

mmbp <- ggplot(mmb, aes(x=date, y = cases, colour = valeurs)) + geom_line(lwd = 1) + facet_wrap(.~Bloc) + theme_bw() +ylab("Bache")
mmhp <- ggplot(mmh, aes(x=date, y = cases, colour = valeurs)) + geom_line(lwd = 1) + facet_wrap(.~Bloc) + theme_bw()+ylab("haut")
mmrp <- ggplot(mmr, aes(x=date, y = cases, colour = valeurs)) + geom_line(lwd = 1) + facet_wrap(.~Bloc) + theme_bw()+ylab("ras")

grid.arrange(mmbp, mmhp, mmrp, nrow=3)