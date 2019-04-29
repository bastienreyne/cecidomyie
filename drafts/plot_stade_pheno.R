
# Packages ----------------------------------------------------------------
library(tidyverse)
library(magrittr)
library(lubridate)
source("model1.R")

# data --------------------------------------------------------------------

load("piege2018.Rdata")
data2017PS <- read_csv("b1.csv")
data2017ER <- read_csv("r1.csv")
data2017EH <- read_csv("h1.csv")

data2017PS2 <- read_csv("b2.csv")
data2017ER2 <- read_csv("r2.csv")
data2017EH2 <- read_csv("h2.csv")

date2018 <- unique(bloc$Date)
inflos2018 <- data.frame(cbind(ER = bloc %>% filter(Sol == "ER") %>% pull(Inflos),
                               PS = bloc %>% filter(Sol == "PS") %>% pull(Inflos),
                               EH = bloc %>% filter(Sol == "EH") %>% pull(Inflos)))
larves2018 <- data.frame(cbind(ER = bloc %>% filter(Sol == "ER") %>% pull(Larves),
                               PS = bloc %>% filter(Sol == "PS") %>% pull(Larves),
                               EH = bloc %>% filter(Sol == "EH") %>% pull(Larves)))
date2017 <- as_date(data2017ER$date[1]:data2017ER$date[length(data2017EH$date)])
inflos2017 <- data.frame(cbind(ER = data2017ER$inflos_vivantes,
                               PS = data2017PS$inflos_vivantes,
                               EH = data2017EH$inflos_vivantes))
larves2017 <- data.frame(cbind(ER = data2017ER$larves,
                               PS = data2017PS$larves,
                               EH = data2017EH$larves))

inflos2017_b2 <- data.frame(cbind(ER = data2017ER2$inflos_vivantes,
                                  PS = data2017PS2$inflos_vivantes,
                                  EH = data2017EH2$inflos_vivantes))
larves2017_b2 <- data.frame(cbind(ER = data2017ER2$larves,
                                  PS = data2017PS2$larves,
                                  EH = data2017EH2$larves))

load("stades.Rdata")


