## Script calculant la probabilité de pupaison en fonction de la température pour 2017


# packages / data ---------------------------------------------------------

library(tidyverse)
library(magrittr)
library(lubridate)
temp <- read.csv2("../data/meteo_station_St-Paul_2017.csv")
load("../data/date2017.Rdata")

# calcul_p_pup ------------------------------------------------------------

## (Intercept)   1.9544
## Température  -0.0549
## cf. cecidomyie/skype/p_pup.pdf

indices <- which(temp$Date %>% dmy %in% date2017)

good_temp_factor <- temp$Temperature.moyenne..C.[indices]
good_temp <- as.numeric(levels(good_temp_factor))[good_temp_factor]

moving_mean <- function(x) {
    ## Moyenne mobile d'ordre 3
    n_x <- length(x)
    ans <- rep(NA, n_x)
    
    ans[2:(n_x-1)] <- x[1:(n_x - 2)] + x[2:(n_x - 1)] + x[3:n_x]
    ans[1] <- 2 * x[1] + x[2]
    ans[n_x] <- x[n_x-1] + 2 * x[n_x]
    
    ans / 3
}


p_pup <- 1.9544 - 0.0549 * moving_mean(good_temp)
# save(p_pup, file = "p_pup.Rdata")

# ppup temp 15j -----------------------------------------------------------

all_tempf <- temp$Temperature.moyenne..C.
all_temp <- as.numeric(levels(all_tempf))[all_tempf]

temp201715j <- rep(NA, 80)
for (i in 1:80) {
    temp201715j[i] <- mean(all_temp[(indices[i] - 7):(indices[i] + 7)])
}

p_pup15j <- 1.95554 - 0.05497 * temp201715j

data.frame(date = date2017, quinzaine = p_pup15j, constant = 0.77) %>% 
    gather(quinzaine, constant, key = method, value = proba) %>% 
    ggplot +
    aes(x = date, y = proba, color = method) +
    geom_line(lwd = 0.75) +
    ylab("Probabilité d'entrer en pupaison et d'y survivre") +
    xlab("Date") +
    theme_bw() +
    scale_color_discrete(labels = c("0.77 ",
                                         "Température sur la quinzaine ")) +
    theme(legend.title = element_blank(), legend.position = "bottom")
        