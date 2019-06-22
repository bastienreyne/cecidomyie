library(tidyverse)
library(magrittr)
library(lubridate)

data <- read_csv2("/home/bastien/Stage/Moi/Data/2017_flo_B1_.csv") %>%
    mutate(Observées = alive, Théoriques = alive_theo) %>%
    gather(stadeC_theo, stadeE_theo, stadeF_theo, key = "Stade", value = "Nombre")

ggplot(data) +
    aes(x = days, y = Théoriques, fill = Stade) + 
    geom_line(lwd=1) +
    geom_area() +
    theme_bw() +
    xlab("Date") +
    ylab("Nombre d'inflorescences vivantes observées et théoriques") +
    theme(legend.title = element_blank())

moving.mean <- function(x) {
    n_x <- length(x)
    ans <- rep(NA, n_x)
    ans[2:(n_x - 1)] <- x[2:(n_x - 1)] + x[1:(n_x - 2)] + x[3:n_x]
    ans[1] <- 2 * x[1] + x[2]
    ans[n_x] <- 2 * x[n_x] + x[n_x - 1]
    
    ans / 3
}


df_prop <- data.frame(date = data2$days,
                      stadec = data2$stadeC_theo / (data2$stadeC_theo + data2$stadeE_theo + data2$stadeF_theo),
                      stadee = data2$stadeE_theo / (data2$stadeC_theo + data2$stadeE_theo + data2$stadeF_theo),
                      stadef = data2$stadeF_theo / (data2$stadeC_theo + data2$stadeE_theo + data2$stadeF_theo),
                      theo = data2$alive_theo)
    gather(stadec, stadee, stadef, key = stade, value = prop)
df_prop %>% ggplot + 
    aes(x = date) +
    geom_line(aes(y = theo)) +
    geom_ribbon(aes(ymin = 0, ymax = stadec * theo, fill = "C")) +
    geom_ribbon(aes(ymin = stadec * theo, ymax = (stadec + stadee) * theo, fill = "E")) +
    geom_ribbon(aes(ymin = (stadec + stadee) * theo, ymax = (stadec + stadee + stadef) * theo, fill = "F")) +
    theme_bw() +
    ggtitle("Inflorescences vivantes théoriques et stade phénologiques théoriques calculés\ngrâce aux débourrements observés sur le dataset 1") +
    xlab("Date") +
    ylab("Nombre") +
    theme(legend.title = element_blank())
