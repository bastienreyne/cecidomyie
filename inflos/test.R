## Script qui teste la différence théorique des dynamiques d'inflos vivantes et d'inflos attractives

library(tidyverse)
library(magrittr)

burst <- round(rnorm(1000, mean = 50, sd = 8))
death <- burst + 50
attractive <- burst + 43

data <- data.frame(burst, death, attractive)

alive <- rep(NA, max(death))
attra <- rep(NA, max(death))
date <- 1:max(death)
for (day in 1:length(alive)) {
    alive[day] <- data %$% which(day >= burst & day < death) %>% length
    attra[day] <- data %$% which(day >= burst & day < attractive) %>% length
}

## Ce que l'on doit obtenir.
to_plot <- data.frame(date, Vivantes = alive, Attractives = attra) %>% 
    gather(Vivantes, Attractives, key = Inflorescences, value = nombre)
to_plot %>% ggplot(aes(x = date, y = nombre, color = Inflorescences)) +
    geom_line(lwd = 0.75) +
    labs(title = "Comparaison des dynamiques des inflorescences vivantes et attractives") +
    xlab("Date") +
    ylab("Nombre")

## Ce que j'ai fait
contage <- data %>% count(burst)
df <- data.frame(date, deb = 0)
df$deb[which(df$date %in% contage$burst)] <- contage$n
cum_deads <- cumsum(df$deb) - alive

deads <- cum_deads - lag(cum_deads)
deads_c <- rev(lag(rev(deads), 7))

alive_c <- cumsum(df$deb) - cumsum(deads_c)

## Même résultat !!
