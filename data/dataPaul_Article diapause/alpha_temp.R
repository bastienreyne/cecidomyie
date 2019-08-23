## Script pour ajuster alpha ~ temp

# packages / data ---------------------------------------------------------

library(tidyverse)
data1 <- c(8.3333333, 12.0000000, 5.3333333, 0.6666667, 0.3333333, 1.3333333, 0.3333333,
           1.3333333, 1.6666667, 1.0000000, 0.3333333, 1.3333333, 0.3333333, 0.3333333)
temp1 <- c(21.45000, 21.10000, 20.77143, 20.92857, 21.48571, 20.31429, 21.18571,
           21.12857, 21.45714, 21.84286, 21.41429, 22.20000, 21.81429, 22.54286)

data2 <- c(6.3333333, 10.3333333, 0.6666667, 2.3333333, 1.3333333, 2.3333333, 2.3333333,
           1.0000000, 2.0000000, 0.3333333, 0.6666667, 0.3333333)
temp2 <- c(20.31667, 20.84286, 22.05714, 21.22857, 21.25714, 21.45714, 21.44286, 20.80000,
           20.86667, 21.24286, 21.35000, 21.57143)

## Seuil 20.5°C
sortie <- density(c(data1, data2))
fdr <- ecdf(c(data1, data2))

prop_sortie <- rep(NA, 12)
prop_sortie[1] <- fdr(1) - fdr(-15)
for (i in 2:12) {
    prop_sortie[i] <- fdr(i) - fdr(i-1) 
}


toto <- data.frame(jour = 1:84, prob = rep(prop_sortie/7, each = 7))
toto %>% ggplot +
    aes(x = jour, y = prob) +
    geom_point() +
    theme_bw() +
    ylab("Proportion du stock de diapausantes qui sortent") +
    xlab("Jour")

prop_sortie <- rep(prop_sortie/7, each = 7)
save(prop_sortie, file = "sortie_diapause2017.Rdata")
    