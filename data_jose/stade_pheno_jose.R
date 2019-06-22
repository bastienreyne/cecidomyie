

library(tidyverse)
data_jose <- read.csv2("Essai_José_BP.csv") 

data_jose$B[data_jose$B %>% is.na] <- 0
data_jose$C[data_jose$C %>% is.na] <- 0
data_jose$D[data_jose$D %>% is.na] <- 0
data_jose$E[data_jose$E %>% is.na] <- 0
data_jose$F[data_jose$F %>% is.na] <- 0
data_jose$G[data_jose$G %>% is.na] <- 0

fit <- lm(nb_larves ~ B + C + D + E + F + G - 1,
          data = data_jose)
