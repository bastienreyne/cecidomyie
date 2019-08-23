## Script qui corrige le dataset 1 en utilisant le dataset 2

# Data / packages ---------------------------------------------------------

library(tidyverse)
library(magrittr)

ds1 <- read_csv("2017_floraison_not_corrected.csv")
ds2 <- read_csv("2017_piege.csv") %>% select(date, inflos, Sol)

