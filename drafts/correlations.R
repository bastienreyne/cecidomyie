library(tidyverse)
library(magrittr)
source("model1-calibration.R")

nb <- 10000


gammas <- seq(0, 0.1, length.out = nb)
pms <- seq(0, 0.5, length.out = nb)
muers <- seq(0.75, 1, length.out = nb)
muehs <- seq(0, 0.1, length.out = nb)
ks <- seq(0, 0.2, length.out = nb)