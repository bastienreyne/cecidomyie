# Packages / data ---------------------------------------------------------

library(tidyverse)
library(magrittr)
library(lubridate)

inflos_piege_ER <- read_csv2("Data/2017_B1_enh.ras.csv") %>% select(date, inflos_vivantes)
inflos_piege_PS <- read_csv2("Data/2017_B1_bache.csv") %>% select(date, inflos_vivantes)
inflos_piege_EH <- read_csv2("Data/2017_B1_enh.haut.csv") %>% select(date, inflos_vivantes)

