## Script qui teste


# Packages / data ---------------------------------------------------------

library(tidyverse)
library(magrittr)
library(lubridate)
source("model1.R")
source("model.R")
data <- read_csv("../data/2017_piege.csv")
inflos <- cbind(data %>% filter(Sol == "ER") %>% pull(inflos),
                data %>% filter(Sol == "PS") %>% pull(inflos),
                data %>% filter(Sol == "EH") %>% pull(inflos))
larves <- cbind(data %>% filter(Sol == "ER") %>% pull(larves),
                data %>% filter(Sol == "PS") %>% pull(larves),
                data %>% filter(Sol == "EH") %>% pull(larves))

# Optimisation ------------------------------------------------------------

dynamics(c(0.2, 0.5, 1, 0, 0.2, 1, 0, 0, 0, 0, 0), inflos) == dynamiques(0.2, 0.5, 1, 0, 0.2, inflos)
dynamics(c(0.2, 0.5, 1, 0, 0.8, 1, 0, 0, 0, 0, 0), inflos) == dynamiques(0.2, 0.5, 1, 0, 0.8, inflos)
dynamics(c(0.01, 0.9, 1, 0.5, 0.2, 1, 0, 0, 0, 0, 0), inflos) == dynamiques(0.01, 0.9, 1, 0.5, 0.2, inflos)
dynamics(c(0.2, 0.5, 1, 0, 0.2, 1, 0, 0, 0, 0, 0), inflos) == dynamiques(0.2, 0.5, 1, 0, 0.2, inflos)
