library(tidyverse)
library(magrittr)
library(readxl)

data <- read_excel("/home/bastien/Stage/Moi/dynamique.cecido.modif0.xlsx")
data %<>% mutate_at(c("nb.piqures", "nb.larves", "cecido", "autres", "etat.panicule"), list(as.numeric))
data %<>% mutate_at("date", list(as_date))
