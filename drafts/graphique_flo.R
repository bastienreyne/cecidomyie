library(tidyverse)
library(magrittr)
library(lubridate)

data <- read_csv2("/home/bastien/Stage/Moi/Data/2017_flo_B1_.csv") %>% mutate(Observées = moving.mean(alive), Théoriques = moving.mean(alive_theo))
data2 <- data %>% gather(`Observées`, `Théoriques`, key = "Stade", value = "Nombre")

ggplot(data2, aes(x=days ,y= Nombre, color = Stade))+geom_line(lwd=1) + theme_bw() + xlab("Date") + ylab("Nombre d'inflorescences vivantes observées et théoriques") + theme(legend.title = element_blank())
