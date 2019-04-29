library(tidyverse)
library(magrittr)
library(lubridate)

moving.mean <- function(x){
    L <- length(x)
    ans <- (x + c(x[2:L], NA) + c(NA, x[1:(L-1)])) / 3
    ans[1] <- (2 * x[1] + x[2]) / 3
    ans[L] <- (x[L-1] + x[L] * 2) / 3
    
    return(ans)
}


tr1 <- read_csv2("temp/ER_Bloc1.csv") %>% mutate_at("Date", dmy) %>% mutate(Bloc = "Bloc 1", Sol = "ER")
tb1 <- read_csv2("temp/B_Bloc1.csv") %>% mutate_at("Date", dmy) %>% mutate(Bloc = "Bloc 1", Sol = "B")
th1 <- read_csv2("temp/EH_Bloc1.csv") %>% mutate_at("Date", dmy) %>% mutate(Bloc = "Bloc 1", Sol = "EH")
tr2 <- read_csv2("temp/ER_Bloc2.csv") %>% mutate_at("Date", dmy) %>% mutate(Bloc = "Bloc 2", Sol = "ER")
tb2 <- read_csv2("temp/B_Bloc2.csv") %>% mutate_at("Date", dmy) %>% mutate(Bloc = "Bloc 2", Sol = "B")
th2 <- read_csv2("temp/EH_Bloc2.csv") %>% mutate_at("Date", dmy) %>% mutate(Bloc = "Bloc 2", Sol = "EH")

temps <- bind_rows(tr1, tb1, th1, tr2, tb2, th2)

ggplot(temps, aes(x=Date, y = moving.mean(Tmoy))) + geom_line(col ="green4") + geom_ribbon(aes( ymin = moving.mean(Tmin), ymax= moving.mean(Tmax)), alpha = 0.3, fill ="green4") + geom_hline(yintercept = c(20,26), col ="red") + facet_grid(Sol~Bloc)
ggplot(temps, aes(x=Date, y = moving.mean(Tmoy), color= Sol, fill = Sol)) + geom_line()+ facet_grid(.~Bloc) + scale_color_viridis_d() +theme_bw() #+ geom_ribbon(aes( ymin = moving.mean(Tmin), ymax= moving.mean(Tmax)), alpha = 0.3, ) + facet_grid(.~Bloc)
