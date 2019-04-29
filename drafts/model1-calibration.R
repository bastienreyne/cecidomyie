library(tidyverse)
library(magrittr)
source("model1.R")

my.rmse <- function(x, y){
    n <- length(x)
    ans <- sqrt(sum((x-y)^2) / n)
    return(ans)
}


my.rrmse <- function(x, y){
    ans <- sqrt(sum((x-y)^2)) / mean(y)
    return(ans)
}


my.mae <- function(x, y){
    n <- length(x)
    return( sum(abs(x-y)) / n )
}


my.mse <- function(x, y){
    n <- length(x)
    return( sum((x-y)^2) / n )
}


objectif <- function(x, ma.fonction){
    larves_estimees <- dynamiques(x[1], x[2], x[3], x[4], x[5], inflos)
    # index <- 13:80
    larvesER <- larves_estimees[,ER]
    larvesB <- larves_estimees[,B]
    larvesEH <- larves_estimees[,EH]
    
    return(c(ma.fonction(larvesER, larves.obs[,ER]), ma.fonction(larvesB, larves.obs[,B]), ma.fonction(larvesEH, larves.obs[,EH])))
}



# test.rmse <- nsga2(objectif, 5, 3, my.rmse, lower.bounds = rep(0,5), upper.bounds = c(1000,1,1,1,1000), popsize = 1000, generations = 1000)
# test.rrmse <- nsga2(objectif, 5, 3, my.rrmse, lower.bounds = rep(0,5), upper.bounds = c(1000,1,1,1,1000), popsize = 1000, generations = 1000)
# test.mae <- nsga2(objectif, 5, 3, my.mae, lower.bounds = rep(0,5), upper.bounds = c(1000,1,1,1,1000), popsize = 1000, generations = 1000)
# test.mse <- nsga2(objectif, 5, 3, my.mse, lower.bounds = rep(0,5), upper.bounds = c(1000,1,1,1,1000), popsize = 1000, generations = 1000)
# 
# save(test.rmse, test.rrmse, test.mae, test.mse, file = "test3")
# 

# 
# hmmm <- function(res){
#     require(gridExtra)
#     a <- res$value
#     colnames(a) <- c("ER", "B", "EH")
#     a %<>% as_tibble %>% mutate(norme = sqrt(ER^2 + B^2 + EH^2))
#     b <- res$par[which.min(a$norme), ]
#     toto <- dynamiques(b[1], b[2], b[3], b[4], b[5], inflos)
#     colnames(toto) <- c("ER", "B", "EH")
#     tutu <- cbind(toto, larves.obs) %>% as_tibble
#     plot1 <- tutu %>% ggplot(aes(x=1:80)) + geom_step(aes(y=larves.ER), lwd = 1, col ="green4") + geom_line(aes(y=ER), lwd = 1, col ="red")
#     plot2 <- tutu %>% ggplot(aes(x=1:80)) + geom_step(aes(y=larves.B), lwd = 1, col ="green4") + geom_line(aes(y=B), lwd = 1, col ="red")
#     plot3 <- tutu %>% ggplot(aes(x=1:80)) + geom_step(aes(y=larves.EH), lwd = 1, col ="green4") + geom_line(aes(y=EH), lwd = 1, col ="red")
#     grid.arrange(plot1, plot2, plot3, nrow=3)
# }

# objectifER <- function(x, ma.fonction){
#     larves_estimees <- dynamiques(x[1], x[2], x[3], x[4], x[5], inflos)
#     # index <- 13:80
#     larvesER <- larves_estimees[,ER]
#     
#     return(ma.fonction(larvesER, larves.obs[,ER]))
# }
# 
# test.er <- nsga2(objectifER, 5, 1, my.mae, lower.bounds = rep(0,5), upper.bounds = c(1000,1,1,1,1000), popsize = 1000, generations = 200)

# round2 <- nsga2(objectif, 3, 1, my.mae, lower.bounds = rep(0, 3), upper.bounds = c(0.1, 0.5, 0.2),
#                 popsize = 1000, generations = 200)
# 
# save(round2, file = "nsga22.Rdata")