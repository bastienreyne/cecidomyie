## Datas
inflos.ER <- read.csv("r1.csv")$inflos_vivantes
inflos.B <- read.csv("b1.csv")$inflos_vivantes
inflos.EH <- read.csv("h1.csv")$inflos_vivantes
larves.ER <- read.csv("r1.csv")$larves
larves.B <- read.csv("b1.csv")$larves
larves.EH <- read.csv("h1.csv")$larves
inflos <- cbind(inflos.ER, inflos.B, inflos.EH)
larves.obs <- cbind(larves.ER, larves.B, larves.EH)

my.mae <- function(x, y){
    n <- length(x)
    return( sum(abs(x-y)) / n )
}

fun.obj <- function(beta, cost){
    est <- beta*inflos
    ER <- cost(est[,1], larves.ER)
    B <- cost(est[,2], larves.B)
    EH <- cost(est[,3], larves.EH)
    return(B)
}

library(mco)
super.res <- nsga2(fun.obj, 1, 1, my.rmse, lower.bounds = 0, upper.bounds = 1, popsize = 80, generations = 50)
lol <- super.res$par[1] * inflos
super.df <- cbind(est = lol[,2], obs = larves.B)
super.df %>% as_tibble %>% ggplot(aes(x=1:80, y=obs)) + geom_step(col ="green4") + geom_line(aes(y=est), col="red")

