load("def.Rdata")
source("model1.R")


# Pareto ------------------------------------------------------------------

f.res <- def$value %>% as.data.frame()
colnames(f.res) <- c("ER", "B", "EH")


library(plotly)
plot_ly(x=~f.res$ER[1:2000], y=~f.res$B[1:2000], z=~f.res$EH[1:2000], size = I(1))


# Trajectoires ------------------------------------------------------------

f.res %<>% mutate(norm1 = abs(ER) + abs(B) + abs(EH), norm2 = sqrt(ER^2 + B^2 + EH^2))

min1 <- which.min(f.res$norm1)
min2 <- which.min(f.res$norm2)

xopt1 <- def$par[min1,]
xopt2 <- def$par[min2,]

larves.est1 <- dynamiques(xopt1[1], xopt1[2], xopt1[3], xopt1[4], xopt1[5], inflos)
larves.est2 <- dynamiques(xopt2[1], xopt2[2], xopt2[3], xopt2[4], xopt2[5], inflos)

colnames(larves.est1) <- c("ER", "B", "EH")
colnames(larves.est2) <- c("ER", "B", "EH")

larves1 <- cbind(larves.est1, larves.obs) %>% as_tibble
larves2 <- cbind(larves.est2, larves.obs) %>% as_tibble

plot1 <- larves1 %>% ggplot(aes(x=1:80)) +
    geom_step(aes(y=larves.ER), lwd = 1, col ="green4") +
    geom_line(aes(y=ER), lwd = 1, col ="red")
plot2 <- larves1 %>% ggplot(aes(x=1:80)) +
    geom_step(aes(y=larves.B), lwd = 1, col ="green4") +
    geom_line(aes(y=B), lwd = 1, col ="red")
plot3 <- larves1 %>% ggplot(aes(x=1:80)) +
    geom_step(aes(y=larves.EH), lwd = 1, col ="green4") +
    geom_line(aes(y=EH), lwd = 1, col ="red")
library(gridExtra)
grid.arrange(plot1, plot2, plot3, nrow=3)

plot4 <- larves2 %>% ggplot(aes(x=1:80)) +
    geom_step(aes(y=larves.ER), lwd = 1, col ="green4") +
    geom_line(aes(y=ER), lwd = 1, col ="red")
plot5 <- larves2 %>% ggplot(aes(x=1:80)) +
    geom_step(aes(y=larves.B), lwd = 1, col ="green4") +
    geom_line(aes(y=B), lwd = 1, col ="red")
plot6 <- larves2 %>% ggplot(aes(x=1:80)) +
    geom_step(aes(y=larves.EH), lwd = 1, col ="green4") +
    geom_line(aes(y=EH), lwd = 1, col ="red")
grid.arrange(plot4, plot5, plot6, nrow=3)


# Correlation matrice -----------------------------------------------------

library(corrplot)
load("test3.Rdata")
def$par %>% cor %>% corrplot.mixed
test.rmse$par %>% cor %>% corrplot.mixed()

# Parameters density ------------------------------------------------------

def$par %>% as_tibble %>% 
    gather(V1, V2, V3, V4, V5, key= Parameter, value = Valeur) %>%
    ggplot(aes(x=Valeur)) + geom_density() + 
    facet_grid(Parameter~., scales = "free")


# Sobol indices -----------------------------------------------------------

load("soboled4.Rdata")

df <- rbind(SG, SPM, SMUER, SMUEH, SK, SGt, SPMt, SMUERt, SMUEHt, SKt) %>% as_tibble
colnames(df) <- c("V1", "V2", "V3")
df %<>% mutate(Parameter = rep(c("gamma", "pm", "muER", "muEH", "k"), 2)) %>%
    gather(V1, V2, V3, key=Parcelle, value = Valeur) %>%
    mutate(Effect = rep(c("Main effect", "Total effect"), each=5, times=3))
plot <- df %>% ggplot(aes(x=Parameter, y=Valeur, fill = Effect)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_grid(.~Parcelle)


# Pic d'inflos ------------------------------------------------------------

inflos2 <- inflos
inflos2[38, ] <- rep(10000, 3)
larves.est3 <- dynamiques(xopt1[1], xopt1[2], xopt1[3], xopt1[4], xopt1[5], inflos2)
colnames(larves.est3) <- c("ER", "B", "EH")
larves3 <- cbind(larves.est3, larves.obs) %>% as_tibble
plot7 <- larves3 %>% ggplot(aes(x=1:80)) + geom_line(aes(y=ER), lwd = 1, col ="red")
plot8 <- larves3 %>% ggplot(aes(x=1:80)) + geom_line(aes(y=B), lwd = 1, col ="red")
plot9 <- larves3 %>% ggplot(aes(x=1:80)) + geom_line(aes(y=EH), lwd = 1, col ="red")
grid.arrange(plot7, plot8, plot9, nrow=3)


# Round 2 -----------------------------------------------------------------

load("dyn.Rdata")
colnames(dyn) <- c("ERest", "Best", "EHest")
trajs <- cbind(dyn, larves.obs) %>% as.data.frame()
ER <- tibble(Estimed = trajs[, 1], Observed = trajs[, 4]) %>% mutate(date = 1:80)
BA <- tibble(Estimed = trajs[, 2], Observed = trajs[, 5]) %>% mutate(date = 1:80)
EH <- tibble(Estimed = trajs[, 3], Observed = trajs[, 6]) %>% mutate(date = 1:80)

plot10 <- ER %>% gather(Estimed, Observed, key = Type, value = Nombre) %>% 
    ggplot(aes(x=date, color = Type, y = Nombre)) + geom_line() + geom_point()
plot11 <- BA %>% gather(Estimed, Observed, key = Type, value = Nombre) %>% 
    ggplot(aes(x=date, color = Type, y = Nombre)) + geom_line() + geom_point()
plot12 <- EH %>% gather(Estimed, Observed, key = Type, value = Nombre) %>% 
    ggplot(aes(x=date, color = Type, y = Nombre)) + geom_line() + geom_point()
grid.arrange(plot10, plot11, plot12, nrow=3)


# Corrplot DE -------------------------------------------------------------
library(corrplot)
matDE <- matrix(c(1.000000, -0.658776, -0.525787,  0.109526, -0.531451,
-0.658776,  1.000000, -0.017371, -0.169714,  0.714145,
-0.525787, -0.017371,  1.000000, -0.235969, -0.155854,
0.109526, -0.169714, -0.235969,  1.000000,  0.043995,
-0.531451,  0.714145, -0.155854,  0.043995,  1.000000), nrow = 5)
corDE <- matDE %>% corrplot.mixed()


# corrplot DE2 ------------------------------------------------------------

matDE2 <- matrix(c(1.000000,  0.182970, -0.346373,  0.141448, -0.285088,
0.182970,  1.000000, -0.291980, -0.275502,  0.122284,
-0.346373, -0.291980,  1.000000,  0.008693, -0.025013,
0.141448, -0.275502,  0.008693,  1.000000, -0.427027,
-0.285088,  0.122284, -0.025013, -0.427027,  1.000000), nrow = 5) %>% corrplot.mixed()


# params DE pas mauvais ---------------------------------------------------

xoptDE <- c(0.02507885, 0.45716143, 1. , 0. , 0.10998349) # MAE 1057
xoptNSGA <- c(2.667557e-02, 3.497850e-01, 9.999998e-01, 1.573494e-05, 1.096670e-01) # MAE 1059, norme 1
xopt2 <- c(0.0424936240, 0.2368537254, 0.9999620007, 0.0001214987, 0.0868952803) # MAE 1067, norme 2
xopt3 <- c(1.430920e-02, 4.138999e-01, 1.000000e+00, 3.776191e-16, 5.295801e-02) # MAE 1165, norme max
xopt <- c(0.02510529, 0.45756503, 1. , 0. , 0.10999303) 


# Matrices de correlations ------------------------------------------------
load("matcors.Rdata")


corDE <- matDE %>% corrplot.mixed()
cor1 <- norme1 %>% cor(method = "spearman") %>% corrplot.mixed()
cor2 <- norme2 %>% cor(method = "spearman") %>% corrplot.mixed()
cor3 <- normemax %>% cor(method = "spearman") %>% corrplot.mixed()


norme1 %<>% as.data.frame()
norme2 %<>% as.data.frame()
normemax %<>% as.data.frame()
ggpairs(norme1)
ggpairs(norme2)
ggpairs(normemax)

# plot various optimized trajs --------------------------------------------

params <- read.csv("params.csv") %>% as_tibble() %>% select(-1)
trajectories <- matrix(NA, nrow = 80, ncol = 3 * 50)
for (iter in 1:50) {
    trajectories[, c(iter, iter + 50, iter + 2 * 50)] <- dynamiques(params[iter, 1],
                                                                            params[iter, 2],
                                                                            params[iter, 3],
                                                                            params[iter, 4],
                                                                            params[iter, 5],
                                                                            inflos)
}

trajectories %<>% as.data.frame()
A <- trajectories[, 1:50]
B <- trajectories[, 51:100]
C <- trajectories[, 101:150]

nice_plot <- larves.ER %>% enframe %>%
    ggplot(aes(x = name, y = value)) + geom_point() + geom_line()
for (i in 1:50) {
    nice_plot <- nice_plot + geom_point(data = A[, i] %>% enframe,
                                        aes(x = name, y = value),
                                        col = "red") + geom_line(data = A[, i] %>% enframe,
                                                                 aes(x = name, y = value),
                                                                 col = "red")
}

nice_plot2 <- larves.B %>% enframe %>%
    ggplot(aes(x = name, y = value)) + geom_point() + geom_line()
for (i in 1:50) {
    nice_plot2 <- nice_plot2 + geom_point(data = B[, i] %>% enframe,
                                        aes(x = name, y = value),
                                        col = "red") + geom_line(data = B[, i] %>% enframe,
                                                                 aes(x = name, y = value),
                                                                 col = "red")
}

nice_plot3 <- larves.EH %>% enframe %>%
    ggplot(aes(x = name, y = value)) + geom_point() + geom_line()
for (i in 1:50) {
    nice_plot3 <- nice_plot3 + geom_point(data = C[, i] %>% enframe,
                                          aes(x = name, y = value),
                                          col = "red") + geom_line(data = C[, i] %>% enframe,
                                                                   aes(x = name, y = value),
                                                                   col = "red")
}



# second try --------------------------------------------------------------


params <- read.csv("parametres.csv") %>% as_tibble() %>% select(-1)
trajectories <- matrix(NA, nrow = 80, ncol = 3 * 10)
for (iter in 1:10) {
    trajectories[, c(iter, iter + 10, iter + 2 * 10)] <- dynamiques(params[iter, 1],
                                                                    params[iter, 2],
                                                                    params[iter, 3],
                                                                    params[iter, 4],
                                                                    params[iter, 5],
                                                                    inflos)
}

trajectories %<>% as.data.frame()
A <- trajectories[, 1:10]
B <- trajectories[, 11:20]
C <- trajectories[, 21:30]

nice_plot <- larves.ER %>% enframe %>%
    ggplot(aes(x = name, y = value)) + geom_point() + geom_line()
for (i in 1:10) {
    nice_plot <- nice_plot + geom_point(data = A[, i] %>% enframe,
                                        aes(x = name, y = value),
                                        col = "red") + geom_line(data = A[, i] %>% enframe,
                                                                 aes(x = name, y = value),
                                                                 col = "red")
}

nice_plot2 <- larves.B %>% enframe %>%
    ggplot(aes(x = name, y = value)) + geom_point() + geom_line()
for (i in 1:10) {
    nice_plot2 <- nice_plot2 + geom_point(data = B[, i] %>% enframe,
                                          aes(x = name, y = value),
                                          col = "red") + geom_line(data = B[, i] %>% enframe,
                                                                   aes(x = name, y = value),
                                                                   col = "red")
}

nice_plot3 <- larves.EH %>% enframe %>%
    ggplot(aes(x = name, y = value)) + geom_point() + geom_line()
for (i in 1:10) {
    nice_plot3 <- nice_plot3 + geom_point(data = C[, i] %>% enframe,
                                          aes(x = name, y = value),
                                          col = "red") + geom_line(data = C[, i] %>% enframe,
                                                                   aes(x = name, y = value),
                                                                   col = "red")
}



# Plot 3D -----------------------------------------------------------------

library(plotly)
plot_ly(x = ~ test.mae$value[, 1], 
        y = ~ test.mae$value[, 2],
        z = ~ test.mae$value[, 3],
        size = I(1))
