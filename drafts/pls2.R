## Script PLS2 des résultats de NSGA-II

# packages / data ---------------------------------------------------------

library(tidyverse)
library(magrittr)
library(emoa)
library(pls)
library(FactoMineR)
source("../model_R/model.R")
source("../model_R/objectif.R")
load("to_PLS2.Rdata")

# pareto front ------------------------------------------------------------

values <- rbind(res1$value, res2$value, res3$value, res4$value, res5$value,
                res6$value, res7$value, res8$value, res9$value, res10$value,
                res11$value, res12$value, res13$value, res14$value, res15$value)
true_values <- values[!is_dominated(t(values)), ]

params <- rbind(res1$par, res2$par, res3$par, res4$par, res5$par,
                res6$par, res7$par, res8$par, res9$par, res10$par,
                res11$par, res12$par, res13$par, res14$par, res15$par)

true_param <- params[!is_dominated(t(values)), ]


# calcul nrmse ps ---------------------------------------------------------

n_pareto <- nrow(true_param)
critere_ps <- rep(NA, n_pareto)
for (i in 1:n_pareto) {
    critere_ps[i] <- objPS(true_param[i, ])
}

all_critere <- cbind(true_values[, 1], critere_ps,
                     true_values[, 2], -true_values[, 3])
colnames(all_critere) <- c("ER", "PS", "EH", "max_emer")

# PLS2 --------------------------------------------------------------------

Y <- scale(all_critere)
X <- scale(true_param)
colnames(X) <- c("gamma", "pm", "muER", "muEH", "k")

reg_pls2 <- plsr(Y ~ X, ncomp = 5, scale = TRUE, validation = "LOO")

data.frame(Measured = Y[, 1], Predicted = reg_pls2$fitted.values[, 1, 5]) %>% 
    ggplot +
    aes(x = Measured, y = Predicted) +
    geom_point() +
    theme_bw() +
    coord_equal() +
    xlim(c(-2, 3.7)) +
    ylim(c(-2, 3.7)) +
    geom_abline(aes(intercept = 0, slope = 1))


# PCA ---------------------------------------------------------------------

topca <- cbind(Y, X)
pca <- PCA(topca, quanti.sup = 5:9)

pca$ind$coord %>%
    as.data.frame %>%
    ggplot() +
    aes(x = Dim.1, y = Dim.3) +
    geom_hline(aes(yintercept = 0)) +
    geom_vline(aes(xintercept = 0)) +
    geom_point() +
    theme_bw() +
    theme(legend.title = element_blank()) +
    xlab("Composante 1 (62.07 %)") +
    ylab("Composante 3 (9.05 %)")

df2 <- data.frame(pca$var$coord) %>% mutate(statut = factor("in", levels = c("in", "out")))
df3 <- data.frame(pca$quanti.sup$coord) %>% mutate(statut = factor("out", levels = c("in", "out")))
df4 <- bind_rows(df2, df3)
df4 %>% ggplot +
    aes(x = Dim.1, y = Dim.2, color = statut) +
    geom_segment(aes(x = 0, y = 0, xend = Dim.1, yend = Dim.2),
                 arrow = arrow(length = unit(0.4, "cm"))) +
    annotate("path", x=0+1*cos(seq(0,2*pi,length.out=100)), y=0+1*sin(seq(0,2*pi,length.out=100))) +
    geom_hline(aes(yintercept = 0)) +
    geom_vline(aes(xintercept = 0)) +
    geom_label(aes(y = Dim.2 - 0.06,
                   label = c("ER", "PS", "EH", "max_endo",
                             "gamma", "p[m]", "mu[ER]", "mu[EH]", "k")),
               parse = TRUE) +
    theme_bw() +
    xlab("Composante 1 (62.07 %)") + 
    ylab("Composante 2 (27.71 %)") +
    theme_bw() +
    theme(aspect.ratio = 1, legend.title = element_blank()) +
    scale_color_discrete(labels = c("Active", "Supplémentaire")) 


df4 %>% ggplot +
    aes(x = Dim.1, y = Dim.3, color = statut) +
    geom_segment(aes(x = 0, y = 0, xend = Dim.1, yend = Dim.3),
                 arrow = arrow(length = unit(0.4, "cm"))) +
    annotate("path", x=0+1*cos(seq(0,2*pi,length.out=100)), y=0+1*sin(seq(0,2*pi,length.out=100))) +
    geom_hline(aes(yintercept = 0)) +
    geom_vline(aes(xintercept = 0)) +
    geom_label(aes(y = Dim.3 + 0.06,
                   label = c("ER", "PS", "EH", "max_endo",
                             "gamma", "p[m]", "mu[ER]", "mu[EH]", "k")),
               parse = TRUE) +
    # geom_label(aes(label = c("ER", "PS", "EH", "max_emer",
    #                          "gamma", "pm", "muER", "muEH", "k")),
    #            check_overlap = TRUE) +
    theme_bw() +
    xlab("Composante 1 (62.07 %)") + 
    ylab("Composante 3 (9.05 %)") +
    theme_bw() +
    theme(aspect.ratio = 1, legend.title = element_blank()) +
    scale_color_discrete(labels = c("Active", "Supplémentaire")) 


# exemples solutions ------------------------------------------------------

## max endo
arg1 <- true_param[which(true_param[, 1] < quantile(true_param[, 1], 0.01)), ]
arg1 <- arg1[1, ]

## min PS et EH 
arg2 <- true_param[which(true_param[, 4] > quantile(true_param[, 4], 0.915) &
                             true_param[, 3] < quantile(true_param[, 3], 0.1) ), ]

## min ER
arg3 <- true_param[which(true_param[, 4] < quantile(true_param[, 4], 0.00187)), ]
arg3 <- arg3[1, ]

arg4 <- c( 0.04658174, 0.78545289, 0.49709368, 0.87663688, 3.71755689)

arg5 <- true_param[912, ]

# plot --------------------------------------------------------------------

estimation <- dynamics2(arg5, inflos_simulated)
proportion <- decomposition2(arg5, inflos_simulated)

ploter <- data.frame(Date = date2017,
                     Sol = factor("Enherbement ras", levels = c("Enherbement ras",
                                                                "Paillage synthétique",
                                                                "Enherbement haut")),
                     method = factor("Modification", levels = c("Référence", "Modification")),
                     Observation = larves1,
                     Estimation = estimation[, 1],
                     Endogène = proportion[[3]][, 1],
                     Side = proportion[[4]][, 1],
                     Exogène = proportion[[2]][, 1]) %>% 
    gather(Exogène, Side, Endogène, key = prov, value = prop, factor_key = TRUE)

plotps <- data.frame(Date = date2017,
                     Sol = factor("Paillage synthétique", levels = c("Enherbement ras",
                                                                     "Paillage synthétique",
                                                                     "Enherbement haut")),
                     method = factor("Modification", levels = c("Référence", "Modification")),
                     Observation = larves2,
                     Estimation = estimation[, 2],
                     Endogène = proportion[[3]][, 2],
                     Side = proportion[[4]][, 2],
                     Exogène = proportion[[2]][, 2]) %>% 
    gather(Exogène, Side, Endogène, key = prov, value = prop, factor_key = TRUE)

ploteh <- data.frame(Date = date2017,
                     Sol = factor("Enherbement haut", levels = c("Enherbement ras",
                                                                 "Paillage synthétique",
                                                                 "Enherbement haut")),
                     method = factor("Modification", levels = c("Référence", "Modification")),
                     Observation = larves3,
                     Estimation = estimation[, 3],
                     Endogène = proportion[[3]][, 3],
                     Side = proportion[[4]][, 3],
                     Exogène = proportion[[2]][, 3]) %>% 
    gather(Exogène, Side, Endogène, key = prov, value = prop, factor_key = TRUE)


to_plot <- bind_rows(ploter, plotps, ploteh)
to_plot %>% ggplot +
    aes(x = Date) +
    geom_area(aes(y = prop, fill = prov), alpha = 0.5) +
    geom_line(aes(y = Observation, color = "Observation"), lwd = 0.75) +
    geom_line(aes(y = Estimation, color = "Estimation"), lwd = 0.75) +
    geom_point(aes(y = Observation, color = "Observation")) +
    geom_point(aes(y = Estimation, color = "Estimation")) +
    theme_bw() +
    facet_grid(. ~ Sol) +
    scale_color_manual(values = c("green4", "black")) +
    theme(legend.title = element_blank(), legend.position = "bottom") +
    ylab("Nombre de larves s'éjectant")



