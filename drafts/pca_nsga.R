## ACP sur les résultats de NSGA-II

# Packages / data ---------------------------------------------------------

library(tidyverse)
library(FactoMineR)
library(magrittr)
library(ggrepel)
load(file = "to_PCA.Rdata")

# Mise en forme data ------------------------------------------------------

df_pca <- cbind(scale(new_front), params)
colnames(df_pca) <- c("ER", "EH", "Critère", "gamma", "pm", "muER", "muEH", "k")
pca_nsga <- PCA(df_pca, quanti.sup = 4:8)

# Graphs ------------------------------------------------------------------

ind_muER <- 1:317 %in% which(df_pca[, 6] > median(df_pca[, 6])) %>% as.numeric()
ind_muEH <- 1:317 %in% which(df_pca[, 7] > median(df_pca[, 7])) %>% as.numeric()
ind_gamma <- 1:317 %in% which(df_pca[, 4] < median(df_pca[, 4])) %>% as.numeric()
ind_pm <- 1:317 %in% which(df_pca[, 5] < median(df_pca[, 5])) %>% as.numeric()

pca_nsga$ind$coord %>%
    as.data.frame %>%
    mutate(yep = factor(ind_muER * ind_muEH * ind_gamma * ind_pm)) %>% 
    ggplot() +
    aes(x = Dim.1, y = Dim.2) +
    geom_hline(aes(yintercept = 0)) +
    geom_vline(aes(xintercept = 0)) +
    geom_point(aes(color = yep)) +
    # geom_point() +
    # stat_ellipse(aes(x = Dim.1, y = Dim.2), data = 
    #                  as.data.frame(pca_nsga$ind$coord[which(df_pca[, 6] > median(df_pca[, 6])), ])) +
    # stat_ellipse(aes(x = Dim.1, y = Dim.2), data = 
    #                  as.data.frame(pca_nsga$ind$coord[which(df_pca[, 7] > median(df_pca[, 7])), ])) +
    # stat_ellipse(aes(x = Dim.1, y = Dim.2), data = 
    #                  as.data.frame(pca_nsga$ind$coord[which(df_pca[, 4] < median(df_pca[, 4])), ])) +
    # stat_ellipse(aes(x = Dim.1, y = Dim.2), data = 
    #                  as.data.frame(pca_nsga$ind$coord[which(df_pca[, 5] > median(df_pca[, 5])), ])) +
    theme_bw() +
    theme(legend.title = element_blank()) +
    xlab("Composante 1 (73.02 %)") +
    ylab("Composante 2 (17.87 %)") +
    scale_color_discrete(labels = c("Non", "Oui"))

df2 <- data.frame(pca_nsga$var$coord) %>% mutate(statut = factor("in", levels = c("in", "out")))
df3 <- data.frame(pca_nsga$quanti.sup$coord) %>% mutate(statut = factor("out", levels = c("in", "out")))
df4 <- bind_rows(df2, df3)
df4 %>% ggplot +
    aes(x = Dim.1, y= Dim.2, color = statut) +
    geom_segment(aes(x = 0, y = 0, xend = Dim.1, yend = Dim.2),
                 arrow = arrow(length = unit(0.4, "cm"))) +
    annotate("path", x=0+1*cos(seq(0,2*pi,length.out=100)), y=0+1*sin(seq(0,2*pi,length.out=100))) +
    geom_abline(aes(intercept=0, slope=0)) +
    geom_vline(aes(xintercept = 0)) +
    geom_label_repel(aes(label = c("ER", "EH", "Critère", "gamma", "pm", "muER", "muEH", "k")),
                     position = "dodge") +
    theme_bw() +
    xlab("Composante 1 (73.02 %)") + 
    ylab("Composante 2 (17.87 %)") +
    theme_bw() +
    theme(aspect.ratio = 1, legend.title = element_blank()) +
    scale_color_discrete(labels = c("Principale", "Supplémentaire"))


# Paramètres intéressant --------------------------------------------------

good_sol <- params[which(ind_muER * ind_muEH * ind_gamma * ind_pm == 1), ]
