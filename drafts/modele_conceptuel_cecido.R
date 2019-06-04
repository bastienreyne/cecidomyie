## Script relatif au modèle conceptuel de cécidomyies
## L_t = alpha_t * R * (mu * L_t-12 + E_t-7)

# Packages / data ---------------------------------------------------------

library(tidyverse)
library(magrittr)
library(mco)
library(gridExtra)
data <- read.csv("../data/2017_piege.csv")
larves_ER <- data %>% filter(Sol == "ER") %>% pull(larves)
larves_PS <- data %>% filter(Sol == "PS") %>% pull(larves)
larves_EH <- data %>% filter(Sol == "EH") %>% pull(larves)
inflos_ER <- data %>% filter(Sol == "ER") %>% pull(inflos)
inflos_PS <- data %>% filter(Sol == "PS") %>% pull(inflos)
inflos_EH <- data %>% filter(Sol == "EH") %>% pull(inflos)
load("../data/date2017.Rdata")

# Régression --------------------------------------------------------------

### Mettre en forme les données

## ER
mat_ER <- matrix(NA, nrow = 63, ncol = 12)
mat_ER[1:63] <- larves_ER[18:80]
for (colonne in 1:11) {
    mat_ER[, colonne + 1] <- larves_ER[(12 - colonne):(74 - colonne)]
}
colnames(mat_ER) <- c("Lt", paste0("Lt-", 7:17))

## PS
mat_PS <- matrix(NA, nrow = 63, ncol = 12)
mat_PS[1:63] <- larves_PS[18:80]
for (colonne in 1:11) {
    mat_PS[, colonne + 1] <- larves_PS[(12 - colonne):(74 - colonne)]
}
colnames(mat_PS) <- c("Lt", paste0("Lt-", 7:17))

## EH
mat_EH <- matrix(NA, nrow = 63, ncol = 12)
mat_EH[1:63] <- larves_EH[18:80]
for (colonne in 1:11) {
    mat_EH[, colonne + 1] <- larves_EH[(12 - colonne):(74 - colonne)]
}
colnames(mat_EH) <- c("Lt", paste0("Lt-", 7:17))

mat_ER %<>% as.data.frame
mat_PS %<>% as.data.frame
mat_EH %<>% as.data.frame

### Régression linéaire simple
lm_ER <- lm(Lt ~ ., data = mat_ER)
lm_PS <- lm(Lt ~ ., data = mat_PS)
lm_EH <- lm(Lt ~ ., data = mat_EH)

### GLM poisson
mat_ER2 <- mat_ER %>% round
mat_PS2 <- mat_PS %>% round
mat_EH2 <- mat_EH %>% round

glm_ER <- glm(Lt ~ ., data = mat_ER2, family = "poisson")
glm_PS <- glm(Lt ~ ., data = mat_PS2, family = "poisson")
glm_EH <- glm(Lt ~ ., data = mat_EH2, family = "poisson")

### GLM gaussien
glm_ER2 <- glm(Lt ~ ., data = mat_ER)
glm_PS2 <- glm(Lt ~ ., data = mat_PS)
glm_EH2 <- glm(Lt ~ ., data = mat_EH)

### Effets fixes
mat_ER2 %<>% mutate(Sol = factor("ER", levels = c("ER", "PS", "EH")))
mat_PS2 %<>% mutate(Sol = factor("PS", levels = c("ER", "PS", "EH")))
mat_EH2 %<>% mutate(Sol = factor("EH", levels = c("ER", "PS", "EH")))
mat <- rbind(mat_ER2, mat_PS2, mat_EH2)

fixed <- glm(Lt ~ . - 1, data = mat, family = "poisson")
fixed_lm <- lm(Lt ~ . - 1, data = mat)

### Graphiques

## ER
df_ER <- data.frame(Date = date2017[18:80], Observation = larves_ER[18:80],
                    Prédiction = predict(lm_ER))
plot_ER1 <- df_ER %>% ggplot() +
    aes(x = Observation, y = Prédiction) +
    geom_point() +
    theme_bw() +
    theme(aspect.ratio = 1) +
    ylim(c(0, 2605)) +
    xlim(c(0, 2605))

plot_ER2 <- df_ER %>% gather(Observation, Prédiction, key = cle, value = Nombre) %>% 
    ggplot() +
    aes(x = Date, y = Nombre, color = cle) +
    geom_point() +
    geom_line() +
    theme_bw() +
    ylab("Nombre de larves") +
    theme(legend.title = element_blank())

## PS
df_PS <- data.frame(Date = date2017[18:80], Observation = larves_PS[18:80],
                    Prédiction = predict(lm_PS))
plot_PS1 <- df_PS %>% ggplot() +
    aes(x = Observation, y = Prédiction) +
    geom_point() +
    theme_bw() +
    theme(aspect.ratio = 1) +
    ylim(c(-30, 1515)) +
    xlim(c(-30, 1515))

plot_PS2 <- df_PS %>% gather(Observation, Prédiction, key = cle, value = Nombre) %>% 
    ggplot() +
    aes(x = Date, y = Nombre, color = cle) +
    geom_point() +
    geom_line() +
    theme_bw() +
    theme(legend.title = element_blank())

## EH
df_EH <- data.frame(Date = date2017[18:80], Observation = larves_EH[18:80],
                    Prédiction = predict(lm_EH))
plot_EH1 <- df_EH %>% ggplot() +
    aes(x = Observation, y = Prédiction) +
    geom_point() +
    theme_bw() +
    theme(aspect.ratio = 1) +
    ylim(c(-385, 2950)) +
    xlim(c(-385, 2950))

plot_EH2 <- df_EH %>% gather(Observation, Prédiction, key = cle, value = Nombre) %>% 
    ggplot() +
    aes(x = Date, y = Nombre, color = cle) +
    geom_point() +
    geom_line() +
    theme_bw() +
    theme(legend.title = element_blank())

# Correlations ------------------------------------------------------------

for (day in 7:17) {
    # print(day)
    print(c(cor(larves_ER[(day + 1):80], inflos_ER[1:(80 - day)], method = "spearman"),
            cor(larves_PS[(day + 1):80], inflos_PS[1:(80 - day)], method = "spearman"),
            cor(larves_EH[(day + 1):80], inflos_EH[1:(80 - day)], method = "spearman")))
}

for (day in 1:17) {
    # print(day)
    print(c(cor(larves_ER[(day + 1):80], larves_ER[1:(80 - day)], method = "spearman"),
            cor(larves_PS[(day + 1):80], larves_PS[1:(80 - day)], method = "spearman"),
            cor(larves_EH[(day + 1):80], larves_EH[1:(80 - day)], method = "spearman")))
}


# Optimisation ------------------------------------------------------------

my_mae <- function(x, y) {
    n_obs <- length(x)
    sum(abs(x - y)) / n_obs
}

objectif_larves <- function(x, larve) {
    alpha <- x[1:68]
    r <- x[69]
    mu <- x[70]
    exo <- x[71:138]
    larves_lag <- lag(larve, 12)[-(1:12)]
    larves_pred <- alpha * r * (mu * larves_lag + exo)
    
    my_mae(larves_pred, larve[13:80])
}

res_ER <- nsga2(objectif_larves, 138, 1, larves_ER,
                lower.bounds = rep(0, 138),
                upper.bounds = rep(c(1, 10, 1, 15000), times = c(68, 1, 1, 68)),
                popsize = 800, generations = 2000)

res_PS <- nsga2(objectif_larves, 138, 1, larves_PS,
                lower.bounds = rep(0, 138),
                upper.bounds = rep(c(1, 10, 1, 15000), times = c(68, 1, 1, 68)),
                popsize = 800, generations = 2000)

res_EH <- nsga2(objectif_larves, 138, 1, larves_EH,
                lower.bounds = rep(0, 138), 
                upper.bounds = rep(c(1, 10, 1, 15000), times = c(68, 1, 1, 68)),
                popsize = 800, generations = 2000)

give <- function(x, larve) {
    alpha <- x[1:68]
    r <- x[69]
    mu <- x[70]
    exo <- x[71:138]
    larves_lag <- lag(larve, 12)[-(1:12)]
    larves_pred <- alpha * r * (mu * larves_lag + exo)
}


est_ER <- data.frame(Date = date2017[13:80],
                     Prédiction = give(res_ER$par[1, ], larves_ER),
                     Observation = larves_ER[13:80])

est_PS <- data.frame(Date = date2017[13:80],
                     Prédiction = give(res_PS$par[1, ], larves_PS),
                     Observation = larves_PS[13:80])

est_EH <- data.frame(Date = date2017[13:80],
                     Prédiction = give(res_EH$par[1, ], larves_EH),
                     Observation = larves_EH[13:80])

### Plot trajectoires
t1 <- est_ER %>% gather(Prédiction, Observation, key = statut, value = Nombre) %>% 
    ggplot() +
    aes(x = Date, y = Nombre, color = statut) +
    geom_point() +
    geom_line() +
    theme_bw() +
    theme(legend.title = element_blank(), legend.position = "bottom") +
    ggtitle("Enherbement ras")

t2 <- est_PS %>% gather(Prédiction, Observation, key = statut, value = Nombre) %>% 
    ggplot() +
    aes(x = Date, y = Nombre, color = statut) +
    geom_point() +
    geom_line() +
    theme_bw() +
    theme(legend.title = element_blank(), legend.position = "bottom") +
    ggtitle("Paillage synthétique")

t3 <- est_EH %>% gather(Prédiction, Observation, key = statut, value = Nombre) %>% 
    ggplot() +
    aes(x = Date, y = Nombre, color = statut) +
    geom_point() +
    geom_line() +
    theme_bw() +
    theme(legend.title = element_blank(), legend.position = "bottom") +
    ggtitle("Enherbement haut")

grid.arrange(t1, t2, t3, ncol = 3)


### plot pred vs obs
p1 <- est_ER %>% ggplot() +
    aes(x = Observation, y = Prédiction) +
    geom_point() +
    theme_bw() +
    theme(aspect.ratio = 1) +
    ggtitle("Enherbement ras")

p2 <- est_PS %>% ggplot() +
    aes(x = Observation, y = Prédiction) +
    geom_point() +
    theme_bw() +
    theme(aspect.ratio = 1) +
    ggtitle("Paillage synthétique")

p3 <- est_EH %>% ggplot() +
    aes(x = Observation, y = Prédiction) +
    geom_point() +
    theme_bw() +
    theme(aspect.ratio = 1) +
    ggtitle("Enherbement haut")

grid.arrange(p1, p2, p3, nrow = 1)


# Modèle global -----------------------------------------------------------

objectif_larves_global <- function(x) {
    r <- x[1]
    mu_ER <- x[2]
    mu_PS <- x[3]
    mu_EH <- x[4]
    alpha_ER <- x[5:72]
    alpha_PS <- x[73:140]
    alpha_EH <- x[141:208]
    exo_ER <- x[209:276]
    exo_PS <- x[277:344]
    exo_EH <- x[345:412]
    
    larves_pred_ER <- alpha_ER * r * (mu_ER * larves_ER[1:68] + exo_ER)
    larves_pred_PS <- alpha_PS * r * (mu_PS * larves_PS[1:68] + exo_PS)
    larves_pred_EH <- alpha_EH * r * (mu_EH * larves_EH[1:68] + exo_EH)
    
    c(my_mae(larves_pred_ER, larves_ER[13:80]),
      my_mae(larves_pred_PS, larves_PS[13:80]),
      my_mae(larves_pred_EH, larves_EH[13:80]))
}

res_glob <- nsga2(objectif_larves_global, 412, 3,
                lower.bounds = rep(0, 412),
                upper.bounds = rep(c(10, 1, 15000), times = c(1, 207, 204)),
                popsize = 800, generations = 8000)


ind_opt_glob <- res_glob$value %>% as_tibble %>%
    mutate(norm = abs(V1 + V2 + V3)) %$% 
    which.min(norm)

arg_opt_glob <- res_glob$par[ind_opt_glob, ]

give_global <- function(x) {
    r <- x[1]
    mu_ER <- x[2]
    mu_PS <- x[3]
    mu_EH <- x[4]
    alpha_ER <- x[5:72]
    alpha_PS <- x[73:140]
    alpha_EH <- x[141:208]
    exo_ER <- x[209:276]
    exo_PS <- x[277:344]
    exo_EH <- x[345:412]
    
    larves_pred_ER <- alpha_ER * r * (mu_ER * larves_ER[1:68] + exo_ER)
    larves_pred_PS <- alpha_PS * r * (mu_PS * larves_PS[1:68] + exo_PS)
    larves_pred_EH <- alpha_EH * r * (mu_EH * larves_EH[1:68] + exo_EH)
    
    matrix(c(larves_pred_ER, larves_pred_PS, larves_pred_EH), ncol = 3)
}

trajs <- give_global(arg_opt_glob)
est_ER2 <- data.frame(Date = date2017[13:80],
                     Prédiction = trajs[, 1],
                     Observation = larves_ER[13:80])

est_PS2 <- data.frame(Date = date2017[13:80],
                     Prédiction = trajs[, 2],
                     Observation = larves_PS[13:80])

est_EH2 <- data.frame(Date = date2017[13:80],
                     Prédiction = trajs[, 3],
                     Observation = larves_EH[13:80])

### Plot trajectoires
t1_2 <- est_ER2 %>% gather(Prédiction, Observation, key = statut, value = Nombre) %>% 
    ggplot() +
    aes(x = Date, y = Nombre, color = statut) +
    geom_point() +
    geom_line() +
    theme_bw() +
    theme(legend.title = element_blank(), legend.position = "bottom") +
    ggtitle("Enherbement ras")

t2_2 <- est_PS2 %>% gather(Prédiction, Observation, key = statut, value = Nombre) %>% 
    ggplot() +
    aes(x = Date, y = Nombre, color = statut) +
    geom_point() +
    geom_line() +
    theme_bw() +
    theme(legend.title = element_blank(), legend.position = "bottom") +
    ggtitle("Paillage synthétique")

t3_2 <- est_EH2 %>% gather(Prédiction, Observation, key = statut, value = Nombre) %>% 
    ggplot() +
    aes(x = Date, y = Nombre, color = statut) +
    geom_point() +
    geom_line() +
    theme_bw() +
    theme(legend.title = element_blank(), legend.position = "bottom") +
    ggtitle("Enherbement haut")

grid.arrange(t1_2, t2_2, t3_2, ncol = 3)


### plot pred vs obs
p1_2 <- est_ER2 %>% ggplot() +
    aes(x = Observation, y = Prédiction) +
    geom_point() +
    theme_bw() +
    theme(aspect.ratio = 1) +
    ggtitle("Enherbement ras")

p2_2 <- est_PS2 %>% ggplot() +
    aes(x = Observation, y = Prédiction) +
    geom_point() +
    theme_bw() +
    theme(aspect.ratio = 1) +
    ggtitle("Paillage synthétique")

p3_2 <- est_EH2 %>% ggplot() +
    aes(x = Observation, y = Prédiction) +
    geom_point() +
    theme_bw() +
    theme(aspect.ratio = 1) +
    ggtitle("Enherbement haut")

grid.arrange(p1_2, p2_2, p3_2, nrow = 1)

# LM larves + inflos ------------------------------------------------------

mat_ER3 <- matrix(NA, nrow = 63, ncol = 30)
mat_ER3[1:63] <- larves_ER[18:80]
for (colonne in 1:11) {
    mat_ER3[, colonne + 1] <- larves_ER[(12 - colonne):(74 - colonne)]
}
mat_ER3[, 13] <- inflos_ER[18:80]
for (colonne in 1:17) {
    mat_ER3[, colonne + 13] <- inflos_ER[(18 - colonne):(80 - colonne)]
}

colnames(mat_ER3) <- c("Lt", paste0("Lt-", 7:17), "It", paste0("It-", 1:17))



mat_PS3 <- matrix(NA, nrow = 63, ncol = 30)
mat_PS3[1:63] <- larves_PS[18:80]
for (colonne in 1:11) {
    mat_PS3[, colonne + 1] <- larves_PS[(12 - colonne):(74 - colonne)]
}
mat_PS3[, 13] <- inflos_PS[18:80]
for (colonne in 1:17) {
    mat_PS3[, colonne + 13] <- inflos_PS[(18 - colonne):(80 - colonne)]
}

colnames(mat_PS3) <- c("Lt", paste0("Lt-", 7:17), "It", paste0("It-", 1:17))




mat_EH3 <- matrix(NA, nrow = 63, ncol = 30)
mat_EH3[1:63] <- larves_EH[18:80]
for (colonne in 1:11) {
    mat_EH3[, colonne + 1] <- larves_EH[(12 - colonne):(74 - colonne)]
}
mat_EH3[, 13] <- inflos_EH[18:80]
for (colonne in 1:17) {
    mat_EH3[, colonne + 13] <- inflos_EH[(18 - colonne):(80 - colonne)]
}

colnames(mat_EH3) <- c("Lt", paste0("Lt-", 7:17), "It", paste0("It-", 1:17))



mat_ER3 %<>% as_tibble() %>% mutate(Sol = factor("ER", levels = c("ER", "PS", "EH")))
mat_PS3 %<>% as_tibble() %>% mutate(Sol = factor("PS", levels = c("ER", "PS", "EH")))
mat_EH3 %<>% as_tibble() %>% mutate(Sol = factor("EH", levels = c("ER", "PS", "EH")))

df <- bind_rows(mat_ER3, mat_PS3, mat_EH3)

fit <- lm(Lt ~ . - 1, data = df)

df2 <- cbind(Observation = df$Lt, Prédiction = fit$fitted.values) %>%
    as.data.frame() %>%
    mutate(Sol = rep(c("ER", "PS", "EH"), each = 63)) %>% 
    ggplot() +
    aes(x = Observation, y = Prédiction, color = Sol) +
    geom_point() + 
    theme_bw() +
    theme(aspect.ratio = 1)

df_ER2 <- df_ER
df_ER2$Prédiction <- fit$fitted.values[1:63]
m1 <- df_ER2 %>% gather(Observation, Prédiction, key = statut, value = Nombre) %>% 
    ggplot +
    aes(x = Date, y = Nombre, color = statut) +
    geom_point() +
    geom_line() +
    theme_bw() +
    theme(legend.title = element_blank(), legend.position = "bottom") +
    ggtitle("Enherbement ras")

df_PS2 <- df_PS
df_PS2$Prédiction <- fit$fitted.values[64:126]
m2 <- df_PS2 %>% gather(Observation, Prédiction, key = statut, value = Nombre) %>% 
    ggplot +
    aes(x = Date, y = Nombre, color = statut) +
    geom_point() +
    geom_line()+
    theme_bw() +
    theme(legend.title = element_blank(), legend.position = "bottom") +
    ggtitle("Paillage synthétique")

df_EH2 <- df_EH
df_EH2$Prédiction <- fit$fitted.values[127:189]
m3 <- df_EH2 %>% gather(Observation, Prédiction, key = statut, value = Nombre) %>% 
    ggplot +
    aes(x = Date, y = Nombre, color = statut) +
    geom_point() +
    geom_line()+
    theme_bw() +
    theme(legend.title = element_blank(), legend.position = "bottom") +
    ggtitle("Enherbement haut")

grid.arrange(m1, m2, m3, ncol = 3)
