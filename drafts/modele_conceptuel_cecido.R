## Script relatif au modèle conceptuel de cécidomyies
## L_t = alpha_t * R * (mu * L_t-12 + E_t-7)

# Packages / data ---------------------------------------------------------

library(tidyverse)
library(magrittr)
library(mco)
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


# Clem --------------------------------------------------------------------

library(tidyverse)

## Je génère des données pour l'exemple
toto <- quantile(rnorm(100), seq(0, 1, 0.01))
tutu <- quantile(rnorm(100, mean = 0.5), seq(0, 1, 0.01))

## Avec ggplot
df <- data.frame(toto, tutu)
df %>% ggplot +
    aes(x = tutu, y = toto) +
    geom_point() +
    ylim(c(-2.2, 2.7)) + ## J'ajuste les limites à la main
    xlim(c(-2.2, 2.7)) + ## Les mêmes pour x et y
    theme_bw() +
    theme(aspect.ratio = 1)
