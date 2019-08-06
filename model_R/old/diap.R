
area_circle <- function(x) {
    ## Donne l'aire d'un cercle en fonction de son périmètre.
    x^2 / (4 * pi)
}

area_arbre <- area_circle(c(14.2, 14, 14.3, 15.2, 13.3, 12.5, 15.8)) %>% mean

