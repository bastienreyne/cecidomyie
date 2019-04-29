
# Packages ----------------------------------------------------------------

library(mco)
source("new.R")

# Cost functions ----------------------------------------------------------

my_mae <- function(x, y) {
    n_obs <- length(x)
    sum(abs(x - y)) / n_obs
}

my_rmse <- function(x, y) {
    n_obs <- length(x)
    sqrt(sum((x - y)^2) / n_obs)
}

# Objective ---------------------------------------------------------------

objective <- function(args, cost_function) {
    larves_est <- dynamics(args)
    c(cost_function(larves_est[, 1], larves_obs[, 1]),
      cost_function(larves_est[, 2], larves_obs[, 2]),
      cost_function(larves_est[, 3], larves_obs[, 3]))
}

# NSGA-II -----------------------------------------------------------------

bornes_inf <- rep(0, 10)
bornes_sup <- c(2, 10, 1, 1, 1, 60, 60, 60, 80, 80)
res_mae <- nsga2(objective, 10, 3, my_mae, 
                 lower.bounds = bornes_inf, upper.bounds = bornes_sup, 
                 popsize = 500, generations = 50)
# res_rmse <- nsga2(objective, 7, 3, my_mae, 
#                   lower.bounds = bornes_inf, upper.bounds = bornes_sup, 
#                   popsize = 500, generations = 50)

# save(res_mae, res_rmse, file = "inchallah.Rdata")

# Summary -----------------------------------------------------------------

# load("init.Rdata")
res_mae$value %>% as_tibble() %>% mutate(norm = V1 + V2 + V3) %>% pull(norm) %>% which.min() ## 6506
res_mae$value[252, ] %>% sum ## 1056
par_opt <- res_mae$par[252, ]
traj_opt <- dynamics(par_opt)

to_plot <- cbind(traj_opt, larves_obs) %>% as_tibble() %>% mutate(date = 1:80)
to_plot %>% ggplot(aes(x = date, y = larves_EH)) + geom_line() + geom_line(aes(y = V3), col = "red")
