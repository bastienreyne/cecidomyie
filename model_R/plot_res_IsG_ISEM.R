## Script avec des fonctions qui plottent / version congres ISEM2019


# packages / data ---------------------------------------------------------

library(tidyverse)
data_piege <- read.csv("/home/bastien/cecidomyie/data/2017_piege.csv")
# data_piege <- read.csv("D:/Mes donnees/GitHub_Bastien/cecidomyie-master/data/2017_piege.csv")
larves_obs <- cbind(data_piege %>% filter(Sol == "ER") %>% pull(larves),
                    data_piege %>% filter(Sol == "PS") %>% pull(larves),
                    data_piege %>% filter(Sol == "EH") %>% pull(larves))
inflos_obs <- cbind(data_piege %>% filter(Sol == "ER") %>% pull(inflos),
                    data_piege %>% filter(Sol == "PS") %>% pull(inflos),
                    data_piege %>% filter(Sol == "EH") %>% pull(inflos))
data_piege_b2 <- read.csv("/home/bastien/cecidomyie/data/2017_piege_bloc2.csv")
# data_piege_b2 <- read.csv("D:/Mes donnees/GitHub_Bastien/cecidomyie-master/data/2017_piege_bloc2.csv")
larves_obs_b2 <- cbind(data_piege_b2 %>% filter(Sol == "ER") %>% pull(larves),
                       data_piege_b2 %>% filter(Sol == "PS") %>% pull(larves),
                       data_piege_b2 %>% filter(Sol == "EH") %>% pull(larves))

# Sys.setlocale("LC_TIME","English")


# Plots function ----------------------------------------------------------

plot_dynamics_A <- function(args, inflos) {
    obs <- larves_obs
    est <- dynamics_A(args, inflos)

    er <- data.frame(date = date2017, obs = obs[, 1], est = est[, 1]) %>%
        mutate(Sol = factor("Low weed cover",
                            levels = c("Low weed cover", 
                                       "Synthetic mulching", 
                                       "High weed cover"))) %>% 
        gather(obs, est, key = statut, value = nombre, factor_key = TRUE)
    
    ps <- data.frame(date = date2017, obs = obs[, 2], est = est[, 2]) %>%
        mutate(Sol = factor("Synthetic mulching", 
        										levels = c("Low weed cover", 
        															 "Synthetic mulching", 
        															 "High weed cover"))) %>% 
    	gather(obs, est, key = statut, value = nombre, factor_key = TRUE)
    
    eh <- data.frame(date = date2017, obs = obs[, 3], est = est[, 3]) %>%
        mutate(Sol = factor("High weed cover",
        										levels = c("Low weed cover", 
        															 "Synthetic mulching", 
        															 "High weed cover"))) %>% 
    	gather(obs, est, key = statut, value = nombre, factor_key = TRUE)
    
    to_plot <- bind_rows(er, ps, eh)
    to_plot %>% ggplot +
        aes(x = date, y = nombre, color = statut) +
        geom_point() +
        geom_line() +
        facet_grid(. ~ Sol) +
        theme_bw() +
        theme(legend.title = element_blank(), legend.position = "bottom") +
        # scale_color_discrete(labels = c("Observation", "Estimation")) +
        scale_color_manual(values = c("black", "green4"), labels = c("Observed", "Estimated")) +
    		xlab(element_blank()) +
    		theme(axis.text.x = element_text(size=10, angle=45, vjust=1, hjust=1)) +
        ylab("Number of larvae")
}

plot_dynamics <- function(args, inflos) {
    obs <- larves_obs
    est <- dynamics(args, inflos)
    
    er <- data.frame(date = date2017, obs = obs[, 1], est = est[, 1]) %>%
        mutate(Sol = factor("Enherbement ras",
        										levels = c("Low weed cover", 
        															 "Synthetic mulching", 
        															 "High weed cover"))) %>% 
    	gather(obs, est, key = statut, value = nombre, factor_key = TRUE)
    
    ps <- data.frame(date = date2017, obs = obs[, 2], est = est[, 2]) %>%
        mutate(Sol = factor("Paillage synthétique", 
        										levels = c("Low weed cover", 
        															 "Synthetic mulching", 
        															 "High weed cover"))) %>% 
    	gather(obs, est, key = statut, value = nombre, factor_key = TRUE)
    
    eh <- data.frame(date = date2017, obs = obs[, 3], est = est[, 3]) %>%
        mutate(Sol = factor("Enherbement haut",
        										levels = c("Low weed cover", 
        															 "Synthetic mulching", 
        															 "High weed cover"))) %>% 
    	gather(obs, est, key = statut, value = nombre, factor_key = TRUE)
    
    to_plot <- bind_rows(er, ps, eh)
    to_plot %>% ggplot +
        aes(x = date, y = nombre, color = statut) +
        geom_point() +
        geom_line() +
        facet_grid(. ~ Sol) +
        theme_bw() +
        theme(legend.title = element_blank(), legend.position = "bottom") +
        scale_color_discrete(labels = c("Observation", "Estimation")) +
        xlab("Date") +
        ylab("Nombre de larves")
}

plot_decompo_A <- function(args, inflos) {
    obs <- larves_obs
    estimations <- decomposition_A(args, inflos)
    
    L_obs <- apply(obs, 2, sum)
    L_sim <- apply(estimations[[1]], 2, sum)
    # avec inflos CDE
    LperI0_CDE_obs <- apply(obs/inflos, 2, mean, na.rm=T)
    LperI0_CDE_sim <- apply(estimations[[1]]/inflos, 2, mean, na.rm=T)
    LperI_CDE_obs <- L_obs / apply(inflos, 2, sum)
    LperI_CDE_sim <- L_sim / apply(inflos, 2, sum)
    # avec toutes les inflos
    LperI0_obs <- apply(obs/inflos_obs, 2, mean, na.rm=T)
    LperI0_sim <- apply(estimations[[1]]/inflos_obs, 2, mean, na.rm=T)
    LperI_obs <- L_obs / apply(inflos_obs, 2, sum)
    LperI_sim <- L_sim / apply(inflos_obs, 2, sum)
    print(list(rbind(L_obs, LperI0_CDE_obs, LperI_CDE_obs, LperI0_obs, LperI_obs),
    					 rbind(L_sim, LperI0_CDE_sim, LperI_CDE_sim, LperI0_sim, LperI_sim)))
    
    ploter <- data.frame(Date = date2017,
                         Sol = factor("Low weed cover",
                         						 levels = c("Low weed cover", 
                         						 					 "Synthetic mulching", 
                         						 					 "High weed cover")), 
    										 Observed = obs[, 1],
                         Estimated = estimations[[1]][, 1],
                         F_pup = estimations[[2]][, 1],
                         F_diap = estimations[[3]][, 1],
                         F_endo = estimations[[4]][, 1],
                         F_exo = estimations[[5]][, 1]) %>% 
        gather(F_exo, F_endo, F_diap, F_pup,
               key = prov, value = prop, factor_key = TRUE)
    
    plotps <- data.frame(Date = date2017,
                         Sol = factor("Synthetic mulching",
                         						 levels = c("Low weed cover", 
                         						 					 "Synthetic mulching", 
                         						 					 "High weed cover")), 
    										 Observed = obs[, 2],
                         Estimated = estimations[[1]][, 2],
                         F_pup = estimations[[2]][, 2],
                         F_diap = estimations[[3]][, 2],
                         F_endo = estimations[[4]][, 2],
                         F_exo = estimations[[5]][, 2]) %>% 
    	gather(F_exo, F_endo, F_diap, F_pup,
    				 key = prov, value = prop, factor_key = TRUE)
    
    ploteh <- data.frame(Date = date2017,
                         Sol = factor("High weed cover",
                         						 levels = c("Low weed cover", 
                         						 					 "Synthetic mulching", 
                         						 					 "High weed cover")), 
    										 Observed = obs[, 3],
                         Estimated = estimations[[1]][, 3],
                         F_pup = estimations[[2]][, 3],
                         F_diap = estimations[[3]][, 3],
                         F_endo = estimations[[4]][, 3],
                         F_exo = estimations[[5]][, 3]) %>% 
    	gather(F_exo, F_endo, F_diap, F_pup,
    				 key = prov, value = prop, factor_key = TRUE)
    
    to_plot <- bind_rows(ploter, plotps, ploteh)
    to_plot %>% ggplot +
        aes(x = Date) +
        geom_area(aes(y = prop, fill = prov), alpha = 0.5) +
        geom_line(aes(y = Observed, color = "Observed"), lwd = 0.75) +
        geom_line(aes(y = Estimated, color = "Estimated"), lwd = 0.75) +
        geom_point(aes(y = Observed, color = "Observed")) +
        geom_point(aes(y = Estimated, color = "Estimated")) +
        theme_bw() +
        facet_grid(. ~ Sol) +
        theme(legend.title = element_blank(), legend.position = "right") +
        scale_color_manual(values = c("green4", "black")) +
				xlab(element_blank()) +
	    	theme(axis.text.x = element_text(size=10, angle=45, vjust=1, hjust=1)) +
				ylab("Number of larvae")
    
}

plot_decompo_B <- function(args, inflos) {
    obs <- larves_obs
    estimations <- decomposition_B(args, inflos)

    L_obs <- apply(obs, 2, sum)
    L_sim <- apply(estimations[[1]], 2, sum)
    # avec inflos CDE
    LperI0_CDE_obs <- apply(obs/inflos, 2, mean, na.rm=T)
    LperI0_CDE_sim <- apply(estimations[[1]]/inflos, 2, mean, na.rm=T)
    LperI_CDE_obs <- L_obs / apply(inflos, 2, sum)
    LperI_CDE_sim <- L_sim / apply(inflos, 2, sum)
    # avec toutes les inflos
    LperI0_obs <- apply(obs/inflos_obs, 2, mean, na.rm=T)
    LperI0_sim <- apply(estimations[[1]]/inflos_obs, 2, mean, na.rm=T)
    LperI_obs <- L_obs / apply(inflos_obs, 2, sum)
    LperI_sim <- L_sim / apply(inflos_obs, 2, sum)
    print(list(rbind(L_obs, LperI0_CDE_obs, LperI_CDE_obs, LperI0_obs, LperI_obs),
    					 rbind(L_sim, LperI0_CDE_sim, LperI_CDE_sim, LperI0_sim, LperI_sim)))
    
    ploter <- data.frame(Date = date2017,
    										 Sol = factor("Low weed cover",
    										 						 levels = c("Low weed cover", 
    										 						 					 "Synthetic mulching", 
    										 						 					 "High weed cover")), 
    										 Observed = obs[, 1],
    										 Estimated = estimations[[1]][, 1],
    										 F_pup = estimations[[2]][, 1],
    										 F_diap = estimations[[3]][, 1],
    										 F_endo = estimations[[4]][, 1],
    										 F_exo = estimations[[5]][, 1]) %>% 
    	gather(F_exo, F_endo, F_diap, F_pup,
    				 key = prov, value = prop, factor_key = TRUE)
    
    plotps <- data.frame(Date = date2017,
    										 Sol = factor("Synthetic mulching",
    										 						 levels = c("Low weed cover", 
    										 						 					 "Synthetic mulching", 
    										 						 					 "High weed cover")), 
    										 Observed = obs[, 2],
    										 Estimated = estimations[[1]][, 2],
    										 F_pup = estimations[[2]][, 2],
    										 F_diap = estimations[[3]][, 2],
    										 F_endo = estimations[[4]][, 2],
    										 F_exo = estimations[[5]][, 2]) %>% 
    	gather(F_exo, F_endo, F_diap, F_pup,
    				 key = prov, value = prop, factor_key = TRUE)
    
    ploteh <- data.frame(Date = date2017,
    										 Sol = factor("High weed cover",
    										 						 levels = c("Low weed cover", 
    										 						 					 "Synthetic mulching", 
    										 						 					 "High weed cover")), 
    										 Observed = obs[, 3],
    										 Estimated = estimations[[1]][, 3],
    										 F_pup = estimations[[2]][, 3],
    										 F_diap = estimations[[3]][, 3],
    										 F_endo = estimations[[4]][, 3],
    										 F_exo = estimations[[5]][, 3]) %>% 
    	gather(F_exo, F_endo, F_diap, F_pup,
    				 key = prov, value = prop, factor_key = TRUE)
    
    to_plot <- bind_rows(ploter, plotps, ploteh)
    to_plot %>% ggplot +
    	aes(x = Date) +
    	geom_area(aes(y = prop, fill = prov), alpha = 0.5) +
    	geom_line(aes(y = Observed, color = "Observed"), lwd = 0.75) +
    	geom_line(aes(y = Estimated, color = "Estimated"), lwd = 0.75) +
    	geom_point(aes(y = Observed, color = "Observed")) +
    	geom_point(aes(y = Estimated, color = "Estimated")) +
    	theme_bw() +
    	facet_grid(. ~ Sol) +
    	theme(legend.title = element_blank(), legend.position = "right") +
    	scale_color_manual(values = c("green4", "black")) +
    	xlab(element_blank()) +
    	theme(axis.text.x = element_text(size=10, angle=45, vjust=1, hjust=1)) +
    	ylab("Number of larvae")
}

plot_decompo_B2 <- function(args, inflos) {
    obs <- larves_obs
    estimations <- decomposition_B2(args, inflos)

    L_obs <- apply(obs, 2, sum)
    L_sim <- apply(estimations[[1]], 2, sum)
    # avec inflos CDE
    LperI0_CDE_obs <- apply(obs/inflos, 2, mean, na.rm=T)
    LperI0_CDE_sim <- apply(estimations[[1]]/inflos, 2, mean, na.rm=T)
    LperI_CDE_obs <- L_obs / apply(inflos, 2, sum)
    LperI_CDE_sim <- L_sim / apply(inflos, 2, sum)
    # avec toutes les inflos
    LperI0_obs <- apply(obs/inflos_obs, 2, mean, na.rm=T)
    LperI0_sim <- apply(estimations[[1]]/inflos_obs, 2, mean, na.rm=T)
    LperI_obs <- L_obs / apply(inflos_obs, 2, sum)
    LperI_sim <- L_sim / apply(inflos_obs, 2, sum)
    print(list(rbind(L_obs, LperI0_CDE_obs, LperI_CDE_obs, LperI0_obs, LperI_obs),
    					 rbind(L_sim, LperI0_CDE_sim, LperI_CDE_sim, LperI0_sim, LperI_sim)))
    
    ploter <- data.frame(Date = date2017,
    										 Sol = factor("Low weed cover",
    										 						 levels = c("Low weed cover", 
    										 						 					 "Synthetic mulching", 
    										 						 					 "High weed cover")), 
    										 Observed = obs[, 1],
    										 Estimated = estimations[[1]][, 1],
    										 F_pup = estimations[[2]][, 1],
    										 F_diap = estimations[[3]][, 1],
    										 F_endo = estimations[[4]][, 1],
    										 F_exo = estimations[[5]][, 1]) %>% 
    	gather(F_exo, F_endo, F_diap, F_pup,
    				 key = prov, value = prop, factor_key = TRUE)
    
    plotps <- data.frame(Date = date2017,
    										 Sol = factor("Synthetic mulching",
    										 						 levels = c("Low weed cover", 
    										 						 					 "Synthetic mulching", 
    										 						 					 "High weed cover")), 
    										 Observed = obs[, 2],
    										 Estimated = estimations[[1]][, 2],
    										 F_pup = estimations[[2]][, 2],
    										 F_diap = estimations[[3]][, 2],
    										 F_endo = estimations[[4]][, 2],
    										 F_exo = estimations[[5]][, 2]) %>% 
    	gather(F_exo, F_endo, F_diap, F_pup,
    				 key = prov, value = prop, factor_key = TRUE)
    
    ploteh <- data.frame(Date = date2017,
    										 Sol = factor("High weed cover",
    										 						 levels = c("Low weed cover", 
    										 						 					 "Synthetic mulching", 
    										 						 					 "High weed cover")), 
    										 Observed = obs[, 3],
    										 Estimated = estimations[[1]][, 3],
    										 F_pup = estimations[[2]][, 3],
    										 F_diap = estimations[[3]][, 3],
    										 F_endo = estimations[[4]][, 3],
    										 F_exo = estimations[[5]][, 3]) %>% 
    	gather(F_exo, F_endo, F_diap, F_pup,
    				 key = prov, value = prop, factor_key = TRUE)
    
    to_plot <- bind_rows(ploter, plotps, ploteh)
    to_plot %>% ggplot +
    	aes(x = Date) +
    	geom_area(aes(y = prop, fill = prov), alpha = 0.5) +
    	geom_line(aes(y = Observed, color = "Observed"), lwd = 0.75) +
    	geom_line(aes(y = Estimated, color = "Estimated"), lwd = 0.75) +
    	geom_point(aes(y = Observed, color = "Observed")) +
    	geom_point(aes(y = Estimated, color = "Estimated")) +
    	theme_bw() +
    	facet_grid(. ~ Sol) +
    	theme(legend.title = element_blank(), legend.position = "right") +
    	scale_color_manual(values = c("green4", "black")) +
    	xlab(element_blank()) +
    	theme(axis.text.x = element_text(size=10, angle=45, vjust=1, hjust=1)) +
    	ylab("Number of larvae")
}

plot_decompo_C <- function(args, inflos) {
    obs <- larves_obs
    estimations <- decomposition_C(args, inflos)

    L_obs <- apply(obs, 2, sum)
    L_sim <- apply(estimations[[1]], 2, sum)
		# avec inflos CDE
    LperI0_CDE_obs <- apply(obs/inflos, 2, mean, na.rm=T)
    LperI0_CDE_sim <- apply(estimations[[1]]/inflos, 2, mean, na.rm=T)
    LperI_CDE_obs <- L_obs / apply(inflos, 2, sum)
    LperI_CDE_sim <- L_sim / apply(inflos, 2, sum)
		# avec toutes les inflos
    LperI0_obs <- apply(obs/inflos_obs, 2, mean, na.rm=T)
    LperI0_sim <- apply(estimations[[1]]/inflos_obs, 2, mean, na.rm=T)
    LperI_obs <- L_obs / apply(inflos_obs, 2, sum)
    LperI_sim <- L_sim / apply(inflos_obs, 2, sum)
		print(list(rbind(L_obs, LperI0_CDE_obs, LperI_CDE_obs, LperI0_obs, LperI_obs),
    					 rbind(L_sim, LperI0_CDE_sim, LperI_CDE_sim, LperI0_sim, LperI_sim)))
    
    ploter <- data.frame(Date = date2017,
    										 Sol = factor("Low weed cover",
    										 						 levels = c("Low weed cover", 
    										 						 					 "Synthetic mulching", 
    										 						 					 "High weed cover")), 
    										 Observed = obs[, 1],
    										 Estimated = estimations[[1]][, 1],
    										 F_pup = estimations[[2]][, 1],
    										 F_diap = estimations[[3]][, 1],
    										 F_endo = estimations[[4]][, 1],
    										 F_exo = estimations[[5]][, 1]) %>% 
    	gather(F_exo, F_endo, F_diap, F_pup,
    				 key = prov, value = prop, factor_key = TRUE)
    
    plotps <- data.frame(Date = date2017,
    										 Sol = factor("Synthetic mulching",
    										 						 levels = c("Low weed cover", 
    										 						 					 "Synthetic mulching", 
    										 						 					 "High weed cover")), 
    										 Observed = obs[, 2],
    										 Estimated = estimations[[1]][, 2],
    										 F_pup = estimations[[2]][, 2],
    										 F_diap = estimations[[3]][, 2],
    										 F_endo = estimations[[4]][, 2],
    										 F_exo = estimations[[5]][, 2]) %>% 
    	gather(F_exo, F_endo, F_diap, F_pup,
    				 key = prov, value = prop, factor_key = TRUE)
    
    ploteh <- data.frame(Date = date2017,
    										 Sol = factor("High weed cover",
    										 						 levels = c("Low weed cover", 
    										 						 					 "Synthetic mulching", 
    										 						 					 "High weed cover")), 
    										 Observed = obs[, 3],
    										 Estimated = estimations[[1]][, 3],
    										 F_pup = estimations[[2]][, 3],
    										 F_diap = estimations[[3]][, 3],
    										 F_endo = estimations[[4]][, 3],
    										 F_exo = estimations[[5]][, 3]) %>% 
    	gather(F_exo, F_endo, F_diap, F_pup,
    				 key = prov, value = prop, factor_key = TRUE)
    
    to_plot <- bind_rows(ploter, plotps, ploteh)
    to_plot %>% ggplot +
    	aes(x = Date) +
    	geom_area(aes(y = prop, fill = prov), alpha = 0.5) +
    	geom_line(aes(y = Observed, color = "Observed"), lwd = 0.75) +
    	geom_line(aes(y = Estimated, color = "Estimated"), lwd = 0.75) +
    	geom_point(aes(y = Observed, color = "Observed")) +
    	geom_point(aes(y = Estimated, color = "Estimated")) +
    	theme_bw() +
    	facet_grid(. ~ Sol) +
    	theme(legend.title = element_blank(), legend.position = "right") +
    	scale_color_manual(values = c("green4", "black")) +
    	xlab(element_blank()) +
    	theme(axis.text.x = element_text(size=10, angle=45, vjust=1, hjust=1)) +
    	ylab("Number of larvae")
}

plot_decompo <- function(args, inflos) {
    obs <- larves_obs
    estimations <- decomposition(args, inflos)
    
    L_obs <- apply(obs, 2, sum)
    L_sim <- apply(estimations[[1]], 2, sum)
    # avec inflos CDE
    LperI0_CDE_obs <- apply(obs/inflos, 2, mean, na.rm=T)
    LperI0_CDE_sim <- apply(estimations[[1]]/inflos, 2, mean, na.rm=T)
    LperI_CDE_obs <- L_obs / apply(inflos, 2, sum)
    LperI_CDE_sim <- L_sim / apply(inflos, 2, sum)
    # avec toutes les inflos
    LperI0_obs <- apply(obs/inflos_obs, 2, mean, na.rm=T)
    LperI0_sim <- apply(estimations[[1]]/inflos_obs, 2, mean, na.rm=T)
    LperI_obs <- L_obs / apply(inflos_obs, 2, sum)
    LperI_sim <- L_sim / apply(inflos_obs, 2, sum)
    print(list(rbind(L_obs, LperI0_CDE_obs, LperI_CDE_obs, LperI0_obs, LperI_obs),
    					 rbind(L_sim, LperI0_CDE_sim, LperI_CDE_sim, LperI0_sim, LperI_sim)))
    
    ploter <- data.frame(Date = date2017,
    										 Sol = factor("Low weed cover",
    										 						 levels = c("Low weed cover", 
    										 						 					 "Synthetic mulching", 
    										 						 					 "High weed cover")), 
    										 Observed = obs[, 1],
    										 Estimated = estimations[[1]][, 1],
    										 F_pup = estimations[[2]][, 1],
    										 F_diap = estimations[[3]][, 1],
    										 F_endo = estimations[[4]][, 1],
    										 F_exo = estimations[[5]][, 1]) %>% 
    	gather(F_exo, F_endo, F_diap, F_pup,
    				 key = prov, value = prop, factor_key = TRUE)
    
    plotps <- data.frame(Date = date2017,
    										 Sol = factor("Synthetic mulching",
    										 						 levels = c("Low weed cover", 
    										 						 					 "Synthetic mulching", 
    										 						 					 "High weed cover")), 
    										 Observed = obs[, 2],
    										 Estimated = estimations[[1]][, 2],
    										 F_pup = estimations[[2]][, 2],
    										 F_diap = estimations[[3]][, 2],
    										 F_endo = estimations[[4]][, 2],
    										 F_exo = estimations[[5]][, 2]) %>% 
    	gather(F_exo, F_endo, F_diap, F_pup,
    				 key = prov, value = prop, factor_key = TRUE)
    
    ploteh <- data.frame(Date = date2017,
    										 Sol = factor("High weed cover",
    										 						 levels = c("Low weed cover", 
    										 						 					 "Synthetic mulching", 
    										 						 					 "High weed cover")), 
    										 Observed = obs[, 3],
    										 Estimated = estimations[[1]][, 3],
    										 F_pup = estimations[[2]][, 3],
    										 F_diap = estimations[[3]][, 3],
    										 F_endo = estimations[[4]][, 3],
    										 F_exo = estimations[[5]][, 3]) %>% 
    	gather(F_exo, F_endo, F_diap, F_pup,
    				 key = prov, value = prop, factor_key = TRUE)
    
    to_plot <- bind_rows(ploter, plotps, ploteh)
    to_plot %>% ggplot +
    	aes(x = Date) +
    	geom_area(aes(y = prop, fill = prov), alpha = 0.5) +
    	geom_line(aes(y = Observed, color = "Observed"), lwd = 0.75) +
    	geom_line(aes(y = Estimated, color = "Estimated"), lwd = 0.75) +
    	geom_point(aes(y = Observed, color = "Observed")) +
    	geom_point(aes(y = Estimated, color = "Estimated")) +
    	theme_bw() +
    	facet_grid(. ~ Sol) +
    	theme(legend.title = element_blank(), legend.position = "right") +
    	scale_color_manual(values = c("green4", "black")) +
    	xlab(element_blank()) +
    	theme(axis.text.x = element_text(size=10, angle=45, vjust=1, hjust=1)) +
    	ylab("Number of larvae")
}

plot_decompo_C2 <- function(args, inflos) {
    obs <- larves_obs
    estimations <- decomposition_C2(args, inflos)
    
    L_obs <- apply(obs, 2, sum)
    L_sim <- apply(estimations[[1]], 2, sum)
    # avec inflos CDE
    LperI0_CDE_obs <- apply(obs/inflos, 2, mean, na.rm=T)
    LperI0_CDE_sim <- apply(estimations[[1]]/inflos, 2, mean, na.rm=T)
    LperI_CDE_obs <- L_obs / apply(inflos, 2, sum)
    LperI_CDE_sim <- L_sim / apply(inflos, 2, sum)
    # avec toutes les inflos
    LperI0_obs <- apply(obs/inflos_obs, 2, mean, na.rm=T)
    LperI0_sim <- apply(estimations[[1]]/inflos_obs, 2, mean, na.rm=T)
    LperI_obs <- L_obs / apply(inflos_obs, 2, sum)
    LperI_sim <- L_sim / apply(inflos_obs, 2, sum)
    print(list(rbind(L_obs, LperI0_CDE_obs, LperI_CDE_obs, LperI0_obs, LperI_obs),
    					 rbind(L_sim, LperI0_CDE_sim, LperI_CDE_sim, LperI0_sim, LperI_sim)))
    
    ploter <- data.frame(Date = date2017,
    										 Sol = factor("Low weed cover",
    										 						 levels = c("Low weed cover", 
    										 						 					 "Synthetic mulching", 
    										 						 					 "High weed cover")), 
    										 Observed = obs[, 1],
    										 Estimated = estimations[[1]][, 1],
    										 F_pup = estimations[[2]][, 1],
    										 F_diap = estimations[[3]][, 1],
    										 F_endo = estimations[[4]][, 1],
    										 F_exo = estimations[[5]][, 1]) %>% 
    	gather(F_exo, F_endo, F_diap, F_pup,
    				 key = prov, value = prop, factor_key = TRUE)
    
    plotps <- data.frame(Date = date2017,
    										 Sol = factor("Synthetic mulching",
    										 						 levels = c("Low weed cover", 
    										 						 					 "Synthetic mulching", 
    										 						 					 "High weed cover")), 
    										 Observed = obs[, 2],
    										 Estimated = estimations[[1]][, 2],
    										 F_pup = estimations[[2]][, 2],
    										 F_diap = estimations[[3]][, 2],
    										 F_endo = estimations[[4]][, 2],
    										 F_exo = estimations[[5]][, 2]) %>% 
    	gather(F_exo, F_endo, F_diap, F_pup,
    				 key = prov, value = prop, factor_key = TRUE)
    
    ploteh <- data.frame(Date = date2017,
    										 Sol = factor("High weed cover",
    										 						 levels = c("Low weed cover", 
    										 						 					 "Synthetic mulching", 
    										 						 					 "High weed cover")), 
    										 Observed = obs[, 3],
    										 Estimated = estimations[[1]][, 3],
    										 F_pup = estimations[[2]][, 3],
    										 F_diap = estimations[[3]][, 3],
    										 F_endo = estimations[[4]][, 3],
    										 F_exo = estimations[[5]][, 3]) %>% 
    	gather(F_exo, F_endo, F_diap, F_pup,
    				 key = prov, value = prop, factor_key = TRUE)
    
    to_plot <- bind_rows(ploter, plotps, ploteh)
    to_plot %>% ggplot +
    	aes(x = Date) +
    	geom_area(aes(y = prop, fill = prov), alpha = 0.5) +
    	geom_line(aes(y = Observed, color = "Observed"), lwd = 0.75) +
    	geom_line(aes(y = Estimated, color = "Estimated"), lwd = 0.75) +
    	geom_point(aes(y = Observed, color = "Observed")) +
    	geom_point(aes(y = Estimated, color = "Estimated")) +
    	theme_bw() +
    	facet_grid(. ~ Sol) +
    	theme(legend.title = element_blank(), legend.position = "right") +
    	scale_color_manual(values = c("green4", "black")) +
    	xlab(element_blank()) +
    	theme(axis.text.x = element_text(size=10, angle=45, vjust=1, hjust=1)) +
    	ylab("Number of larvae")
}

plot_decompo_b2 <- function(args, inflos) {
    obs <- larves_obs_b2
    estimations <- decomposition_b2(args, inflos)
    
    L_obs <- apply(obs, 2, sum)
    L_sim <- apply(estimations[[1]], 2, sum)
    # avec inflos CDE
    LperI0_CDE_obs <- apply(obs/inflos, 2, mean, na.rm=T)
    LperI0_CDE_sim <- apply(estimations[[1]]/inflos, 2, mean, na.rm=T)
    LperI_CDE_obs <- L_obs / apply(inflos, 2, sum)
    LperI_CDE_sim <- L_sim / apply(inflos, 2, sum)
    # avec toutes les inflos
    LperI0_obs <- apply(obs/inflos_obs, 2, mean, na.rm=T)
    LperI0_sim <- apply(estimations[[1]]/inflos_obs, 2, mean, na.rm=T)
    LperI_obs <- L_obs / apply(inflos_obs, 2, sum)
    LperI_sim <- L_sim / apply(inflos_obs, 2, sum)
    print(list(rbind(L_obs, LperI0_CDE_obs, LperI_CDE_obs, LperI0_obs, LperI_obs),
    					 rbind(L_sim, LperI0_CDE_sim, LperI_CDE_sim, LperI0_sim, LperI_sim)))
    
    ploter <- data.frame(Date = date2017,
    										 Sol = factor("Low weed cover",
    										 						 levels = c("Low weed cover", 
    										 						 					 "Synthetic mulching", 
    										 						 					 "High weed cover")), 
    										 Observed = obs[, 1],
    										 Estimated = estimations[[1]][, 1],
    										 F_pup = estimations[[2]][, 1],
    										 F_diap = estimations[[3]][, 1],
    										 F_endo = estimations[[4]][, 1],
    										 F_exo = estimations[[5]][, 1]) %>% 
    	gather(F_exo, F_endo, F_diap, F_pup,
    				 key = prov, value = prop, factor_key = TRUE)
    
    plotps <- data.frame(Date = date2017,
    										 Sol = factor("Synthetic mulching",
    										 						 levels = c("Low weed cover", 
    										 						 					 "Synthetic mulching", 
    										 						 					 "High weed cover")), 
    										 Observed = obs[, 2],
    										 Estimated = estimations[[1]][, 2],
    										 F_pup = estimations[[2]][, 2],
    										 F_diap = estimations[[3]][, 2],
    										 F_endo = estimations[[4]][, 2],
    										 F_exo = estimations[[5]][, 2]) %>% 
    	gather(F_exo, F_endo, F_diap, F_pup,
    				 key = prov, value = prop, factor_key = TRUE)
    
    ploteh <- data.frame(Date = date2017,
    										 Sol = factor("High weed cover",
    										 						 levels = c("Low weed cover", 
    										 						 					 "Synthetic mulching", 
    										 						 					 "High weed cover")), 
    										 Observed = obs[, 3],
    										 Estimated = estimations[[1]][, 3],
    										 F_pup = estimations[[2]][, 3],
    										 F_diap = estimations[[3]][, 3],
    										 F_endo = estimations[[4]][, 3],
    										 F_exo = estimations[[5]][, 3]) %>% 
    	gather(F_exo, F_endo, F_diap, F_pup,
    				 key = prov, value = prop, factor_key = TRUE)
    
    to_plot <- bind_rows(ploter, plotps, ploteh)
    to_plot %>% ggplot +
    	aes(x = Date) +
    	geom_area(aes(y = prop, fill = prov), alpha = 0.5) +
    	geom_line(aes(y = Observed, color = "Observed"), lwd = 0.75) +
    	geom_line(aes(y = Estimated, color = "Estimated"), lwd = 0.75) +
    	geom_point(aes(y = Observed, color = "Observed")) +
    	geom_point(aes(y = Estimated, color = "Estimated")) +
    	theme_bw() +
    	facet_grid(. ~ Sol) +
    	theme(legend.title = element_blank(), legend.position = "right") +
    	scale_color_manual(values = c("green4", "black")) +
    	xlab(element_blank()) +
    	theme(axis.text.x = element_text(size=10, angle=45, vjust=1, hjust=1)) +
    	ylab("Number of larvae")
}

plot_decompo_season <- function(args, inflos) {
    obs <- larves_obs
    estimations <- decomposition_season_bloc1(args, inflos)
    
    L_obs <- apply(obs, 2, sum)
    L_sim <- apply(estimations[[1]], 2, sum)
    # avec inflos CDE
    LperI0_CDE_obs <- apply(obs/inflos, 2, mean, na.rm=T)
    LperI0_CDE_sim <- apply(estimations[[1]]/inflos, 2, mean, na.rm=T)
    LperI_CDE_obs <- L_obs / apply(inflos, 2, sum)
    LperI_CDE_sim <- L_sim / apply(inflos, 2, sum)
    # avec toutes les inflos
    LperI0_obs <- apply(obs/inflos_obs, 2, mean, na.rm=T)
    LperI0_sim <- apply(estimations[[1]]/inflos_obs, 2, mean, na.rm=T)
    LperI_obs <- L_obs / apply(inflos_obs, 2, sum)
    LperI_sim <- L_sim / apply(inflos_obs, 2, sum)
    print(list(rbind(L_obs, LperI0_CDE_obs, LperI_CDE_obs, LperI0_obs, LperI_obs),
    					 rbind(L_sim, LperI0_CDE_sim, LperI_CDE_sim, LperI0_sim, LperI_sim)))
    
    ploter <- data.frame(Date = date2017,
    										 Sol = factor("Low weed cover",
    										 						 levels = c("Low weed cover", 
    										 						 					 "Synthetic mulching", 
    										 						 					 "High weed cover")), 
    										 Observed = obs[, 1],
    										 Estimated = estimations[[1]][, 1],
    										 F_pup = estimations[[2]][, 1],
    										 F_diap = estimations[[3]][, 1],
    										 F_endo = estimations[[4]][, 1],
    										 F_exo = estimations[[5]][, 1]) %>% 
    	gather(F_exo, F_endo, F_diap, F_pup,
    				 key = prov, value = prop, factor_key = TRUE)
    
    plotps <- data.frame(Date = date2017,
    										 Sol = factor("Synthetic mulching",
    										 						 levels = c("Low weed cover", 
    										 						 					 "Synthetic mulching", 
    										 						 					 "High weed cover")), 
    										 Observed = obs[, 2],
    										 Estimated = estimations[[1]][, 2],
    										 F_pup = estimations[[2]][, 2],
    										 F_diap = estimations[[3]][, 2],
    										 F_endo = estimations[[4]][, 2],
    										 F_exo = estimations[[5]][, 2]) %>% 
    	gather(F_exo, F_endo, F_diap, F_pup,
    				 key = prov, value = prop, factor_key = TRUE)
    
    ploteh <- data.frame(Date = date2017,
    										 Sol = factor("High weed cover",
    										 						 levels = c("Low weed cover", 
    										 						 					 "Synthetic mulching", 
    										 						 					 "High weed cover")), 
    										 Observed = obs[, 3],
    										 Estimated = estimations[[1]][, 3],
    										 F_pup = estimations[[2]][, 3],
    										 F_diap = estimations[[3]][, 3],
    										 F_endo = estimations[[4]][, 3],
    										 F_exo = estimations[[5]][, 3]) %>% 
    	gather(F_exo, F_endo, F_diap, F_pup,
    				 key = prov, value = prop, factor_key = TRUE)
    
    to_plot <- bind_rows(ploter, plotps, ploteh)
    to_plot %>% ggplot +
    	aes(x = Date) +
    	geom_area(aes(y = prop, fill = prov), alpha = 0.5) +
    	geom_line(aes(y = Observed, color = "Observed"), lwd = 0.75) +
    	geom_line(aes(y = Estimated, color = "Estimated"), lwd = 0.75) +
    	geom_point(aes(y = Observed, color = "Observed")) +
    	geom_point(aes(y = Estimated, color = "Estimated")) +
    	theme_bw() +
    	facet_grid(. ~ Sol) +
    	theme(legend.title = element_blank(), legend.position = "right") +
    	scale_color_manual(values = c("green4", "black")) +
    	xlab(element_blank()) +
    	theme(axis.text.x = element_text(size=10, angle=45, vjust=1, hjust=1)) +
    	ylab("Number of larvae")
}

plot_decompo_season_b2 <- function(args, inflos) {
    obs <- larves_obs_b2
    estimations <- decomposition_season_bloc2(args, inflos)
    
    L_obs <- apply(obs, 2, sum)
    L_sim <- apply(estimations[[1]], 2, sum)
    # avec inflos CDE
    LperI0_CDE_obs <- apply(obs/inflos, 2, mean, na.rm=T)
    LperI0_CDE_sim <- apply(estimations[[1]]/inflos, 2, mean, na.rm=T)
    LperI_CDE_obs <- L_obs / apply(inflos, 2, sum)
    LperI_CDE_sim <- L_sim / apply(inflos, 2, sum)
    # avec toutes les inflos
    LperI0_obs <- apply(obs/inflos_obs, 2, mean, na.rm=T)
    LperI0_sim <- apply(estimations[[1]]/inflos_obs, 2, mean, na.rm=T)
    LperI_obs <- L_obs / apply(inflos_obs, 2, sum)
    LperI_sim <- L_sim / apply(inflos_obs, 2, sum)
    print(list(rbind(L_obs, LperI0_CDE_obs, LperI_CDE_obs, LperI0_obs, LperI_obs),
    					 rbind(L_sim, LperI0_CDE_sim, LperI_CDE_sim, LperI0_sim, LperI_sim)))
    
    ploter <- data.frame(Date = date2017,
    										 Sol = factor("Low weed cover",
    										 						 levels = c("Low weed cover", 
    										 						 					 "Synthetic mulching", 
    										 						 					 "High weed cover")), 
    										 Observed = obs[, 1],
    										 Estimated = estimations[[1]][, 1],
    										 F_pup = estimations[[2]][, 1],
    										 F_diap = estimations[[3]][, 1],
    										 F_endo = estimations[[4]][, 1],
    										 F_exo = estimations[[5]][, 1]) %>% 
    	gather(F_exo, F_endo, F_diap, F_pup,
    				 key = prov, value = prop, factor_key = TRUE)
    
    plotps <- data.frame(Date = date2017,
    										 Sol = factor("Synthetic mulching",
    										 						 levels = c("Low weed cover", 
    										 						 					 "Synthetic mulching", 
    										 						 					 "High weed cover")), 
    										 Observed = obs[, 2],
    										 Estimated = estimations[[1]][, 2],
    										 F_pup = estimations[[2]][, 2],
    										 F_diap = estimations[[3]][, 2],
    										 F_endo = estimations[[4]][, 2],
    										 F_exo = estimations[[5]][, 2]) %>% 
    	gather(F_exo, F_endo, F_diap, F_pup,
    				 key = prov, value = prop, factor_key = TRUE)
    
    ploteh <- data.frame(Date = date2017,
    										 Sol = factor("High weed cover",
    										 						 levels = c("Low weed cover", 
    										 						 					 "Synthetic mulching", 
    										 						 					 "High weed cover")), 
    										 Observed = obs[, 3],
    										 Estimated = estimations[[1]][, 3],
    										 F_pup = estimations[[2]][, 3],
    										 F_diap = estimations[[3]][, 3],
    										 F_endo = estimations[[4]][, 3],
    										 F_exo = estimations[[5]][, 3]) %>% 
    	gather(F_exo, F_endo, F_diap, F_pup,
    				 key = prov, value = prop, factor_key = TRUE)
    
    to_plot <- bind_rows(ploter, plotps, ploteh)
    to_plot %>% ggplot +
    	aes(x = Date) +
    	geom_area(aes(y = prop, fill = prov), alpha = 0.5) +
    	geom_line(aes(y = Observed, color = "Observed"), lwd = 0.75) +
    	geom_line(aes(y = Estimated, color = "Estimated"), lwd = 0.75) +
    	geom_point(aes(y = Observed, color = "Observed")) +
    	geom_point(aes(y = Estimated, color = "Estimated")) +
    	theme_bw() +
    	facet_grid(. ~ Sol) +
    	theme(legend.title = element_blank(), legend.position = "right") +
    	scale_color_manual(values = c("green4", "black")) +
    	xlab(element_blank()) +
    	theme(axis.text.x = element_text(size=10, angle=45, vjust=1, hjust=1)) +
    	ylab("Number of larvae")
}

plot_decompo_15sept_b2 <- function(args, inflos) {
    obs <- larves_obs_b2
    estimations <- decomposition_15sept_bloc2(args, inflos)
    
    L_obs <- apply(obs, 2, sum)
    L_sim <- apply(estimations[[1]], 2, sum)
    # avec inflos CDE
    LperI0_CDE_obs <- apply(obs/inflos, 2, mean, na.rm=T)
    LperI0_CDE_sim <- apply(estimations[[1]]/inflos, 2, mean, na.rm=T)
    LperI_CDE_obs <- L_obs / apply(inflos, 2, sum)
    LperI_CDE_sim <- L_sim / apply(inflos, 2, sum)
    # avec toutes les inflos
    LperI0_obs <- apply(obs/inflos_obs, 2, mean, na.rm=T)
    LperI0_sim <- apply(estimations[[1]]/inflos_obs, 2, mean, na.rm=T)
    LperI_obs <- L_obs / apply(inflos_obs, 2, sum)
    LperI_sim <- L_sim / apply(inflos_obs, 2, sum)
    print(list(rbind(L_obs, LperI0_CDE_obs, LperI_CDE_obs, LperI0_obs, LperI_obs),
    					 rbind(L_sim, LperI0_CDE_sim, LperI_CDE_sim, LperI0_sim, LperI_sim)))
    
    ploter <- data.frame(Date = date2017,
    										 Sol = factor("Low weed cover",
    										 						 levels = c("Low weed cover", 
    										 						 					 "Synthetic mulching", 
    										 						 					 "High weed cover")), 
    										 Observed = obs[, 1],
    										 Estimated = estimations[[1]][, 1],
    										 F_pup = estimations[[2]][, 1],
    										 F_diap = estimations[[3]][, 1],
    										 F_endo = estimations[[4]][, 1],
    										 F_exo = estimations[[5]][, 1]) %>% 
    	gather(F_exo, F_endo, F_diap, F_pup,
    				 key = prov, value = prop, factor_key = TRUE)
    
    plotps <- data.frame(Date = date2017,
    										 Sol = factor("Synthetic mulching",
    										 						 levels = c("Low weed cover", 
    										 						 					 "Synthetic mulching", 
    										 						 					 "High weed cover")), 
    										 Observed = obs[, 2],
    										 Estimated = estimations[[1]][, 2],
    										 F_pup = estimations[[2]][, 2],
    										 F_diap = estimations[[3]][, 2],
    										 F_endo = estimations[[4]][, 2],
    										 F_exo = estimations[[5]][, 2]) %>% 
    	gather(F_exo, F_endo, F_diap, F_pup,
    				 key = prov, value = prop, factor_key = TRUE)
    
    ploteh <- data.frame(Date = date2017,
    										 Sol = factor("High weed cover",
    										 						 levels = c("Low weed cover", 
    										 						 					 "Synthetic mulching", 
    										 						 					 "High weed cover")), 
    										 Observed = obs[, 3],
    										 Estimated = estimations[[1]][, 3],
    										 F_pup = estimations[[2]][, 3],
    										 F_diap = estimations[[3]][, 3],
    										 F_endo = estimations[[4]][, 3],
    										 F_exo = estimations[[5]][, 3]) %>% 
    	gather(F_exo, F_endo, F_diap, F_pup,
    				 key = prov, value = prop, factor_key = TRUE)
    
    to_plot <- bind_rows(ploter, plotps, ploteh)
    to_plot %>% ggplot +
    	aes(x = Date) +
    	geom_area(aes(y = prop, fill = prov), alpha = 0.5) +
    	geom_line(aes(y = Observed, color = "Observed"), lwd = 0.75) +
    	geom_line(aes(y = Estimated, color = "Estimated"), lwd = 0.75) +
    	geom_point(aes(y = Observed, color = "Observed")) +
    	geom_point(aes(y = Estimated, color = "Estimated")) +
    	theme_bw() +
    	facet_grid(. ~ Sol) +
    	theme(legend.title = element_blank(), legend.position = "right") +
    	scale_color_manual(values = c("green4", "black")) +
    	xlab(element_blank()) +
    	theme(axis.text.x = element_text(size=10, angle=45, vjust=1, hjust=1)) +
    	ylab("Number of larvae")
}

plot_decompo_season_inflos <- function(args, inflos) {
    obs <- larves_obs
    estimations <- decomposition_season_inflos_bloc1(args, inflos)
    
    L_obs <- apply(obs, 2, sum)
    L_sim <- apply(estimations[[1]], 2, sum)
    # avec inflos CDE
    LperI0_CDE_obs <- apply(obs/inflos, 2, mean, na.rm=T)
    LperI0_CDE_sim <- apply(estimations[[1]]/inflos, 2, mean, na.rm=T)
    LperI_CDE_obs <- L_obs / apply(inflos, 2, sum)
    LperI_CDE_sim <- L_sim / apply(inflos, 2, sum)
    # avec toutes les inflos
    LperI0_obs <- apply(obs/inflos_obs, 2, mean, na.rm=T)
    LperI0_sim <- apply(estimations[[1]]/inflos_obs, 2, mean, na.rm=T)
    LperI_obs <- L_obs / apply(inflos_obs, 2, sum)
    LperI_sim <- L_sim / apply(inflos_obs, 2, sum)
    print(list(rbind(L_obs, LperI0_CDE_obs, LperI_CDE_obs, LperI0_obs, LperI_obs),
    					 rbind(L_sim, LperI0_CDE_sim, LperI_CDE_sim, LperI0_sim, LperI_sim)))
    
    ploter <- data.frame(Date = date2017,
    										 Sol = factor("Low weed cover",
    										 						 levels = c("Low weed cover", 
    										 						 					 "Synthetic mulching", 
    										 						 					 "High weed cover")), 
    										 Observed = obs[, 1],
    										 Estimated = estimations[[1]][, 1],
    										 F_pup = estimations[[2]][, 1],
    										 F_diap = estimations[[3]][, 1],
    										 F_endo = estimations[[4]][, 1],
    										 F_exo = estimations[[5]][, 1]) %>% 
    	gather(F_exo, F_endo, F_diap, F_pup,
    				 key = prov, value = prop, factor_key = TRUE)
    
    plotps <- data.frame(Date = date2017,
    										 Sol = factor("Synthetic mulching",
    										 						 levels = c("Low weed cover", 
    										 						 					 "Synthetic mulching", 
    										 						 					 "High weed cover")), 
    										 Observed = obs[, 2],
    										 Estimated = estimations[[1]][, 2],
    										 F_pup = estimations[[2]][, 2],
    										 F_diap = estimations[[3]][, 2],
    										 F_endo = estimations[[4]][, 2],
    										 F_exo = estimations[[5]][, 2]) %>% 
    	gather(F_exo, F_endo, F_diap, F_pup,
    				 key = prov, value = prop, factor_key = TRUE)
    
    ploteh <- data.frame(Date = date2017,
    										 Sol = factor("High weed cover",
    										 						 levels = c("Low weed cover", 
    										 						 					 "Synthetic mulching", 
    										 						 					 "High weed cover")), 
    										 Observed = obs[, 3],
    										 Estimated = estimations[[1]][, 3],
    										 F_pup = estimations[[2]][, 3],
    										 F_diap = estimations[[3]][, 3],
    										 F_endo = estimations[[4]][, 3],
    										 F_exo = estimations[[5]][, 3]) %>% 
    	gather(F_exo, F_endo, F_diap, F_pup,
    				 key = prov, value = prop, factor_key = TRUE)
    
    to_plot <- bind_rows(ploter, plotps, ploteh)
    to_plot %>% ggplot +
    	aes(x = Date) +
    	geom_area(aes(y = prop, fill = prov), alpha = 0.5) +
    	geom_line(aes(y = Observed, color = "Observed"), lwd = 0.75) +
    	geom_line(aes(y = Estimated, color = "Estimated"), lwd = 0.75) +
    	geom_point(aes(y = Observed, color = "Observed")) +
    	geom_point(aes(y = Estimated, color = "Estimated")) +
    	theme_bw() +
    	facet_grid(. ~ Sol) +
    	theme(legend.title = element_blank(), legend.position = "right") +
    	scale_color_manual(values = c("green4", "black")) +
    	xlab(element_blank()) +
    	theme(axis.text.x = element_text(size=10, angle=45, vjust=1, hjust=1)) +
    	ylab("Number of larvae")
}

plot_decompo_season_inflos_b2 <- function(args, inflos) {
    obs <- larves_obs_b2
    estimations <- decomposition_season_inflos_bloc2(args, inflos)
    
    L_obs <- apply(obs, 2, sum)
    L_sim <- apply(estimations[[1]], 2, sum)
    # avec inflos CDE
    LperI0_CDE_obs <- apply(obs/inflos, 2, mean, na.rm=T)
    LperI0_CDE_sim <- apply(estimations[[1]]/inflos, 2, mean, na.rm=T)
    LperI_CDE_obs <- L_obs / apply(inflos, 2, sum)
    LperI_CDE_sim <- L_sim / apply(inflos, 2, sum)
    # avec toutes les inflos
    LperI0_obs <- apply(obs/inflos_obs, 2, mean, na.rm=T)
    LperI0_sim <- apply(estimations[[1]]/inflos_obs, 2, mean, na.rm=T)
    LperI_obs <- L_obs / apply(inflos_obs, 2, sum)
    LperI_sim <- L_sim / apply(inflos_obs, 2, sum)
    print(list(rbind(L_obs, LperI0_CDE_obs, LperI_CDE_obs, LperI0_obs, LperI_obs),
    					 rbind(L_sim, LperI0_CDE_sim, LperI_CDE_sim, LperI0_sim, LperI_sim)))
    
    ploter <- data.frame(Date = date2017,
    										 Sol = factor("Low weed cover",
    										 						 levels = c("Low weed cover", 
    										 						 					 "Synthetic mulching", 
    										 						 					 "High weed cover")), 
    										 Observed = obs[, 1],
    										 Estimated = estimations[[1]][, 1],
    										 F_pup = estimations[[2]][, 1],
    										 F_diap = estimations[[3]][, 1],
    										 F_endo = estimations[[4]][, 1],
    										 F_exo = estimations[[5]][, 1]) %>% 
    	gather(F_exo, F_endo, F_diap, F_pup,
    				 key = prov, value = prop, factor_key = TRUE)
    
    plotps <- data.frame(Date = date2017,
    										 Sol = factor("Synthetic mulching",
    										 						 levels = c("Low weed cover", 
    										 						 					 "Synthetic mulching", 
    										 						 					 "High weed cover")), 
    										 Observed = obs[, 2],
    										 Estimated = estimations[[1]][, 2],
    										 F_pup = estimations[[2]][, 2],
    										 F_diap = estimations[[3]][, 2],
    										 F_endo = estimations[[4]][, 2],
    										 F_exo = estimations[[5]][, 2]) %>% 
    	gather(F_exo, F_endo, F_diap, F_pup,
    				 key = prov, value = prop, factor_key = TRUE)
    
    ploteh <- data.frame(Date = date2017,
    										 Sol = factor("High weed cover",
    										 						 levels = c("Low weed cover", 
    										 						 					 "Synthetic mulching", 
    										 						 					 "High weed cover")), 
    										 Observed = obs[, 3],
    										 Estimated = estimations[[1]][, 3],
    										 F_pup = estimations[[2]][, 3],
    										 F_diap = estimations[[3]][, 3],
    										 F_endo = estimations[[4]][, 3],
    										 F_exo = estimations[[5]][, 3]) %>% 
    	gather(F_exo, F_endo, F_diap, F_pup,
    				 key = prov, value = prop, factor_key = TRUE)
    
    to_plot <- bind_rows(ploter, plotps, ploteh)
    to_plot %>% ggplot +
    	aes(x = Date) +
    	geom_area(aes(y = prop, fill = prov), alpha = 0.5) +
    	geom_line(aes(y = Observed, color = "Observed"), lwd = 0.75) +
    	geom_line(aes(y = Estimated, color = "Estimated"), lwd = 0.75) +
    	geom_point(aes(y = Observed, color = "Observed")) +
    	geom_point(aes(y = Estimated, color = "Estimated")) +
    	theme_bw() +
    	facet_grid(. ~ Sol) +
    	theme(legend.title = element_blank(), legend.position = "right") +
    	scale_color_manual(values = c("green4", "black")) +
    	xlab(element_blank()) +
    	theme(axis.text.x = element_text(size=10, angle=45, vjust=1, hjust=1)) +
    	ylab("Number of larvae")
}

### fonctions pour les simulations / ISEM2019

plot_inflos <- function(inflos, inflos_CDE) {
	est1 <- inflos
	est2 <- inflos_CDE
	er <- data.frame(date = date2017, est1 = est1[, 1], est2 = est2[, 1]) %>%
		mutate(Sol = factor("Low weed cover",
												levels = c("Low weed cover", 
																	 "Synthetic mulching", 
																	 "High weed cover"))) %>% 
		gather(est1, est2, key = statut, value = nombre, factor_key = TRUE)
	
	ps <- data.frame(date = date2017, est1 = est1[, 2], est2 = est2[, 2]) %>%
		mutate(Sol = factor("Synthetic mulching", 
												levels = c("Low weed cover", 
																	 "Synthetic mulching", 
																	 "High weed cover"))) %>% 
		gather(est1, est2, key = statut, value = nombre, factor_key = TRUE)
	
	eh <- data.frame(date = date2017, est1 = est1[, 3], est2 = est2[, 3]) %>%
		mutate(Sol = factor("High weed cover",
												levels = c("Low weed cover", 
																	 "Synthetic mulching", 
																	 "High weed cover"))) %>% 
		gather(est1, est2, key = statut, value = nombre, factor_key = TRUE)
	
	to_plot <- bind_rows(er, ps, eh)
	to_plot %>% ggplot +
		aes(x = date, y = nombre, color = statut) +
		#geom_point() +
		geom_line(lwd=1) +
		facet_grid(. ~ Sol) +
		theme_bw() +
		theme(legend.title = element_blank(), legend.position = "right") +
		scale_color_manual(values = c("blue", "red"), labels = c("Simulation_1", "Simulation_2")) +
		xlab(element_blank()) +
		theme(axis.text.x = element_text(size=10, angle=45, vjust=1, hjust=1)) +
		ylab("Number of inflorescences")
}


plot_sim_fenv_A <- function(args1, args2, inflos) {
	est1 <- dynamics_A(args1, inflos)
	est2 <- dynamics_A(args2, inflos)
	
	larves_sim1 <- apply(est1, 2, sum)
	larves_sim2 <- apply(est2, 2, sum)
	larvesperinf0_sim1 <- apply(est1/inflos, 2, mean, na.rm=T)
	larvesperinf0_sim2 <- apply(est2/inflos, 2, mean, na.rm=T)
	larvesperinf_sim1 <- larves_sim1 / apply(inflos, 2, sum)
	larvesperinf_sim2 <- larves_sim2 / apply(inflos, 2, sum)
	print(list(rbind(larves_sim1, larvesperinf0_sim1, larvesperinf_sim1),
						 rbind(larves_sim2, larvesperinf0_sim2, larvesperinf_sim2),
						 100*(larves_sim2-larves_sim1)/larves_sim2))
	
	er <- data.frame(date = date2017, est1 = est1[, 1], est2 = est2[, 1]) %>%
		mutate(Sol = factor("Low weed cover",
												levels = c("Low weed cover", 
																	 "Synthetic mulching", 
																	 "High weed cover"))) %>% 
		gather(est1, est2, key = statut, value = nombre, factor_key = TRUE)
	
	ps <- data.frame(date = date2017, est1 = est1[, 2], est2 = est2[, 2]) %>%
		mutate(Sol = factor("Synthetic mulching", 
												levels = c("Low weed cover", 
																	 "Synthetic mulching", 
																	 "High weed cover"))) %>% 
		gather(est1, est2, key = statut, value = nombre, factor_key = TRUE)
	
	eh <- data.frame(date = date2017, est1 = est1[, 3], est2 = est2[, 3]) %>%
		mutate(Sol = factor("High weed cover",
												levels = c("Low weed cover", 
																	 "Synthetic mulching", 
																	 "High weed cover"))) %>% 
		gather(est1, est2, key = statut, value = nombre, factor_key = TRUE)
	
	to_plot <- bind_rows(er, ps, eh)
	to_plot %>% ggplot +
		aes(x = date, y = nombre, color = statut) +
		#geom_point() +
		geom_line(lwd=1) +
		facet_grid(. ~ Sol) +
		theme_bw() +
		theme(legend.title = element_blank(), legend.position = "right") +
		scale_color_manual(values = c("black", "red"), labels = c("Simulation_1", "Simulation_2")) +
		xlab(element_blank()) +
		theme(axis.text.x = element_text(size=10, angle=45, vjust=1, hjust=1)) +
		ylab("Number of larvae")
}

plot_sim_fenv_C <- function(args1, args2, inflos) {
	est1 <- dynamics(args1, inflos)
	est2 <- dynamics(args2, inflos)
	
	larves_sim1 <- apply(est1, 2, sum)
	larves_sim2 <- apply(est2, 2, sum)
	larvesperinf0_sim1 <- apply(est1/inflos, 2, mean, na.rm=T)
	larvesperinf0_sim2 <- apply(est2/inflos, 2, mean, na.rm=T)
	larvesperinf_sim1 <- larves_sim1 / apply(inflos, 2, sum)
	larvesperinf_sim2 <- larves_sim2 / apply(inflos, 2, sum)
	print(list(rbind(larves_sim1, larvesperinf0_sim1, larvesperinf_sim1),
						 rbind(larves_sim2, larvesperinf0_sim2, larvesperinf_sim2),
						 100*(larves_sim2-larves_sim1)/larves_sim2))
	
	er <- data.frame(date = date2017, est1 = est1[, 1], est2 = est2[, 1]) %>%
		mutate(Sol = factor("Low weed cover",
												levels = c("Low weed cover", 
																	 "Synthetic mulching", 
																	 "High weed cover"))) %>% 
		gather(est1, est2, key = statut, value = nombre, factor_key = TRUE)
	
	ps <- data.frame(date = date2017, est1 = est1[, 2], est2 = est2[, 2]) %>%
		mutate(Sol = factor("Synthetic mulching", 
												levels = c("Low weed cover", 
																	 "Synthetic mulching", 
																	 "High weed cover"))) %>% 
		gather(est1, est2, key = statut, value = nombre, factor_key = TRUE)
	
	eh <- data.frame(date = date2017, est1 = est1[, 3], est2 = est2[, 3]) %>%
		mutate(Sol = factor("High weed cover",
												levels = c("Low weed cover", 
																	 "Synthetic mulching", 
																	 "High weed cover"))) %>% 
		gather(est1, est2, key = statut, value = nombre, factor_key = TRUE)
	
	to_plot <- bind_rows(er, ps, eh)
	to_plot %>% ggplot +
		aes(x = date, y = nombre, color = statut) +
		#geom_point() +
		geom_line(lwd=1) +
		facet_grid(. ~ Sol) +
		theme_bw() +
		theme(legend.title = element_blank(), legend.position = "right") +
		scale_color_manual(values = c("black", "red"), labels = c("Simulation_1", "Simulation_2")) +
		xlab(element_blank()) +
		theme(axis.text.x = element_text(size=10, angle=45, vjust=1, hjust=1)) +
		ylab("Number of larvae")
}

plot_sim_fenv_D <- function(args1, args2, inflos) {
	est1 <- dynamics_season_b1(args1, inflos)
	est2 <- dynamics_season_b1(args2, inflos)
	
	larves_sim1 <- apply(est1, 2, sum)
	larves_sim2 <- apply(est2, 2, sum)
	larvesperinf0_sim1 <- apply(est1/inflos, 2, mean, na.rm=T)
	larvesperinf0_sim2 <- apply(est2/inflos, 2, mean, na.rm=T)
	larvesperinf_sim1 <- larves_sim1 / apply(inflos, 2, sum)
	larvesperinf_sim2 <- larves_sim2 / apply(inflos, 2, sum)
	print(list(rbind(larves_sim1, larvesperinf0_sim1, larvesperinf_sim1),
						 rbind(larves_sim2, larvesperinf0_sim2, larvesperinf_sim2),
						 100*(larves_sim2-larves_sim1)/larves_sim2))
	
	er <- data.frame(date = date2017, est1 = est1[, 1], est2 = est2[, 1]) %>%
		mutate(Sol = factor("Low weed cover",
												levels = c("Low weed cover", 
																	 "Synthetic mulching", 
																	 "High weed cover"))) %>% 
		gather(est1, est2, key = statut, value = nombre, factor_key = TRUE)
	
	ps <- data.frame(date = date2017, est1 = est1[, 2], est2 = est2[, 2]) %>%
		mutate(Sol = factor("Synthetic mulching", 
												levels = c("Low weed cover", 
																	 "Synthetic mulching", 
																	 "High weed cover"))) %>% 
		gather(est1, est2, key = statut, value = nombre, factor_key = TRUE)
	
	eh <- data.frame(date = date2017, est1 = est1[, 3], est2 = est2[, 3]) %>%
		mutate(Sol = factor("High weed cover",
												levels = c("Low weed cover", 
																	 "Synthetic mulching", 
																	 "High weed cover"))) %>% 
		gather(est1, est2, key = statut, value = nombre, factor_key = TRUE)
	
	to_plot <- bind_rows(er, ps, eh)
	to_plot %>% ggplot +
		aes(x = date, y = nombre, color = statut) +
		#geom_point() +
		geom_line(lwd=1) +
		facet_grid(. ~ Sol) +
		theme_bw() +
		theme(legend.title = element_blank(), legend.position = "right") +
		scale_color_manual(values = c("black", "red"), labels = c("Simulation_1", "Simulation_2")) +
		xlab(element_blank()) +
		theme(axis.text.x = element_text(size=10, angle=45, vjust=1, hjust=1)) +
		ylab("Number of larvae")
}


plot_sim_inflos_A <- function(args1, inflos1, inflos2) {
	est1 <- dynamics_A(args1, inflos1)
	est2 <- dynamics_A(args1, inflos2)
	
	larves_sim1 <- apply(est1, 2, sum)
	larves_sim2 <- apply(est2, 2, sum)
	larvesperinf0_sim1 <- apply(est1/inflos1, 2, mean, na.rm=T)
	larvesperinf0_sim2 <- apply(est2/inflos2, 2, mean, na.rm=T)
	larvesperinf_sim1 <- larves_sim1 / apply(inflos1, 2, sum)
	larvesperinf_sim2 <- larves_sim2 / apply(inflos2, 2, sum)
	print(list(rbind(larves_sim1, larvesperinf0_sim1, larvesperinf_sim1),
						 rbind(larves_sim2, larvesperinf0_sim2, larvesperinf_sim2),
						 100*(larves_sim2-larves_sim1)/larves_sim2))
	
	er <- data.frame(date = date2017, est1 = est1[, 1], est2 = est2[, 1]) %>%
		mutate(Sol = factor("Low weed cover",
												levels = c("Low weed cover", 
																	 "Synthetic mulching", 
																	 "High weed cover"))) %>% 
		gather(est1, est2, key = statut, value = nombre, factor_key = TRUE)
	
	ps <- data.frame(date = date2017, est1 = est1[, 2], est2 = est2[, 2]) %>%
		mutate(Sol = factor("Synthetic mulching", 
												levels = c("Low weed cover", 
																	 "Synthetic mulching", 
																	 "High weed cover"))) %>% 
		gather(est1, est2, key = statut, value = nombre, factor_key = TRUE)
	
	eh <- data.frame(date = date2017, est1 = est1[, 3], est2 = est2[, 3]) %>%
		mutate(Sol = factor("High weed cover",
												levels = c("Low weed cover", 
																	 "Synthetic mulching", 
																	 "High weed cover"))) %>% 
		gather(est1, est2, key = statut, value = nombre, factor_key = TRUE)
	
	to_plot <- bind_rows(er, ps, eh)
	to_plot %>% ggplot +
		aes(x = date, y = nombre, color = statut) +
		#geom_point() +
		geom_line(lwd=1) +
		facet_grid(. ~ Sol) +
		theme_bw() +
		theme(legend.title = element_blank(), legend.position = "right") +
		scale_color_manual(values = c("black", "red"), labels = c("Simulation_1", "Simulation_2")) +
		xlab(element_blank()) +
		theme(axis.text.x = element_text(size=10, angle=45, vjust=1, hjust=1)) +
		ylab("Number of larvae")
}

plot_sim_inflos_C <- function(args1, inflos1, inflos2) {
	est1 <- dynamics(args1, inflos1)
	est2 <- dynamics(args1, inflos2)
	
	larves_sim1 <- apply(est1, 2, sum)
	larves_sim2 <- apply(est2, 2, sum)
	larvesperinf0_sim1 <- apply(est1/inflos1, 2, mean, na.rm=T)
	larvesperinf0_sim2 <- apply(est2/inflos2, 2, mean, na.rm=T)
	larvesperinf_sim1 <- larves_sim1 / apply(inflos1, 2, sum)
	larvesperinf_sim2 <- larves_sim2 / apply(inflos2, 2, sum)
	print(list(rbind(larves_sim1, larvesperinf0_sim1, larvesperinf_sim1),
						 rbind(larves_sim2, larvesperinf0_sim2, larvesperinf_sim2),
						 100*(larves_sim2-larves_sim1)/larves_sim2))
	
	er <- data.frame(date = date2017, est1 = est1[, 1], est2 = est2[, 1]) %>%
		mutate(Sol = factor("Low weed cover",
												levels = c("Low weed cover", 
																	 "Synthetic mulching", 
																	 "High weed cover"))) %>% 
		gather(est1, est2, key = statut, value = nombre, factor_key = TRUE)
	
	ps <- data.frame(date = date2017, est1 = est1[, 2], est2 = est2[, 2]) %>%
		mutate(Sol = factor("Synthetic mulching", 
												levels = c("Low weed cover", 
																	 "Synthetic mulching", 
																	 "High weed cover"))) %>% 
		gather(est1, est2, key = statut, value = nombre, factor_key = TRUE)
	
	eh <- data.frame(date = date2017, est1 = est1[, 3], est2 = est2[, 3]) %>%
		mutate(Sol = factor("High weed cover",
												levels = c("Low weed cover", 
																	 "Synthetic mulching", 
																	 "High weed cover"))) %>% 
		gather(est1, est2, key = statut, value = nombre, factor_key = TRUE)
	
	to_plot <- bind_rows(er, ps, eh)
	to_plot %>% ggplot +
		aes(x = date, y = nombre, color = statut) +
		#geom_point() +
		geom_line(lwd=1) +
		facet_grid(. ~ Sol) +
		theme_bw() +
		theme(legend.title = element_blank(), legend.position = "right") +
		scale_color_manual(values = c("black", "red"), labels = c("Simulation_1", "Simulation_2")) +
		xlab(element_blank()) +
		theme(axis.text.x = element_text(size=10, angle=45, vjust=1, hjust=1)) +
		ylab("Number of larvae")
}

plot_sim_inflos_D <- function(args1, inflos1, inflos2) {
	est1 <- dynamics_season_b1(args1, inflos1)
	est2 <- dynamics_season_b1(args1, inflos2)
	
	larves_sim1 <- apply(est1, 2, sum)
	larves_sim2 <- apply(est2, 2, sum)
	larvesperinf0_sim1 <- apply(est1/inflos1, 2, mean, na.rm=T)
	larvesperinf0_sim2 <- apply(est2/inflos2, 2, mean, na.rm=T)
	larvesperinf_sim1 <- larves_sim1 / apply(inflos1, 2, sum)
	larvesperinf_sim2 <- larves_sim2 / apply(inflos2, 2, sum)
	print(list(rbind(larves_sim1, larvesperinf0_sim1, larvesperinf_sim1),
						 rbind(larves_sim2, larvesperinf0_sim2, larvesperinf_sim2),
						 100*(larves_sim2-larves_sim1)/larves_sim2))
	
	er <- data.frame(date = date2017, est1 = est1[, 1], est2 = est2[, 1]) %>%
		mutate(Sol = factor("Low weed cover",
												levels = c("Low weed cover", 
																	 "Synthetic mulching", 
																	 "High weed cover"))) %>% 
		gather(est1, est2, key = statut, value = nombre, factor_key = TRUE)
	
	ps <- data.frame(date = date2017, est1 = est1[, 2], est2 = est2[, 2]) %>%
		mutate(Sol = factor("Synthetic mulching", 
												levels = c("Low weed cover", 
																	 "Synthetic mulching", 
																	 "High weed cover"))) %>% 
		gather(est1, est2, key = statut, value = nombre, factor_key = TRUE)
	
	eh <- data.frame(date = date2017, est1 = est1[, 3], est2 = est2[, 3]) %>%
		mutate(Sol = factor("High weed cover",
												levels = c("Low weed cover", 
																	 "Synthetic mulching", 
																	 "High weed cover"))) %>% 
		gather(est1, est2, key = statut, value = nombre, factor_key = TRUE)
	
	to_plot <- bind_rows(er, ps, eh)
	to_plot %>% ggplot +
		aes(x = date, y = nombre, color = statut) +
		#geom_point() +
		geom_line(lwd=1) +
		facet_grid(. ~ Sol) +
		theme_bw() +
		theme(legend.title = element_blank(), legend.position = "right") +
		scale_color_manual(values = c("black", "red"), labels = c("Simulation_1", "Simulation_2")) +
		xlab(element_blank()) +
		theme(axis.text.x = element_text(size=10, angle=45, vjust=1, hjust=1)) +
		ylab("Number of larvae")
}


