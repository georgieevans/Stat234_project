setwd('/Users/georgieevans/Google Drive/rl_simulations')
load("sim_output/thompson_sims.Rdata")
load("sim_output/exploration_sims.Rdata")
load("sim_output/random_sims.Rdata")

## Plot proportion of time identify the best 
chosen_policy <- matrix(NA, 4, 4)

for(i in 1:4) {
  
  chosen_policy[i, ] <- as.numeric(table(factor(sapply(1:250, function(s) thompson_sims[[i]][[s]]$chosen_group), levels = 1:4)))
         
}

chosen_policy_e <- matrix(NA, 4, 4)

for(i in 1:4) {
  
  chosen_policy_e[i, ] <- as.numeric(table(factor(sapply(1:250, function(s) exploration_sims[[i]][[s]]$chosen_group), levels = 1:4)))
  
}


chosen_policy_r <- matrix(NA, 4, 4)

for(i in 1:4) {
  
  chosen_policy_r[i, ] <- as.numeric(table(factor(sapply(1:250, function(s) random_sims[[i]][[s]]$chosen_group), levels = 1:4)))
  
}




proportion_plot <- ggplot() +
  geom_line(aes(x = c(1000, 2500, 5000, 10000), y = chosen_policy_e[,4]/250, col = 'Exploration sampling')) + 
  geom_point(aes(x = c(1000, 2500, 5000, 10000), y = chosen_policy_e[,4]/250, col = 'Exploration sampling')) + 
  geom_line(aes(x = c(1000, 2500, 5000, 10000), y = chosen_policy[,4]/250, col = 'Thompson sampling')) + 
  geom_point(aes(x = c(1000, 2500, 5000, 10000), y = chosen_policy[,4]/250, col = 'Thompson sampling')) +
  geom_line(aes(x = c(1000, 2500, 5000, 10000), y = chosen_policy_r[,4]/250, col = 'Random sampling')) +
  geom_point(aes(x = c(1000, 2500, 5000, 10000), y = chosen_policy_r[,4]/250, col = 'Random sampling')) +
    theme_bw() + 
  theme(legend.position = c(0.9, 0.1), 
        legend.justification = c(0.9, 0.1), 
        panel.grid.major = element_blank(), 
        legend.box.background = element_rect(colour = "black")) +
  labs(x = 'Experiment size (N)', y = 'Proportion optimal group choice', col = 'Sampling scheme') 
  
ggsave(proportion_plot, file = 'propotion_plot.pdf', width = 5, height = 4) 
  

# Tables
random <- data.frame(chosen_policy_r/250)
colnames(random) <- c('1', '2', '3', '4')

thompson <- data.frame(chosen_policy/250)
colnames(thompson) <- c('1', '2', '3', '4')

exploration <- data.frame(chosen_policy_e/250)
colnames(exploration) <- c('1', '2', '3', '4')



thompson_sampling <- rbind(data.frame(n = 1000, allocated = unlist(lapply(1:250, function(s) thompson_sims[[1]][[s]]$treatment_assignment))), 
      data.frame(n = 2500, allocated = unlist(lapply(1:250, function(s) thompson_sims[[2]][[s]]$treatment_assignment))), 
      data.frame(n = 5000, allocated = unlist(lapply(1:250, function(s) thompson_sims[[3]][[s]]$treatment_assignment))),
      data.frame(n = 10000, allocated = unlist(lapply(1:250, function(s) thompson_sims[[4]][[s]]$treatment_assignment))))



thompson_allocation <- ggplot(thompson_sampling, aes(x = as.factor(n), fill = as.factor(allocated))) +
  geom_bar(position = 'fill') + scale_y_continuous(labels = scales::percent) + 
  labs(x = 'Experiment size (N)', y = 'Proportion allocated', fill = 'Group', title = 'Thompson Sampling') + 
  theme_bw()

rm(thompson_sampling)

exploration_sampling <- rbind(data.frame(n = 1000, allocated = rep(1:4, apply(do.call(rbind, lapply(1:250, function(s) exploration_sims[[1]][[s]]$treatment_assignment)), 2, sum))), 
                           data.frame(n = 2500, allocated = rep(1:4, apply(do.call(rbind, lapply(1:250, function(s) exploration_sims[[2]][[s]]$treatment_assignment)), 2, sum))), 
                           data.frame(n = 5000, allocated = rep(1:4, apply(do.call(rbind, lapply(1:250, function(s) exploration_sims[[3]][[s]]$treatment_assignment)), 2, sum))),
                           data.frame(n = 10000, allocated = rep(1:4, apply(do.call(rbind, lapply(1:250, function(s) exploration_sims[[4]][[s]]$treatment_assignment)), 2, sum))))



exploration_allocation <- ggplot(exploration_sampling, aes(x = as.factor(n), fill = as.factor(allocated))) +
  geom_bar(position = 'fill') + scale_y_continuous(labels = scales::percent) + 
  labs(x = 'Experiment size (N)', y = 'Proportion allocated', fill = 'Group', title = 'Exploration Sampling') + 
  theme_bw()

#ggsave(exploration_allocation, file = 'exploration_allocation.pdf', width = 5, height = 4) 

combined_plot <- ggpubr::ggarrange(thompson_allocation, exploration_allocation, ncol = 2, 
                                   common.legend = TRUE,
                                   legend = 'bottom')

ggsave(combined_plot, file = 'combined_plot.pdf', width = 7, height = 4) 

## CATE

thompson_draws <- thompson_sims[[4]][[1]]$posterior_ate_draws
exploration_draws <- exploration_sims[[4]][[1]]$posterior_ate_draws
random_draws <- random_sims[[4]][[1]]$posterior_ate_draws

thompson_df <- data.frame(group = as.factor(1:4), 
           t(apply(thompson_draws, 2, function(i) quantile(i, c(0.025, 0.5, 0.975)))),
           sampling = 'Thompson')

exploration_df <- data.frame(group = as.factor(1:4), 
                          t(apply(exploration_draws, 2, function(i) quantile(i, c(0.025, 0.5, 0.975)))),
                          sampling = 'Exploration')

random_df <- data.frame(group = as.factor(1:4), 
                             t(apply(random_draws, 2, function(i) quantile(i, c(0.025, 0.5, 0.975)))),
                             sampling = 'Random')


summary_df <- as.data.frame(rbind(thompson_df, 
          exploration_df, 
          random_df))

colnames(summary_df)[2:4] <- c('lower', 'point', 'upper')

effect_plot <- ggplot(summary_df, aes(group = sampling, col = sampling)) + 
  geom_point(aes(x = group, y = point), position = position_dodge(width = .5)) + 
  geom_errorbar(aes(ymin = lower, ymax = upper, x = group), 
                position=position_dodge(width = .5), 
                width = 0) + 
  theme_bw() + 
  theme(legend.position = c(0.9, 0.1), 
        legend.justification = c(0.9, 0.1), 
        panel.grid.major = element_blank(), 
        legend.box.background = element_rect(colour = "black")) + 
  geom_point(aes(x = rep(as.factor(1:4), 3), y = rep(c(0, 0.05, 0.07, 0.09), 3)), 
             col = 'black', shape = 18, size = 3, position = position_nudge(x = 0.35)) + 
  labs(x = 'Group', y = 'CATE (estimate)')
  
ggsave(effect_plot, file = 'effect_plot.pdf', width = 6, height = 4) 




