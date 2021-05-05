thompsonSampling <- function(N, p_treat = 0.5, alpha_0 = 1, beta = 1, post_draws = 1000) {
  
  ## Set params
  true_theta <- rbind(c(0.9, 0.9), c(0.85, 0.9), c(0.83, 0.9), c(0.81, 0.9))
  
  # Param matrix
  control_params <- treatment_params <- matrix(1, nrow = 4, ncol = 2)
  
  # Store group assignment 
  group_ind_vec <- c()
  
  ## Thompson sampling 
  for(i in 1:N) {
    
    # Sample from posterior Thompson 
    ate_draws <- sapply(1:4, FUN = function(i) rbeta(1, shape1 = treatment_params[i, 1], shape2 = treatment_params[i, 2])) - 
      sapply(1:4, FUN = function(i) rbeta(1, shape1 = control_params[i, 1], shape2 = control_params[i, 2]))
    
    
    # Group indicator
    group_ind <- which.max(ate_draws)
    
    # Sample treat vs. control 
    treat_ind <- rbinom(1, 1, p_treat)
    
    group_ind_vec <- append(group_ind_vec, group_ind) 
    
    # Draw outcome 
    s <- rbinom(1, 1, true_theta[group_ind, treat_ind + 1])
    
    # Update params 
    
    if (treat_ind == 1) {
      
      treatment_params[group_ind, 1] <- treatment_params[group_ind, 1]  + s
      treatment_params[group_ind, 2] <- treatment_params[group_ind, 2]  + 1 - s
      
    } else {
      
      control_params[group_ind, 1] <- control_params[group_ind, 1]  + s
      control_params[group_ind, 2] <- control_params[group_ind, 2]  + 1 - s
      
    }
   
  }
  
    final_draws <- do.call(cbind, lapply(1:4, FUN = function(i) rbeta(post_draws, shape1 = treatment_params[i, 1], shape2 = treatment_params[i, 2]) - 
                                           rbeta(post_draws, shape1 = control_params[i, 1], shape2 = control_params[i, 2])))
    chosen_group <- which.max(apply(final_draws, 2, mean))
    
  return(
    list(treatment_assignment = group_ind_vec,
    posterior_ate_draws = final_draws,
    chosen_group = chosen_group, 
    N = N
    )
    )
}


explorationSampling <- function(N, rounds = 100, post_draws = 1000, p_treat = 0.5,  alpha_0 = 1, beta = 1) {
  
  
  # 4 subgroups
  true_theta <- rbind(c(0.9, 0.9), c(0.85, 0.9), c(0.83, 0.9), c(0.81, 0.9))
  
  # Prior 
  control_params <- treatment_params <- matrix(1, nrow = 4, ncol = 2)
  
  # Store group assignment 
  group_ind_vec <- c()
  N_t <- N/rounds
  assignments_vec <- rep(0, 4)
  
  for(j in 1:rounds) {
    
    # Sample from posterior Thompson 
    ate_draws <- do.call(cbind, lapply(1:4, FUN = function(i) rbeta(post_draws, shape1 = treatment_params[i, 1], shape2 = treatment_params[i, 2]) - 
                                         rbeta(post_draws, shape1 = control_params[i, 1], shape2 = control_params[i, 2])))
                                         
    max_post <- sapply(1:nrow(ate_draws), function(i) which.max(ate_draws[i,]))
    
    p_t <- sapply(1:4, function(i) sum(i == max_post))/post_draws
    
    if(p_t[4] == 1){
      p_t[4] <- 0.92
      p_t[1:3] <- 0.02
    }
    
    q_t <- p_t*(1-p_t)*(1/sum(p_t*(1-p_t)))
    
    
    assignments <- round(N_t*q_t)
    
    assignments_vec <- assignments_vec + assignments
    
    # For each group 
    for(i in 1:4){ 
      
      if(assignments[i] > 0) {
          
      # Sample treat/ control
      treat_ind <- rbinom(assignments[i], 1, p_treat)
      
      # Draw outcome 
      s <- sapply(1:length(treat_ind), function(k) rbinom(1, 1, true_theta[i, treat_ind[k] + 1]))
      
      # Update params 
      treatment_params[i, 1] <- treatment_params[i, 1]  + sum(s*treat_ind)
      treatment_params[i, 2] <- treatment_params[i, 2]  + sum(treat_ind) - sum(s*treat_ind)
      
      control_params[i, 1] <- control_params[i, 1]  + sum(s*(1 -treat_ind))
      control_params[i, 2] <- control_params[i, 2]  + sum(1 - treat_ind) - sum(s*(1 - treat_ind))
      
      }
    }
  }
  
    # Final draws
    ate_draws <- do.call(cbind, lapply(1:4, FUN = function(i) rbeta(post_draws, shape1 = treatment_params[i, 1], shape2 = treatment_params[i, 2]) - 
                                         rbeta(post_draws, shape1 = control_params[i, 1], shape2 = control_params[i, 2])))
    
    # Choose group to target
    chosen_group <- which.max(apply(ate_draws, 2, mean))
      
    return(
      list(
        treatment_assignment = assignments_vec,
        posterior_ate_draws = ate_draws,
        chosen_group = chosen_group, 
        N = N
      )
    )
  }
  
  

randomSampling <- function(N, p_treat = 0.5, post_draws = 1000) {
  
  true_theta <- rbind(c(0.9, 0.9), c(0.85, 0.9), c(0.83, 0.9), c(0.81, 0.9))
  
  # Prior 
  control_params <- treatment_params <- matrix(1, nrow = 4, ncol = 2)
  
  N_g <- N/4
  
  for(i in 1:4){
    
    s_control <- rbinom(1, round(N_g*(1 - p_treat)), true_theta[i, 1])
    s_treat <- rbinom(1, round(N_g*(p_treat)), true_theta[i, 2])

    # Update params 
    treatment_params[i, 1] <- treatment_params[i, 1]  + s_treat
    treatment_params[i, 2] <- treatment_params[i, 2]  + round(N_g*(p_treat)) - s_treat
    
    control_params[i, 1] <- control_params[i, 1]  + s_control
    control_params[i, 2] <- control_params[i, 2]  + round(N_g*(1 - p_treat)) - s_control
  }
  
  # Final draws
  ate_draws <- do.call(cbind, lapply(1:4, FUN = function(i) rbeta(post_draws, shape1 = treatment_params[i, 1], shape2 = treatment_params[i, 2]) - 
                                       rbeta(post_draws, shape1 = control_params[i, 1], shape2 = control_params[i, 2])))
  
  # Choose group to target
  chosen_group <- which.max(apply(ate_draws, 2, mean))
  
  return(
    list(
      posterior_ate_draws = ate_draws,
      chosen_group = chosen_group, 
      N = N
    )
  )
}
  




