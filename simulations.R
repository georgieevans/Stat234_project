source('functions.R')

nsims <- 250
n_seq <- c(1000, 2500, 5000, 10000)

# Run Thompson

thompson_sims <- parallel::mclapply(n_seq, function(n) {
 return(lapply(1:nsims, function(i) thompsonSampling(N = n)))}, 
        mc.cores = 4
        )

save(thompson_sims, file = "sim_output/thompson_sims.Rdata")


# Run exploration 

exploration_sims <- parallel::mclapply(n_seq, function(n) {
  return(lapply(1:nsims, function(i) explorationSampling(N = n)))}, 
  mc.cores = 4
)

save(exploration_sims, file = "sim_output/exploration_sims.Rdata")


# Run SRS

random_sims <- parallel::mclapply(n_seq, function(n) {
  return(lapply(1:nsims, function(i) randomSampling(N = n)))}, 
  mc.cores = 4
)

save(random_sims, file = "sim_output/random_sims.Rdata")



