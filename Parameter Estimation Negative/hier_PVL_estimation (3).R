install.packages("pacman")
install.packages("R2jags")
install.packages("rjags")

# Load the packages
library(R2jags)
library(rjags)
pacman::p_load(R2jags, parallel, ggpubr, extraDistr, truncnorm)

set.seed(2002)

setwd('/work/PE')

# defining a function for calculating the maximum of the posterior density (not exactly the same as the mode)
MPD <- function(x) {
  density(x)$x[which(density(x)$y==max(density(x)$y))]
}

#load data
negative_data <- read.csv("parameter_estimation.csv", sep=";")

# filter participants who had a negative Net_Outcome and 0 Outcome in the first trial
first_trial_neg <- negative_data[negative_data$Trial == 1, ]
negative_participants <- first_trial_neg[first_trial_neg$Net_Outcome < 0, "Subj_ID"]


negative_data <- negative_data[negative_data$Subj_ID %in% negative_participants, ]

#----------prepare data for jags models - want trial x subject arrays for choice and gain & loss ----
# identify and count unique subject IDs
subIDs <- unique(negative_data$Subj_ID)
nsubs <- length(subIDs)
ntrials_max <- 100

# all choices (x) and outcomes (X)
x_raw <- negative_data$Choice
X_raw <- negative_data$Wins + negative_data$Losses #note the sign!

# empty arrays to fill
ntrials_all <- array(0,c(nsubs))
x_all <- array(0,c(nsubs,ntrials_max))
X_all <- array(0,c(nsubs,ntrials_max))

for (s in 1:nsubs) {
  
  #record n trials for subject s
  ntrials_all[s] <- length(x_raw[negative_data$Subj_ID==subIDs[s]])
  
  #pad trials with NA if n trials < maximum (i.e. 100)
  x_sub <- x_raw[negative_data$Subj_ID==subIDs[s]] 
  length(x_sub) <- ntrials_max
  
  X_sub <- X_raw[negative_data$Subj_ID==subIDs[s]] 
  length(X_sub) <- ntrials_max
  
  # assign arrays
  x_all[s,] <- x_sub
  X_all[s,] <- X_sub
  
}

###########################################################
#---------- run the hierarchical model on controls --------
###########################################################

x <- x_all
X <- X_all

ntrials <- ntrials_all

# set up jags and run jags model
data <- list("x","X","ntrials","nsubs") 
params<-c("mu_w","mu_A","mu_theta","mu_a","lambda_w","lambda_A","lambda_theta","lambda_a")

start_time = Sys.time()
samples <- jags.parallel(data, inits=NULL, params,
                         model.file ="hier_PVL.txt",
                         n.chains=3, n.iter=5000, n.burnin=1000, n.thin=1, n.cluster=4)
end_time = Sys.time()
end_time - start_time

save(samples,
     file = "output_data/hier_est_negative.RData")
# SAVE SAMPLES HERE

# let's look at the posteriors for the parameters
par(mfrow=c(2,2))
pl1 <- plot(density(samples$BUGSoutput$sims.list$mu_w))
pl2 <- plot(density(samples$BUGSoutput$sims.list$mu_A))
pl3 <- plot(density(samples$BUGSoutput$sims.list$mu_theta))
pl4 <- plot(density(samples$BUGSoutput$sims.list$mu_a))

ggarrange(pl1, pl2, pl3, pl4)
ggsave('pl1_mu_w.png', pl1)
ggsave('pl2_mu_A.png', pl2)
ggsave('pl3_mu_theta.png', pl3)
ggsave('pl4_mu_a.png', pl4)

# let's look at the posteriors for the parameters
par(mfrow=c(2,2))
pl1_1 <- plot(density(samples$BUGSoutput$sims.list$lambda_w))
pl2_2 <- plot(density(samples$BUGSoutput$sims.list$lambda_A))
pl3_3 <- plot(density(samples$BUGSoutput$sims.list$lambda_theta))
pl4_4 <- plot(density(samples$BUGSoutput$sims.list$lambda_a))

ggarrange(pl1_1, pl2_2, pl3_3, pl4_4)
ggsave('pl1_lambda_w.png', pl1_1)
ggsave('pl2_lambda_A.png', pl2_2)
ggsave('pl3_lambda_theta.png', pl3_3)
ggsave('pl4_lambda_a.png', pl4_4)


