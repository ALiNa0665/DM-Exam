install.packages("pacman")
pacman::p_load(R2jags, parallel, ggplot2)


set.seed(2002)

setwd('/work/AlinaMariaNechita#3266/PPC30')

# defining a function for calculating the maximum of the posterior density (not exactly the same as the mode)
MPD <- function(x) {
  density(x)$x[which(density(x)$y==max(density(x)$y))]
}


#load control data
negative_data <- read.csv("parameter_estimation.csv",sep = ";")

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
X_raw <- negative_data$Wins + negative_data$Losses

#--- assign choices and outcomes in trial x sub matrix

# empty arrays to fill
ntrials_all <- array(0,c(nsubs))
x_all <- array(0,c(nsubs,ntrials_max))
X_all <- array(0,c(nsubs,ntrials_max))

for (s in 1:nsubs) {
  
  #record n trials for subject s
  ntrials_all[s] <- length(x_raw[negative_data$Subj_ID==subIDs[s]])
  
  #ntrials_all[s] <- length(x_raw[negative_data$Subj_ID==subIDs[s]])
  
  #pad trials with NA if n trials < maximum (i.e. 100)
  x_sub <- x_raw[negative_data$Subj_ID==subIDs[s]] 
  length(x_sub) <- ntrials_max
  
  X_sub <- X_raw[negative_data$Subj_ID==subIDs[s]] 
  length(X_sub) <- ntrials_max
  
  # assign arrays
  x_all[s,] <- x_sub
  X_all[s,] <- X_sub
  
}
X_all <- X_all/100

# let's see how the model goes for more than 1 subject. Let's run this on all subjects
pred_success <- array(nsubs)

start_time = Sys.time()

for (s in 1:nsubs) {
  
  x <- x_all[s, ]
  X <- X_all[s, ]
  
  ntrials <- ntrials_all[s]
  
  # set up jags and run jags model on all subjects
  data <- list("x","X","ntrials") 
  params<-c("w","A","theta","a","p")
  samples <- jags.parallel(data, inits=NULL, params,
                                model.file ="PVL.txt",
                                n.chains=3, n.iter=5000, n.burnin=1000, n.thin=1, n.cluster=3)
  
  p_post <- samples$BUGSoutput$sims.list$p
  
  x_predict <- array(ntrials)
  
  for (t in 1:ntrials) {
    p_predict <- c(
      MPD(p_post[,t,1]),
      MPD(p_post[,t,2]),
      MPD(p_post[,t,3]),
      MPD(p_post[,t,4])
    )
    
    x_predict[t] <- which.max(p_predict)
    
  }
  
  pred_success[s] <- sum(x_predict==x[1:ntrials]) # only comparing with trials for which we have choices
  print(s)
  
}

end_time = Sys.time()
end_time - start_time

pred_success_adjust <- pred_success/ntrials_all

avg_pred <- mean(pred_success_adjust)

# plotting code courtesy of Mia
pred_df <- data.frame(pred_success_adjust)
save(pred_df,
     file = "output_data/ppc_pred_negative.RData")

pred_df$sub <- 1:length(pred_success_adjust) # rownames(pred_df) # creating a subject index
pred_df$avg <- mean(pred_df$pred_success)
pred_df$std <- sd(pred_df$pred_success)
pred_df$chance <- .25
pl1 <- ggplot(pred_df, aes(sub, pred_success_adjust)) +
  geom_point() +
  geom_line(aes(y=chance), linetype="dashed", color = "black") +
  geom_ribbon(aes(xmin = -Inf, xmax = Inf, ymin = avg - std, ymax = avg + std), fill = "pink", alpha = 0.6) + 
  geom_line(aes(y=avg)) +
  ylim(0, 1)
ggsave('output_data/pred_all.png', pl1)