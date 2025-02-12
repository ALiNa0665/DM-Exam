install.packages("pacman")
install.packages("showtext")
pacman::p_load(tidyverse, gridExtra, grid, showtext)
setwd("/work/AlinaMariaNechita#3266/PVL/")


load("JAGS_hier_PVL_recovery_new.RData")
samples$BUGSoutput$summary

negative_recovery <- data.frame(true_mu_w, true_mu_A, true_mu_theta, true_mu_a, infer_mu_w, infer_mu_A, infer_mu_theta, infer_mu_a,
                 true_lambda_w, true_lambda_A, true_lambda_theta, true_lambda_a,
                 infer_lambda_w, infer_lambda_A, infer_lambda_theta, infer_lambda_a)
                 
samples1 <- samples


# PLOTS

font_add_google("EB Garamond", "ebgaramond")
showtext_auto()

pl1 <- ggplot(negative_recovery, 
              aes(x = true_mu_w,
                  y = infer_mu_w)) +
  geom_point(color = "#1ABC9C", size = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  geom_smooth(method = "lm", se = TRUE, formula = "y ~ x", color = "#1ABC9C") +
  theme_minimal() +
  xlab("True") + 
  ylab("Inferred") +
  ggtitle(expression(paste(mu, " - "))) +
  theme(plot.title = element_text(hjust = 0.5, size = 16)) 

pl2 <- ggplot(negative_recovery, 
              aes(x = true_mu_A,
                  y = infer_mu_A)) +
  geom_point(color = "#1ABC9C", size = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  geom_smooth(method = "lm", se = TRUE, formula = "y ~ x", color = "#1ABC9C") +
  theme_minimal() +
  xlab("True") + 
  ylab("Inferred") +
  ggtitle(expression(paste(mu, " - ",))) +
  theme(plot.title = element_text(hjust = 0.5, size = 16)) 

pl3 <- ggplot(negative_recovery, 
              aes(x = true_mu_theta,
                  y = infer_mu_theta)) +
  geom_point(color = "#1ABC9C", size = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  geom_smooth(method = "lm", se = TRUE, formula = "y ~ x", color = "#1ABC9C") +
  theme_minimal() +
  xlab("True") + 
  ylab("Inferred") +
  ggtitle(expression(paste(mu, " - "))) +
  theme(plot.title = element_text(hjust = 0.5, size = 16)) 

pl4 <- ggplot(negative_recovery, 
              aes(x = true_mu_a,
                  y = infer_mu_a)) +
  geom_point(color = "#1ABC9C", size = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  geom_smooth(method = "lm", se = TRUE, formula = "y ~ x", color = "#1ABC9C") +
  theme_minimal() +
  xlab("True") + 
  ylab("Inferred") +
  ggtitle(expression(paste(mu, " - "))) +
  theme(plot.title = element_text(hjust = 0.5, size = 16))

pl_negative_1 <- grid.arrange(pl1, pl2, pl3, pl4, nrow = 2, ncol = 3)
ggsave("plots_recovery/pl_negative_recovery_mu.png", pl_negative_1)

pl5 <- ggplot(negative_recovery, 
              aes(x = true_lambda_w,
                  y = infer_lambda_w)) +
  geom_point(color = "#1ABC9C", size = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  geom_smooth(method = "lm", se = TRUE, formula = "y ~ x", color = "#1ABC9C") +
  theme_minimal() +
  xlab("True") + 
  ylab("Inferred") +
  ggtitle(expression(paste(lambda, " - "))) +
  theme(plot.title = element_text(hjust = 0.5, size = 16))


pl6 <- ggplot(negative_recovery, 
              aes(x = true_lambda_A,
                  y = infer_lambda_A)) +
  geom_point(color = "#1ABC9C", size = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  geom_smooth(method = "lm", se = TRUE, formula = "y ~ x", color = "#1ABC9C") +
  theme_minimal() +
  xlab("True") + 
  ylab("Inferred") +
  ggtitle(expression(paste(lambda, " - "))) +
  theme(plot.title = element_text(hjust = 0.5, size = 16)) 

pl7 <- ggplot(negative_recovery, 
              aes(x = true_lambda_theta,
                  y = infer_lambda_theta)) +
  geom_point(color = "#1ABC9C", size = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  geom_smooth(method = "lm", se = TRUE, formula = "y ~ x", color = "#1ABC9C") +
  theme_minimal() +
  xlab("True") + 
  ylab("Inferred") +
  ggtitle(expression(paste(lambda, " - "))) +
  theme(plot.title = element_text(hjust = 0.5, size = 16)) 

pl8 <- ggplot(negative_recovery, 
              aes(x = true_lambda_a,
                  y = infer_lambda_a)) +
  geom_point(color = "#1ABC9C", size = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  geom_smooth(method = "lm", se = TRUE, formula = "y ~ x", color = "#1ABC9C") +
  theme_minimal() +
  xlab("True") + 
  ylab("Inferred") +
  ggtitle(expression(paste(lambda, " - "))) +
  theme(plot.title = element_text(hjust = 0.5, size = 16)) 


pl_negative_2 <- grid.arrange(pl5, pl6, pl7, pl8, nrow = 2, ncol = 3)
ggsave("plots_recovery/pl_negative_recovery_lambda.png", pl_negative_2)
