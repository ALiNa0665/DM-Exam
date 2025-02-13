setwd("/work/AlinaMariaNechita#3266/PE")
install.packages("pacman")
install.packages("showtext")
install.packages("ggpattern")

pacman::p_load(ggpubr, extraDistr, truncnorm, ggplot2, showtext, gridExtra, ggpattern)

font_add_google("EB Garamond", "ebgaramond")
showtext_auto()

load("output_data/hier_est_negative.RData")
samples$BUGSoutput$summary

names(samples$BUGSoutput$sims.list)
# "deviance"   "mu_w"       "mu_A"   "mu_theta"   "mu_a" "lambda_w" "lambda_A" "lambda_theta" "lambda_a"

par(mfrow=c(2,2))
plot(density(samples$BUGSoutput$sims.list$mu_w))
plot(density(samples$BUGSoutput$sims.list$mu_A))
plot(density(samples$BUGSoutput$sims.list$mu_theta))
plot(density(samples$BUGSoutput$sims.list$mu_a))
plot(density(samples$BUGSoutput$sims.list$lambda_w))
plot(density(samples$BUGSoutput$sims.list$lambda_A))
plot(density(samples$BUGSoutput$sims.list$lambda_theta))
plot(density(samples$BUGSoutput$sims.list$lambda_a))

# Extracting samples for each parameter separately:

chains_mu_w <- samples$BUGSoutput$sims.array[, , "mu_w"]
chains_mu_A <- samples$BUGSoutput$sims.array[, , "mu_A"]
chains_mu_theta <- samples$BUGSoutput$sims.array[, , "mu_theta"]
chains_mu_a <- samples$BUGSoutput$sims.array[, , "mu_a"]
chains_lambda_w <- samples$BUGSoutput$sims.array[, , "lambda_w"]
chains_lambda_A <- samples$BUGSoutput$sims.array[, , "lambda_A"]
chains_lambda_theta <- samples$BUGSoutput$sims.array[, , "lambda_theta"]
chains_lambda_a <- samples$BUGSoutput$sims.array[, , "lambda_a"]
# Converting matrices to data frames for plotting:

chains_mu_w <- data.frame(
  Iteration = rep(1:nrow(chains_mu_w), times = ncol(chains_mu_w)),
  Chain = factor(rep(1:ncol(chains_mu_w), each = nrow(chains_mu_w))),
  Value = as.vector(chains_mu_w)
)

chains_mu_A <- data.frame(
  Iteration = rep(1:nrow(chains_mu_A), times = ncol(chains_mu_A)),
  Chain = factor(rep(1:ncol(chains_mu_A), each = nrow(chains_mu_A))),
  Value = as.vector(chains_mu_A)
)

chains_mu_theta <- data.frame(
  Iteration = rep(1:nrow(chains_mu_theta), times = ncol(chains_mu_theta)),
  Chain = factor(rep(1:ncol(chains_mu_theta), each = nrow(chains_mu_theta))),
  Value = as.vector(chains_mu_theta)
)

chains_mu_a <- data.frame(
  Iteration = rep(1:nrow(chains_mu_a), times = ncol(chains_mu_a)),
  Chain = factor(rep(1:ncol(chains_mu_a), each = nrow(chains_mu_a))),
  Value = as.vector(chains_mu_a)
)

chains_lambda_w <- data.frame(
  Iteration = rep(1:nrow(chains_lambda_w), times = ncol(chains_lambda_w)),
  Chain = factor(rep(1:ncol(chains_lambda_w), each = nrow(chains_lambda_w))),
  Value = as.vector(chains_lambda_w)
)

chains_lambda_A <- data.frame(
  Iteration = rep(1:nrow(chains_lambda_A), times = ncol(chains_lambda_A)),
  Chain = factor(rep(1:ncol(chains_lambda_A), each = nrow(chains_lambda_A))),
  Value = as.vector(chains_lambda_A)
)

chains_lambda_theta <- data.frame(
  Iteration = rep(1:nrow(chains_lambda_theta), times = ncol(chains_lambda_theta)),
  Chain = factor(rep(1:ncol(chains_lambda_theta), each = nrow(chains_lambda_theta))),
  Value = as.vector(chains_lambda_theta)
)

chains_lambda_a <- data.frame(
  Iteration = rep(1:nrow(chains_lambda_a), times = ncol(chains_lambda_a)),
  Chain = factor(rep(1:ncol(chains_lambda_a), each = nrow(chains_lambda_a))),
  Value = as.vector(chains_lambda_a)
)

# Plotting:

pl1 <- ggplot(chains_mu_w, aes(x = Iteration, y = Value, color = Chain)) +
  geom_line() +
  labs(
    title = expression(~ mu ~ w),
    x = "Iteration",
    y = expression(~ mu ~ w),
    color = "Chain"
  ) +
  scale_color_manual(
    values = c("1" = "#2ca02c", "2" = "#d62728", "3" = "#ff7f00", "4" = "#c5b0d5")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 50),
    axis.title.x = element_text(size = 40),
    axis.title.y = element_text(size = 40),
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 30)
  )

pl2 <- ggplot(chains_mu_A, aes(x = Iteration, y = Value, color = Chain)) +
  geom_line() +
  labs(
    title = expression(~ mu ~ A),
    x = "Iteration",
    y = expression(~ mu ~ A),
    color = "Chain"
  ) +
  scale_color_manual(
    values = c("1" = "#2ca02c", "2" = "#d62728", "3" = "#ff7f00", "4" = "#c5b0d5")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 50),
    axis.title.x = element_text(size = 40),
    axis.title.y = element_text(size = 40),
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 30)
  )

pl3 <- ggplot(chains_mu_theta, aes(x = Iteration, y = Value, color = Chain)) +
  geom_line() +
  labs(
    title = expression(~ mu ~ theta),
    x = "Iteration",
    y = expression(~ mu ~ theta),
    color = "Chain"
  ) +
  scale_color_manual(
    values = c("1" = "#2ca02c", "2" = "#d62728", "3" = "#ff7f00", "4" = "#c5b0d5")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 50),
    axis.title.x = element_text(size = 40),
    axis.title.y = element_text(size = 40),
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 30)
  )

pl4 <- ggplot(chains_mu_a, aes(x = Iteration, y = Value, color = Chain)) +
  geom_line() +
  labs(
    title = expression(~ mu ~ a),
    x = "Iteration",
    y = expression(~ mu ~ a),
    color = "Chain"
  ) +
  scale_color_manual(
    values = c("1" = "#2ca02c", "2" = "#d62728", "3" = "#ff7f00", "4" = "#c5b0d5")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 50),
    axis.title.x = element_text(size = 40),
    axis.title.y = element_text(size = 40),
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 30)
  )

pl11 <- ggplot(chains_lambda_w, aes(x = Iteration, y = Value, color = Chain)) +
  geom_line() +
  labs(
    title = expression(~ lambda ~ w),
    x = "Iteration",
    y = expression(~ lambda ~ w),
    color = "Chain"
  ) +
  scale_color_manual(
    values = c("1" = "#2ca02c", "2" = "#d62728", "3" = "#ff7f00", "4" = "#c5b0d5")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 50),
    axis.title.x = element_text(size = 40),
    axis.title.y = element_text(size = 40),
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 30)
  )

pl22 <- ggplot(chains_lambda_A, aes(x = Iteration, y = Value, color = Chain)) +
  geom_line() +
  labs(
    title = expression(~ lambda ~ A),
    x = "Iteration",
    y = expression(~ lambda ~ A),
    color = "Chain"
  ) +
  scale_color_manual(
    values = c("1" = "#2ca02c", "2" = "#d62728", "3" = "#ff7f00", "4" = "#c5b0d5")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 50),
    axis.title.x = element_text(size = 40),
    axis.title.y = element_text(size = 40),
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 30)
  )

pl33 <- ggplot(chains_lambda_theta, aes(x = Iteration, y = Value, color = Chain)) +
  geom_line() +
  labs(
    title = expression(~ lambda ~ theta),
    x = "Iteration",
    y = expression(~ lambda ~ theta),
    color = "Chain"
  ) +
  scale_color_manual(
    values = c("1" = "#2ca02c", "2" = "#d62728", "3" = "#ff7f00", "4" = "#c5b0d5")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 50),
    axis.title.x = element_text(size = 40),
    axis.title.y = element_text(size = 40),
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 30)
  )

pl44 <- ggplot(chains_lambda_a, aes(x = Iteration, y = Value, color = Chain)) +
  geom_line() +
  labs(
    title = expression(~ lambda ~ a),
    x = "Iteration",
    y = expression(~ lambda ~ a),
    color = "Chain"
  ) +
  scale_color_manual(
    values = c("1" = "#2ca02c", "2" = "#d62728", "3" = "#ff7f00", "4" = "#c5b0d5")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 50),
    axis.title.x = element_text(size = 40),
    axis.title.y = element_text(size = 40),
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 30)
  )


ggsave("plots/mu_w.png", pl1)
ggsave("plots/mu_A.png", pl2)
ggsave("plots/mu_theta.png", pl3)
ggsave("plots/mu_aa.png", pl4)
ggsave("plots/lambda_w.png", pl11)
ggsave("plots/lambda_A.png", pl22)
ggsave("plots/lambda_theta.png", pl33)
ggsave("plots/lambda_aa.png", pl44)




#now we do the same but for positive
load("output_data/hier_est_positive.RData")
samples$BUGSoutput$summary

par(mfrow=c(2,2))
plot(density(samples$BUGSoutput$sims.list$mu_w))
plot(density(samples$BUGSoutput$sims.list$mu_A))
plot(density(samples$BUGSoutput$sims.list$mu_theta))
plot(density(samples$BUGSoutput$sims.list$mu_a))
plot(density(samples$BUGSoutput$sims.list$lambda_w))
plot(density(samples$BUGSoutput$sims.list$lambda_A))
plot(density(samples$BUGSoutput$sims.list$lambda_theta))
plot(density(samples$BUGSoutput$sims.list$lambda_a))

# Extracting samples for each parameter separately:

chains_mu_w <- samples$BUGSoutput$sims.array[, , "mu_w"]
chains_mu_A <- samples$BUGSoutput$sims.array[, , "mu_A"]
chains_mu_theta <- samples$BUGSoutput$sims.array[, , "mu_theta"]
chains_mu_a <- samples$BUGSoutput$sims.array[, , "mu_a"]
chains_lambda_w <- samples$BUGSoutput$sims.array[, , "lambda_w"]
chains_lambda_A <- samples$BUGSoutput$sims.array[, , "lambda_A"]
chains_lambda_theta <- samples$BUGSoutput$sims.array[, , "lambda_theta"]
chains_lambda_a <- samples$BUGSoutput$sims.array[, , "lambda_a"]
# Converting matrices to data frames for plotting:

chains_mu_w <- data.frame(
  Iteration = rep(1:nrow(chains_mu_w), times = ncol(chains_mu_w)),
  Chain = factor(rep(1:ncol(chains_mu_w), each = nrow(chains_mu_w))),
  Value = as.vector(chains_mu_w)
)

chains_mu_A <- data.frame(
  Iteration = rep(1:nrow(chains_mu_A), times = ncol(chains_mu_A)),
  Chain = factor(rep(1:ncol(chains_mu_A), each = nrow(chains_mu_A))),
  Value = as.vector(chains_mu_A)
)

chains_mu_theta <- data.frame(
  Iteration = rep(1:nrow(chains_mu_theta), times = ncol(chains_mu_theta)),
  Chain = factor(rep(1:ncol(chains_mu_theta), each = nrow(chains_mu_theta))),
  Value = as.vector(chains_mu_theta)
)

chains_mu_a <- data.frame(
  Iteration = rep(1:nrow(chains_mu_a), times = ncol(chains_mu_a)),
  Chain = factor(rep(1:ncol(chains_mu_a), each = nrow(chains_mu_a))),
  Value = as.vector(chains_mu_a)
)

chains_lambda_w <- data.frame(
  Iteration = rep(1:nrow(chains_lambda_w), times = ncol(chains_lambda_w)),
  Chain = factor(rep(1:ncol(chains_lambda_w), each = nrow(chains_lambda_w))),
  Value = as.vector(chains_lambda_w)
)

chains_lambda_A <- data.frame(
  Iteration = rep(1:nrow(chains_lambda_A), times = ncol(chains_lambda_A)),
  Chain = factor(rep(1:ncol(chains_lambda_A), each = nrow(chains_lambda_A))),
  Value = as.vector(chains_lambda_A)
)

chains_lambda_theta <- data.frame(
  Iteration = rep(1:nrow(chains_lambda_theta), times = ncol(chains_lambda_theta)),
  Chain = factor(rep(1:ncol(chains_lambda_theta), each = nrow(chains_lambda_theta))),
  Value = as.vector(chains_lambda_theta)
)

chains_lambda_a <- data.frame(
  Iteration = rep(1:nrow(chains_lambda_a), times = ncol(chains_lambda_a)),
  Chain = factor(rep(1:ncol(chains_lambda_a), each = nrow(chains_lambda_a))),
  Value = as.vector(chains_lambda_a)
)

# Plotting:

pl5 <- ggplot(chains_mu_w, aes(x = Iteration, y = Value, color = Chain)) +
  geom_line() +
  labs(
    title = expression(~ mu ~ w),
    x = "Iteration",
    y = expression(~ mu ~ w),
    color = "Chain"
  ) +
  scale_color_manual(
    values = c("1" = "#2ca02c", "2" = "#d62728", "3" = "#ff7f00", "4" = "#c5b0d5")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 50),
    axis.title.x = element_text(size = 40),
    axis.title.y = element_text(size = 40),
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 30)
  )

pl6 <- ggplot(chains_mu_A, aes(x = Iteration, y = Value, color = Chain)) +
  geom_line() +
  labs(
    title = expression(~ mu ~ A),
    x = "Iteration",
    y = expression(~ mu ~ A),
    color = "Chain"
  ) +
  scale_color_manual(
    values = c("1" = "#2ca02c", "2" = "#d62728", "3" = "#ff7f00", "4" = "#c5b0d5")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 50),
    axis.title.x = element_text(size = 40),
    axis.title.y = element_text(size = 40),
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 30)
  )

pl7 <- ggplot(chains_mu_theta, aes(x = Iteration, y = Value, color = Chain)) +
  geom_line() +
  labs(
    title = expression(~ mu ~ theta),
    x = "Iteration",
    y = expression(~ mu ~ theta),
    color = "Chain"
  ) +
  scale_color_manual(
    values = c("1" = "#2ca02c", "2" = "#d62728", "3" = "#ff7f00", "4" = "#c5b0d5")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 50),
    axis.title.x = element_text(size = 40),
    axis.title.y = element_text(size = 40),
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 30)
  )

pl8 <- ggplot(chains_mu_a, aes(x = Iteration, y = Value, color = Chain)) +
  geom_line() +
  labs(
    title = expression(~ mu ~ a),
    x = "Iteration",
    y = expression(~ mu ~ a),
    color = "Chain"
  ) +
  scale_color_manual(
    values = c("1" = "#2ca02c", "2" = "#d62728", "3" = "#ff7f00", "4" = "#c5b0d5")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 50),
    axis.title.x = element_text(size = 40),
    axis.title.y = element_text(size = 40),
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 30)
  )
pl55 <- ggplot(chains_lambda_w, aes(x = Iteration, y = Value, color = Chain)) +
  geom_line() +
  labs(
    title = expression(~ lambda ~ w),
    x = "Iteration",
    y = expression(~ lambda ~ w),
    color = "Chain"
  ) +
  scale_color_manual(
    values = c("1" = "#2ca02c", "2" = "#d62728", "3" = "#ff7f00", "4" = "#c5b0d5")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 50),
    axis.title.x = element_text(size = 40),
    axis.title.y = element_text(size = 40),
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 30)
  )

pl66 <- ggplot(chains_lambda_A, aes(x = Iteration, y = Value, color = Chain)) +
  geom_line() +
  labs(
    title = expression(~ lambda ~ A),
    x = "Iteration",
    y = expression(~ lambda ~ A),
    color = "Chain"
  ) +
  scale_color_manual(
    values = c("1" = "#2ca02c", "2" = "#d62728", "3" = "#ff7f00", "4" = "#c5b0d5")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 50),
    axis.title.x = element_text(size = 40),
    axis.title.y = element_text(size = 40),
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 30)
  )

pl77 <- ggplot(chains_lambda_theta, aes(x = Iteration, y = Value, color = Chain)) +
  geom_line() +
  labs(
    title = expression(~ lambda ~ theta),
    x = "Iteration",
    y = expression(~ lambda ~ theta),
    color = "Chain"
  ) +
  scale_color_manual(
    values = c("1" = "#2ca02c", "2" = "#d62728", "3" = "#ff7f00", "4" = "#c5b0d5")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 50),
    axis.title.x = element_text(size = 40),
    axis.title.y = element_text(size = 40),
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 30)
  )

pl88 <- ggplot(chains_lambda_a, aes(x = Iteration, y = Value, color = Chain)) +
  geom_line() +
  labs(
    title = expression(~ lambda ~ a),
    x = "Iteration",
    y = expression(~ lambda ~ a),
    color = "Chain"
  ) +
  scale_color_manual(
    values = c("1" = "#2ca02c", "2" = "#d62728", "3" = "#ff7f00", "4" = "#c5b0d5")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 50),
    axis.title.x = element_text(size = 40),
    axis.title.y = element_text(size = 40),
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 30)
  )



ggsave("plots/positive_mu_w.png", pl5)
ggsave("plots/positive_mu_A.png", pl6)
ggsave("plots/positive_mu_theta.png", pl7)
ggsave("plots/positive_mu_aa.png", pl8)
ggsave("plots/positive_lambda_w.png", pl55)
ggsave("plots/positive_lambda_A.png", pl66)
ggsave("plots/positive_lambda_theta.png", pl77)
ggsave("plots/positive_lambda_aa.png", pl88)



# Plotting posterior densities with Negative and Positive:

library(tidyr)
library(dplyr)


load("output_data/hier_est_negative.RData")
samples_negative <- as.data.frame(samples$BUGSoutput$sims.list)

load("output_data/hier_est_positive.RData")
samples_positive <- as.data.frame(samples$BUGSoutput$sims.list)

samples_long_negative <- samples_negative %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value")
samples_long_negative <- samples_long_negative %>%
  filter(Variable != "deviance")

samples_long_positive <- samples_positive %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value")
samples_long_positive <- samples_long_positive %>%
  filter(Variable != "deviance")

samples_long_negative$Group <- "Negative"
samples_long_positive$Group <- "Positive"

estimates <- rbind(samples_long_negative, samples_long_positive)

library(dplyr)
library(ggplot2)

# Calculate 95% CI and density heights at the CI boundaries
ci_data <- estimates %>%
  group_by(Variable, Group) %>%
  summarize(
    CI_low = quantile(Value, 0.025),
    CI_high = quantile(Value, 0.975),
    Density_low = density(Value)$y[which.min(abs(density(Value)$x - quantile(Value, 0.025)))],
    Density_high = density(Value)$y[which.min(abs(density(Value)$x - quantile(Value, 0.975)))]
  )

ggplot(estimates, aes(x = Value, fill = Group, color = Group)) +
  geom_density(alpha = 0.4) +
  facet_wrap(
    ~ Variable, 
    scales = "free", 
    ncol = 2
  ) +
  geom_segment(
    data = ci_data,
    aes(x = CI_low, xend = CI_low, y = 0, yend = Density_low, color = Group),
    linetype = "dashed", size = 0.5, inherit.aes = FALSE
  ) +
  geom_segment(
    data = ci_data,
    aes(x = CI_high, xend = CI_high, y = 0, yend = Density_high, color = Group),
    linetype = "dashed", size = 0.5, inherit.aes = FALSE
  ) +
  theme_minimal() +
  labs(
    y = "Density"
  ) +
  scale_fill_manual(values = c("#A569BD", "#1F618D")) +
  scale_color_manual(values = c("#A569BD", "#1F618D")) + 
  theme(
    panel.grid = element_blank(),     
    panel.background = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5), 
    legend.title = element_blank(),
    axis.title.x = element_blank()
  )
