install.packages("pacman")
install.packages("showtext")
pacman::p_load(parallel, ggpubr, extraDistr, truncnorm, ggplot2, showtext, gridExtra)

font_add_google("EB Garamond", "ebgaramond")
showtext_auto()

setwd("/work/AlinaMariaNechita#3266/PVL2/Traceplots 110")
load("../JAGS_hier_PVL_recovery_two_new.RData")

# Investigating the dimensions:
dim(samples$BUGSoutput$sims.array) 

# Parameter names:
names(samples$BUGSoutput$sims.list)
#[1] "deviance"       "lambda_w"       "lambda_a"   "lambda_A"   "lambda_theta"  "mu_w"  "mu_a"       "mu_A"     "mu_theta"    


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


ggsave("traceplots/mu_w.png", pl1)
ggsave("traceplots/mu_A.png", pl2)
ggsave("traceplots/mu_theta.png", pl3)
ggsave("traceplots/mu_aa.png", pl4)
ggsave("traceplots/lambda_w.png", pl11)
ggsave("traceplots/lambda_A.png", pl22)
ggsave("traceplots/lambda_theta.png", pl33)
ggsave("traceplots/lambda_aa.png", pl44)

plot_1 <- ggarrange(pl1, pl2, pl3, pl4, ncol = 1, nrow = 5)

ggsave("traceplots/traceplots_negative_mu.png", plot_1, width = 10, height = 20)

plot_2 <- ggarrange(pl11, pl22, pl33, pl44, ncol = 1, nrow = 5)

ggsave("traceplots/traceplots_negative_lambda.png", plot_2, width = 10, height = 20)
