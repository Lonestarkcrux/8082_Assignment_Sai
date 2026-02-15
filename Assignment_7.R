##How does sample size effect winner's curse


library(data.table)
library(foreach)
library(ggplot2)


run_experiment <- function(n, effect, sd = 1) {
  control <- rnorm(n, 0, sd)
  treatment <- rnorm(n, effect, sd)
  
  test <- t.test(treatment, control)
  
  data.table(
    pval = test$p.value,
    est = mean(treatment) - mean(control)
  )
}


set.seed(123)

true_effect <- 0.5
alpha <- 0.05
n_sims <- 2000

sample_sizes <- seq(10, 200, by = 10)



winner_results <- foreach(n = sample_sizes, .combine = rbind) %do% {
  
  sim_results <- foreach(i = 1:n_sims, .combine = rbind) %do% {
    run_experiment(n, true_effect)
  }
  
  sim_results[, significant := pval < alpha]
  
  mean_sig_est <- mean(sim_results[significant == TRUE]$est)
  
  data.table(
    n = n,
    winners_curse = mean_sig_est - true_effect,
    power = mean(sim_results$significant)
  )
}


winner_plot <-
  ggplot(winner_results, aes(x = n, y = winners_curse)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Winner's Curse Decreases with Sample Size",
    subtitle = "Bias among statistically significant results",
    x = "Sample size per group",
    y = "Winner's curse (Mean significant estimate − True effect)"
  ) +
  theme_bw()

winner_plot

