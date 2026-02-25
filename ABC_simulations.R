rm(list = ls())

library(data.table)
library(foreach)
library(doMC)
library(causaldata)
library(ggplot2)

dt_obs <- data.table(restaurant_inspections)
x_obs <- log10(dt_obs$NumberofLocations)
y_obs <- dt_obs$inspection_score

summary_stats <- function(y, x) {
  fit <- lm(y ~ x)
  c(
    beta0 = coef(fit)[1],
    beta1 = coef(fit)[2],
    sigma2 = summary(fit)$sigma^2
  )
}

S_obs <- summary_stats(y_obs, x_obs)

run_abc <- function(nsim, tol, beta0_mean, beta0_sd, beta1_mean, beta1_sd) {
  registerDoMC(8)
  
  sim_results <- foreach(i = 1:nsim, .combine = rbind) %dopar% {
    beta0 <- rnorm(1, beta0_mean, beta0_sd)
    beta1 <- rnorm(1, beta1_mean, beta1_sd)
    
    y_sim <- beta0 + beta1 * x_obs + rnorm(length(y_obs), 0, sd(y_obs) / 2)
    
    S_sim <- summary_stats(y_sim, x_obs)
    dist <- sum((S_sim - S_obs)^2)
    
    data.table(beta0 = S_sim[1], beta1 = S_sim[2], dist = dist)
  }
  
  eps <- quantile(sim_results$dist, tol)
  post <- sim_results[dist < eps]
  
  list(
    post = post,
    summary = data.table(
      nsim = nsim,
      tol = tol,
      beta0_mean = beta0_mean,
      beta0_sd = beta0_sd,
      beta1_mean_prior = beta1_mean,
      beta1_sd = beta1_sd,
      n_accept = nrow(post),
      beta1_post_mean = mean(post$beta1),
      beta1_ci_low = as.numeric(quantile(post$beta1, 0.025)),
      beta1_ci_high = as.numeric(quantile(post$beta1, 0.975)),
      beta1_ci_width = as.numeric(quantile(post$beta1, 0.975) - quantile(post$beta1, 0.025))
    )
  )
}

res_prior_diffuse <- run_abc(15000, 0.01, 0, 10, 0, 10)
res_prior_inform <- run_abc(15000, 0.01, 100, 5, -1, 2)

res_tol_1pct <- run_abc(15000, 0.01, 0, 10, 0, 10)
res_tol_05pct <- run_abc(15000, 0.005, 0, 10, 0, 10)

res_nsim_15000 <- run_abc(15000, 0.01, 0, 10, 0, 10)
res_nsim_100000 <- run_abc(100000, 0.01, 0, 10, 0, 10)

dt_summary <- rbindlist(list(
  cbind(change = "prior", setting = "diffuse (0,10)", res_prior_diffuse$summary),
  cbind(change = "prior", setting = "inform (100,5) & (-1,2)", res_prior_inform$summary),
  cbind(change = "tolerance", setting = "tol = 0.01", res_tol_1pct$summary),
  cbind(change = "tolerance", setting = "tol = 0.005", res_tol_05pct$summary),
  cbind(change = "nsim", setting = "nsim = 15000", res_nsim_15000$summary),
  cbind(change = "nsim", setting = "nsim = 100000", res_nsim_100000$summary)
))

print(dt_summary[, .(change, setting, nsim, tol, n_accept, beta1_post_mean, beta1_ci_low, beta1_ci_high, beta1_ci_width)])

dt_post <- rbindlist(list(
  cbind(change = "prior", setting = "diffuse (0,10)", res_prior_diffuse$post),
  cbind(change = "prior", setting = "inform (100,5) & (-1,2)", res_prior_inform$post),
  cbind(change = "tolerance", setting = "tol = 0.01", res_tol_1pct$post),
  cbind(change = "tolerance", setting = "tol = 0.005", res_tol_05pct$post),
  cbind(change = "nsim", setting = "nsim = 15000", res_nsim_15000$post),
  cbind(change = "nsim", setting = "nsim = 100000", res_nsim_100000$post)
), use.names = TRUE, fill = TRUE)

ggplot(dt_post, aes(x = beta1, color = setting)) +
  geom_density(linewidth = 1) +
  facet_wrap(~ change, scales = "free_y") +
  theme_bw()

ggplot(dt_summary, aes(x = reorder(setting, beta1_ci_width), y = beta1_ci_width, fill = change)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~ change, scales = "free_y") +
  theme_bw() +
  xlab("") +
  ylab("95% CI width (beta1)")