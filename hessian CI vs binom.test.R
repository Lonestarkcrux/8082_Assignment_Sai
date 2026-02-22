library(data.table)
library(foreach)
library(ggplot2)

set.seed(123)

p_true <- 0.6
n_vals <- c(5,50,100,10000)

k_vals <- rbinom(length(n_vals), size=n_vals, prob=p_true)

get_hessian_ci <- function(k,n,alpha = 0.05) {
  p_hat<- k/n
  if (p_hat == 0 || p_hat == 1) {
    return(list(
      p_hat= p_hat,
      hessian = NA_real_,
      se_hessian = NA_real_,
      lwr_hessian = NA_real_,
      upr_hessian = NA_real_
    ))
  }
  
  hess <--(k/(p_hat^2))-((n-k)/((1-p_hat)^2))
  info <--hess
  se <- sqrt(1/info)
  z <- qnorm(1-alpha/2)
  
  ci <- p_hat + c (-1,1)*z*se
  ci <- pmax(0,pmin(1,ci))
  list(
    p_hat=p_hat,
    hessian = hess,
    se_hessian = se,
    lwr_hessian = ci[1],
    upr_hessian = ci[2]
  )
}

results <- foreach(i= seq_along(n_vals), .combine = rbind) %do% {
  n <- n_vals[i]
  k <- k_vals[i]
  
  h_out <- get_hessian_ci(k,n)
  
  bt<- binom.test(k,n)
  exact_ci <- bt$conf.int
  
  #endpoint distance
  endpoint_distance <- sqrt((h_out$lwr_hessian - exact_ci[1])^2 + (h_out$upr_hessian - exact_ci[2])^2)
  
  width_hessian <- h_out$upr_hessian - h_out$lwr_hessian
  width_exact <- exact_ci[2] - exact_ci[1]
  width_diff <- width_hessian - width_exact
  
  data.table(
    n=n,
    k=k,
    p_hat = h_out$p_hat,
    hessian_ci_lower = h_out$lwr_hessian,
    hessian_ci_uppwer = h_out$upr_hessian,
    exact_ci_lower = exact_ci[1],
    exact_ci_upper = exact_ci[2],
    hessian_width = width_hessian,
    exact_width = width_exact,
    width_diff_hessian_minus_exact = width_diff,
    ci_endpoint_distance = endpoint_distance
  )
}

print(results)
ggplot(results, aes(x = n, y = ci_endpoint_distance)) +
  geom_point(size = 3) +
  geom_line() +
  scale_x_continuous(trans = "log10", breaks = n_vals)+
  labs(
    x= "Number of flips (n, log scale)",
    y = "Distance between CIs (endpoint disance)",
    title = "Hessian CI vs exact binom.test CI"
  ) +
  theme_minimal()