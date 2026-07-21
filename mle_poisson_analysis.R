# Alternative Implementation: Poisson MLE using Numerical Optimization

# 1. Generate or load data
set.seed(42)
data_poisson <- rpois(n = 100, lambda = 5)

# 2. Define Log-Likelihood Function
poisson_log_lik <- function(param, x) {
  lambda <- param[1]
  if (lambda <= 0) return(Inf) # Boundary constraint
  
  # Negative log-likelihood for minimization
  n <- length(x)
  neg_ll <- - (sum(x) * log(lambda) - n * lambda - sum(lfactorial(x)))
  return(neg_ll)
}

# 3. Estimate Lambda using optim()
init_val <- c(lambda = 1)
opt_result <- optim(par = init_val, fn = poisson_log_lik, x = data_poisson, method = "L-BFGS-B", lower = 0.001)

# 4. Results
cat("Estimated Lambda (Numerical MLE):", opt_result$par, "\n")
cat("Analytical Mean (Sample Mean):", mean(data_poisson), "\n")
