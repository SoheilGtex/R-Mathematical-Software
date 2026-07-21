# پروژه1
# Poisson regression using maximum likelihood
#ابوالفضل رحمی
#4022011036
library(readxl)

# Read the data
crab_data <- read_excel("Crabs.xlsx")

# Dummy variables for color
crab_data$C2 <- ifelse(crab_data$C == 2, 1, 0)
crab_data$C3 <- ifelse(crab_data$C == 3, 1, 0)
crab_data$C4 <- ifelse(crab_data$C == 4, 1, 0)

# Dummy variables for spine
crab_data$S2 <- ifelse(crab_data$S == 2, 1, 0)
crab_data$S3 <- ifelse(crab_data$S == 3, 1, 0)

# Design matrix
X <- cbind(
  1,
  crab_data$C2,
  crab_data$C3,
  crab_data$C4,
  crab_data$S2,
  crab_data$S3,
  crab_data$W,
  crab_data$Wt
)

y <- crab_data$Sa

# Neg log-likelihood
poisson_nll <- function(beta) {

  eta <- X %*% beta
  lambda <- exp(eta)

  log_lik <- sum(
    dpois(y, lambda = lambda, log = TRUE)
  )

  return(-log_lik)
}


beta_start <- c(
  log(mean(y)),
  rep(0, 7)
)

# Estimate parameters
fit <- nlminb(
  start = beta_start,
  objective = poisson_nll
)

mle_coef <- fit$par
names(mle_coef) <- c(
  "(Intercept)", "C2", "C3", "C4",
  "S2", "S3", "W", "Wt"
)

print(mle_coef)

# Fit the same model using glm
glm_fit <- glm(
  Sa ~ C2 + C3 + C4 + S2 + S3 + W + Wt,
  data = crab_data,
  family = poisson(link = "log")
)

glm_coef <- coef(glm_fit)
print(glm_coef)

# Compare the results
comparison <- data.frame(
  MLE = mle_coef,
  GLM = glm_coef,
  Difference = abs(mle_coef - glm_coef)
)


# Check optimization
print(fit$convergence)

cat("Number of observations:", nrow(crab_data), "\n")

cat(
  "nlminb convergence:",
  fit$convergence,
  "(0 means success)\n"
)

cat(
  "Negative log-likelihood:",
  format(fit$objective, digits = 12),
  "\n\n"
)

print(comparison, row.names = FALSE)

cat(
  "\nMaximum absolute difference:",
  format(max(comparison$Difference), digits = 8),
  "\n"
)

if (max(comparison$Difference) < 1e-5) {
  cat(
    "Conclusion: nlminb and glm coefficients agree up to numerical precision.\n"
  )
} else {
  cat(
    "Conclusion: Small differences may be due to optimization tolerance and stopping criteria.\n"
  )
}
print(fit$objective)
