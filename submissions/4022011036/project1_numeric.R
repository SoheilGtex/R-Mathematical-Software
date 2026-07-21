# Project 1
# Estimation of a Poisson regression model using Maximum Likelihood

library(readxl)

# ------------------------------------------------------------
# 1. Read the crab dataset
# ------------------------------------------------------------

data_crab <- read_excel("Crabs.xlsx")

# ------------------------------------------------------------
# 2. Generate indicator variables
# ------------------------------------------------------------

# Color indicators: color 1 is the reference category
data_crab$C2 <- ifelse(data_crab$C == 2, 1L, 0L)
data_crab$C3 <- ifelse(data_crab$C == 3, 1L, 0L)
data_crab$C4 <- ifelse(data_crab$C == 4, 1L, 0L)

# Spine indicators: spine 1 is the reference category
data_crab$S2 <- ifelse(data_crab$S == 2, 1L, 0L)
data_crab$S3 <- ifelse(data_crab$S == 3, 1L, 0L)

# ------------------------------------------------------------
# 3. Construct the model matrix
# ------------------------------------------------------------

design_matrix <- cbind(
  Intercept = 1,
  C2 = data_crab$C2,
  C3 = data_crab$C3,
  C4 = data_crab$C4,
  S2 = data_crab$S2,
  S3 = data_crab$S3,
  W = data_crab$W,
  Wt = data_crab$Wt
)

response <- data_crab$Sa

# ------------------------------------------------------------
# 4. Define the negative log-likelihood function
# ------------------------------------------------------------

negative_log_likelihood <- function(beta) {

  # Linear predictor
  linear_predictor <- as.vector(design_matrix %*% beta)

  # Expected number of satellites
  expected_count <- exp(linear_predictor)

  # Log-likelihood of the Poisson distribution
  log_likelihood <- sum(
    dpois(
      x = response,
      lambda = expected_count,
      log = TRUE
    )
  )

  # nlminb minimizes the objective function
  return(-log_likelihood)
}

# ------------------------------------------------------------
# 5. Choose initial parameter values
# ------------------------------------------------------------

starting_values <- numeric(ncol(design_matrix))
starting_values[1] <- log(mean(response))

# ------------------------------------------------------------
# 6. Estimate parameters using maximum likelihood
# ------------------------------------------------------------

manual_poisson_fit <- nlminb(
  start = starting_values,
  objective = negative_log_likelihood
)

manual_estimates <- manual_poisson_fit$par
names(manual_estimates) <- colnames(design_matrix)

cat("Maximum likelihood estimates using nlminb:\n")
print(manual_estimates)

# ------------------------------------------------------------
# 7. Estimate the same model using glm
# ------------------------------------------------------------

poisson_glm_fit <- glm(
  Sa ~ C2 + C3 + C4 + S2 + S3 + W + Wt,
  data = data_crab,
  family = poisson(link = "log")
)

glm_estimates <- coef(poisson_glm_fit)

cat("\nPoisson regression estimates using glm:\n")
print(glm_estimates)

# ------------------------------------------------------------
# 8. Compare the estimated coefficients
# ------------------------------------------------------------

coefficient_comparison <- data.frame(
  Parameter = colnames(design_matrix),
  Manual_MLE = unname(manual_estimates),
  GLM_Estimate = unname(glm_estimates)
)

coefficient_comparison$Absolute_Difference <- with(
  coefficient_comparison,
  abs(Manual_MLE - GLM_Estimate)
)

cat("\nComparison of the two estimation methods:\n")
print(coefficient_comparison)

# ------------------------------------------------------------
# 9. Display optimization information
# ------------------------------------------------------------

cat("\nOptimization convergence code:\n")
print(manual_poisson_fit$convergence)

cat("\nFinal negative log-likelihood:\n")
print(manual_poisson_fit$objective)
