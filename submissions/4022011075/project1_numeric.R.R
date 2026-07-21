train_data <- read.csv("Crabs 2.csv")
train_data$C2 <- ifelse(train_data$C == 2, 1, 0)
train_data$C3 <- ifelse(train_data$C == 3, 1, 0)
train_data$C4 <- ifelse(train_data$C == 4, 1, 0)
train_data$S2 <- ifelse(train_data$S == 2, 1, 0)
train_data$S3 <- ifelse(train_data$S == 3, 1, 0)
nloglik_loglinear <- function(theta, data) {
  beta0 <- theta[1]
  beta1 <- theta[2]
  beta2 <- theta[3]
  beta3 <- theta[4]
  beta4 <- theta[5]
  beta5 <- theta[6]
  beta6 <- theta[7]
  beta7 <- theta[8]
  eta <- beta0 + beta1*data$C2 + beta2*data$C3 + beta3*data$C4 +
          beta4*data$S2 + beta5*data$S3 + beta6*data$W + beta7*data$Wt
  lambda <- exp(eta)
  log_lik <- sum(dpois(data$Sa, lambda, log = TRUE))
  return(-log_lik)
}
init_theta <- c(log(mean(train_data$Sa)), rep(0, 7))
names(init_theta) <- c("Intercept", "C2", "C3", "C4", "S2", "S3", "W", "Wt")
opt <- nlminb(start = init_theta, 
              objective = nloglik_loglinear, 
              data = train_data,
              control = list(iter.max = 1000, eval.max = 1000))
manual_estimates <- opt$par
names(manual_estimates) <- c("Intercept", "C2", "C3", "C4", "S2", "S3", "W", "Wt")
cat("Manual MLE Estimates (from nlminb):\n")
print(manual_estimates)
train_data$C <- as.factor(train_data$C)
train_data$S <- as.factor(train_data$S)
glm_fit <- glm(Sa ~ C + S + W + Wt, 
               data = train_data, 
               family = poisson(link = "log"))
glm_estimates <- coef(glm_fit)
cat("glm() Estimates:\n")
print(glm_estimates)
glm_ordered <- c(glm_estimates["(Intercept)"],
                 glm_estimates["C2"],
                 glm_estimates["C3"],
                 glm_estimates["C4"],
                 glm_estimates["S2"],
                 glm_estimates["S3"],
                 glm_estimates["W"],
                 glm_estimates["Wt"])
comparison <- data.frame(
  Parameter = names(manual_estimates),
  Manual_MLE = manual_estimates,
  glm_est = glm_ordered,
  Difference = manual_estimates - glm_ordered
)
cat("Comparison Table:\n")
print(comparison)
max_diff <- max(abs(comparison$Difference), na.rm = TRUE)
cat("Maximum absolute difference:", max_diff, "\n")
if (max_diff < 1e-6) {
  cat("The manual MLE matches glm() almost exactly!\n")
} else {
  cat("There is a noticeable difference. Check convergence.\n")
}
cat("\nConvergence status (0 = success):", opt$convergence, "\n")
cat("Number of iterations:", opt$iterations, "\n")
