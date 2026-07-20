library(readxl)
data_path <- "Crabs.xlsx"
crab_d <- read_xlsx(data_path)

crab_d$C <- factor(crab_d$C)
crab_d$S <- factor(crab_d$S)

crab_d$C2 <- ifelse(crab_d$C == 2, 1, 0)
crab_d$C3 <- ifelse(crab_d$C == 3, 1, 0)
crab_d$C4 <- ifelse(crab_d$C == 4, 1, 0)
head(crab_d[, c("C", "C2", "C3", "C4")])

crab_d$S2 <- ifelse(crab_d$S == 2, 1, 0)
crab_d$S3 <- ifelse(crab_d$S == 3, 1, 0)
head(crab_d[, c("S", "S2", "S3")])

nloglik_loglinear <- function(theta, data) {
  beta0 <- theta[1]
  beta1 <- theta[2]  # C2
  beta2 <- theta[3]  # C3
  beta3 <- theta[4]  # C4
  beta4 <- theta[5]  # S2
  beta5 <- theta[6]  # S3
  beta6 <- theta[7]  # W
  beta7 <- theta[8]  # Wt

  eta <- beta0 + beta1 * data$C2 + beta2 * data$C3 +
    beta3 * data$C4 +
    beta4 * data$S2 +
    beta5 * data$S3 +
    beta6 * data$W +
    beta7 * data$Wt

  lambda <- exp(eta)
  log_likelihoods <- dpois(x = data$Sa, lambda = lambda, log = TRUE)
  neg_log_likelihoods <- -sum(log_likelihoods)

  return(neg_log_likelihoods)
}

initial_tehta <- rep(0, 8)
initial_null <- nloglik_loglinear(theta = initial_tehta, data = crab_d)
beta0_start <- log(mean(crab_d$Sa))
another_beta_start <- rep(0, 7)
theta0 <- c(beta0_start, other_beta_start)
print(theta0)

optimize_result <- nlminb(start = theta0, objective = nloglik_loglinear,
                          data = crab_d, lower = -Inf, upper = Inf)
print(optimize_result)

model_glm <- glm(crab_d$Sa ~ crab_d$C + crab_d$S + crab_d$W +
                   crab_d$Wt, data = train_data, family = poisson(link = "log"))

summary(model_glm)
