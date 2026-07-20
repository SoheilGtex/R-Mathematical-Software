library(readxl)
data_path <- "Crabs.xlsx"
crb_data <- read_xlsx(data_path)

crb_data$C <- factor(crb_data$C)
crb_data$S <- factor(crb_data$S)

#  Train/Test
set.seed(53)
n <- nrow(crb_data)
train_idx <- sample(1:n, floor(0.8 * n))
train_data <- crb_data[train_idx, ]
test_data <- crb_data[-train_idx, ]

crb_data$C2 <- ifelse(crb_data$C == 2, 1, 0)
crb_data$C3 <- ifelse(crb_data$C == 3, 1, 0)
crb_data$C4 <- ifelse(crb_data$C == 4, 1, 0)
head(crb_data[, c("C", "C2", "C3", "C4")])

crb_data$S2 <- ifelse(crb_data$S == 2, 1, 0)
crb_data$S3 <- ifelse(crb_data$S == 3, 1, 0)
head(crb_data[, c("S", "S2", "S3")])

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
initial_null <- nloglik_loglinear(theta = initial_tehta, data = crb_data)
beta0_start <- log(mean(crb_data$Sa))
another_beta_start <- rep(0, 7)
theta0 <- c(beta0_start, other_beta_start)
print(theta0)

optimize_result <- nlminb(start = theta0, objective = nloglik_loglinear,
                          data = crb_data, lower = -Inf, upper = Inf)
print(optimize_result)

model_glm <- glm(crb_data$Sa ~ crb_data$C + crb_data$S + crb_data$W+
 crb_data$Wt, data = train_data, family = poisson(link = 'log'))

summary(model_glm)
