library(readxl)

train_data <- read_excel("Crabs.xlsx")

train_data$C2 <- ifelse(train_data$C == 2, 1, 0)
train_data$C3 <- ifelse(train_data$C == 3, 1, 0)
train_data$C4 <- ifelse(train_data$C == 4, 1, 0)

train_data$S2 <- ifelse(train_data$S == 2, 1, 0)
train_data$S3 <- ifelse(train_data$S == 3, 1, 0)

train_data$C <- factor(train_data$C)
train_data$S <- factor(train_data$S)

nloglik <- function(theta){

  b0 <- theta[1]
  b1 <- theta[2]
  b2 <- theta[3]
  b3 <- theta[4]
  b4 <- theta[5]
  b5 <- theta[6]
  b6 <- theta[7]
  b7 <- theta[8]

  ll <- 0

  for(i in 1:nrow(train_data)){

    eta <- b0 + b1*train_data$C2[i] + b2*train_data$C3[i] +
      b3*train_data$C4[i] + b4*train_data$S2[i] +
      b5*train_data$S3[i] + b6*train_data$W[i] +
      b7*train_data$Wt[i]

    lambda <- exp(eta)

    ll <- ll + dpois(train_data$Sa[i],
                     lambda = lambda,
                     log = TRUE)
  }

  return(-ll)
}

theta0 <- c(log(mean(train_data$Sa)), 0, 0, 0, 0, 0, 0, 0)

fit <- nlminb(
  start = theta0,
  objective = nloglik
)


cat("Negative Log-Likelihood =", fit$objective, "\n")

coef_mle <- fit$par

names(coef_mle) <- c(
  "Intercept",
  "C2",
  "C3",
  "C4",
  "S2",
  "S3",
  "W",
  "Wt"
)

print(coef_mle)

fit_glm <- glm(
  Sa ~ C + S + W + Wt,
  data = train_data,
  family = poisson(link = "log")
)

print(coef(fit_glm))

comp <- data.frame(
  MLE = coef_mle,
  GLM = coef(fit_glm)
)

print(comp)
