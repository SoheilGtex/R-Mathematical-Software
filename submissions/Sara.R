library(readxl)

crab <- read_xlsx("assignments/mathematical-software/Crabs.xlsx")
crab <- na.omit(crab)

crab$C <- factor(crab$C)
crab$S <- factor(crab$S)

set.seed(42)

train_id <- sample(
  seq_len(nrow(crab)),
  size = 0.8 * nrow(crab)
)

train <- crab[train_id, ]

Z <- model.matrix(
  Sa ~ C + S + W + Wt,
  data = train
)

Sa <- train$Sa

mle <- function(b) {
  lambda <- exp(Z %*% b)
  lambda[lambda < 1e-10] <- 1e-10
  return(-sum(dpois(Sa, lambda, log = TRUE)))
}

b0 <- rep(0, ncol(Z))

answer <- optim(
  b0,
  mle,
  method = "BFGS",
  control = list(maxit = 1000)
)

mle_result <- answer$par

glm_model <- glm(
  Sa ~ C + S + W + Wt,
  data = train,
  family = poisson(link = "log")
)

names(mle_result) <- colnames(Z)

print(mle_result)
print(coef(glm_model))
print(mle_result - coef(glm_model))
