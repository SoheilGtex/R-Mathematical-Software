library(readxl)
df = as.data.frame(read_xlsx("Crabs.xlsx"))

C = factor(df$C)
S = factor(df$S)

C2 = ifelse(df$C == 2, 1, 0)
C3 = ifelse(df$C == 3, 1, 0)
C4 = ifelse(df$C == 4, 1, 0)

S2 = ifelse(df$S== 2, 1, 0)
S3 = ifelse(df$S == 3, 1, 0)

df <- cbind(df,  C2, C3, C4, S2, S3)

set.seed(42)
n <- nrow(df)
train_idx <- sample(1:n, size = floor(0.8 * n))
train_data <- df[train_idx, ]
test_data <- df[-train_idx]

C = train_data$C
S = train_data$S
W = train_data$W 
Wt = train_data$Wt
Sa = train_data$Sa
C2 = train_data$C2
C3 = train_data$C3
C4 = train_data$C4
S2 = train_data$S2
S3 = train_data$S3


n_train <- nrow(train_data)

nloglik_loglinear <- function(theta){
  beta0 <- theta[1]
  beta1 <- theta[2]
  beta2 <- theta[3]
  beta3 <- theta[4]
  beta4 <- theta[5]
  beta5 <- theta[6]
  beta6 <- theta[7]
  beta7 <- theta[8]
  
  etha <- numeric(n_train)
  lambda <- numeric(n_train)
  p <- numeric(n_train)
  
  for(i in 1:n_train){
    etha[i] = beta0 + beta1 * C2[i] + beta2 * C3[i] + beta3 * C4[i] + beta4 * S2[i] + beta5 * S3[i] + beta6 * W[i] + beta7 * Wt[i]
    lambda[i] = exp(etha[i])
    p[i] = dpois(Sa[i] ,lambda[i])
  }
  
  lik <- prod(p)
  return(-log(lik))
  
}

theta0 <- c(log(mean(Sa)), 0, 0, 0, 0, 0, 0, 0)


fit_mle <- nlminb(theta0,
                  nloglik_loglinear,
                  lower = rep(-Inf, 8),
                  upper = rep(Inf, 8)
                  )

print(fit_mle$par)

fit_glm <- glm(Sa ~ C + S + W + Wt, data = train_data, family = poisson(link = "log"))
