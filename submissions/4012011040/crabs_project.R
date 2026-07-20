install.packages("readxl")
library(readxl)
df = as.data.frame(read_xlsx("Crabs.xlsx"))

df$C = factor(df$C)
df$S = factor(df$S)

set.seed(42)
n <- nrow(df)
train_idx <- sample(1:n , floor(0.8*n))
train_df <- df[train_idx, ]
test_df <- df[-train_idx, ]

model <- glm(Sa~C+S+W+Wt , family = poisson(link = "log"), data = train_df)

C2 <- ifelse(train_df$C==2,1,0)
C3 <- ifelse(train_df$C==3,1,0)
C4 <- ifelse(train_df$C==4,1,0)
S2 <- ifelse(train_df$S==2,1,0)
S3 <- ifelse(train_df$S==3,1,0)
W  <- train_df$W
Wt <- train_df$Wt
Sa <- train_df$Sa



nloglik_loglinear <- function(theta){
  
  beta0 <- theta[1]
  beta1 <- theta[2]
  beta2 <- theta[3]
  beta3 <- theta[4]
  beta4 <- theta[5]
  beta5 <- theta[6]
  beta6 <- theta[7]
  beta7 <- theta[8]
  
  n_train <- nrow(train_df)
  lik <- numeric(n_train)

  
  for(i in 1:n_train){
    
    lp <- beta0 +
      beta1*C2[i] +
      beta2*C3[i] +
      beta3*C4[i] +
      beta4*S2[i] +
      beta5*S3[i] +
      beta6*W[i]  +
      beta7*Wt[i]
    
    lambda <- exp(lp)
    
    lik[i] <- dpois(Sa[i],lambda,log = TRUE)
  }
  
  likelihood <- sum(lik)
  
  return(-likelihood)
}

beta0_0 <- log(mean(Sa))
theta0 <- c(beta0_0,0,0,0,0,0,0,0)

fit_mle <- nlminb(
  theta0,
  nloglik_loglinear,
  lower = rep(-Inf,8),
  upper = rep( Inf,8)
)

fit_mle$par
coef(model)