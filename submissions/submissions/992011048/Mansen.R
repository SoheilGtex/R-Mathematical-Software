#step 1
install.packages("readxl")
library(readxl)
df = as.data.frame(read_xlsx("Crabs.xlsx"))

set.seed(42)
n <- nrow(df)
train_idx <- sample(1:n , floor(0.8*n))
train_data <- df[train_idx, ]
test_data <- df[-train_idx, ]

C2 <- ifelse(train_data$C==2,1,0)
C3 <- ifelse(train_data$C==3,1,0)
C4 <- ifelse(train_data$C==4,1,0)
S2 <- ifelse(train_data$S==2,1,0)
S3 <- ifelse(train_data$S==3,1,0)
W  <- train_data$W
Wt <- train_data$Wt
Sa <- train_data$Sa
n_train <- nrow(train_data)

#step2
nloglik_loglinear <- function(theta){
  
  beta0 <- theta[1]
  beta1 <- theta[2]
  beta2 <- theta[3]
  beta3 <- theta[4]
  beta4 <- theta[5]
  beta5 <- theta[6]
  beta6 <- theta[7]
  beta7 <- theta[8]
  
  lik <- numeric(n_train)
  
  for(i in 1:n_train){
    
    linear_predictor <- beta0 +
      beta1*C2[i] +
      beta2*C3[i] +
      beta3*C4[i] +
      beta4*S2[i] +
      beta5*S3[i] +
      beta6*W[i]  +
      beta7*Wt[i]
    
    landa <- exp(linear_predictor)
    
    lik[i] <- dpois(Sa[i],landa)
  }
  
  likelihood <- prod(lik)
  return(-log(likelihood))
}

#step3
beta0 <- log(mean(Sa))
theta0 <- c(beta0,0,0,0,0,0,0,0)

fit_mle <- nlminb(
  theta0,
  nloglik_loglinear,
  lower = rep(-Inf,8),
  upper = rep( Inf,8)
)

