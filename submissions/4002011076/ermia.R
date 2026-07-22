install.packages("readxl")
library(readxl)
df = as.data.frame(read_xlsx("Crabs.xlsx"))

set.seed(42)
n <- nrow(df)
Train_idx <- sample(1:n , floor(0.8*n))
Train_df <- df[Train_idx, ]
Test_df <- df[-Train_idx, ]

C2 <- ifelse(Train_df$C==2,1,0)
C3 <- ifelse(Train_df$C==3,1,0)
C4 <- ifelse(Train_df$C==4,1,0)
S2 <- ifelse(Train_df$S==2,1,0)
S3 <- ifelse(Train_df$S==3,1,0)
W  <- Train_df$W
Wt <- Train_df$Wt
Sa <- Train_df$Sa

nloglik_loglinear <- function(theta){
  
  beta0 <- theta[1]
  beta1 <- theta[2]
  beta2 <- theta[3]
  beta3 <- theta[4]
  beta4 <- theta[5]
  beta5 <- theta[6]
  beta6 <- theta[7]
  beta7 <- theta[8]
  
  n_Train <- nrow(Train_df)
  lik <- numeric(n_Train)
  
  for(i in 1:n_Train){
    
    linear_predictor <- beta0 +
      beta1*C2[i] +
      beta2*C3[i] +
      beta3*C4[i] +
      beta4*S2[i] +
      beta5*S3[i] +
      beta6*W[i]  +
      beta7*Wt[i]
    
    lambda <- exp(linear_predictor)
    
    lik[i] <- dpois(Sa[i],lambda)
  }
  
  likelihood <- prod(lik)
  return(-log(likelihood))
}

beta0_0 <- log(mean(Sa))
theta0 <- c(beta0_0,0,0,0,0,0,0,0)

fit_mle <- nlminb(
  theta0,
  nloglik_loglinear,
  lower = rep(-Inf,8),
  upper = rep( Inf,8)
)
