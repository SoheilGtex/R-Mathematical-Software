train_data <- read.csv("Crabs.csv")

names(train_data) <- c("C", "S", "W", "Wt", "Sa")


head(train_data)
str(train_data)



train_data$C2 <- ifelse(train_data$C == 2, 1, 0)
train_data$C3 <- ifelse(train_data$C == 3, 1, 0)
train_data$C4 <- ifelse(train_data$C == 4, 1, 0)


train_data$S2 <- ifelse(train_data$S == 2, 1, 0)
train_data$S3 <- ifelse(train_data$S == 3, 1, 0)


nloglik_loglinear <- function(theta) {
  

  b0 <- theta[1]
  b1 <- theta[2]
  b2 <- theta[3]
  b3 <- theta[4]
  b4 <- theta[5]
  b5 <- theta[6]
  b6 <- theta[7]
  b7 <- theta[8]
  
  n <- nrow(train_data)
  loglik <- 0   
  
  
  for (i in 1:n) {
    
    
    linear_predictor <- b0 +
      b1 * train_data$C2[i] +
      b2 * train_data$C3[i] +
      b3 * train_data$C4[i] +
      b4 * train_data$S2[i] +
      b5 * train_data$S3[i] +
      b6 * train_data$W[i]  +
      b7 * train_data$Wt[i]
    
    
    lambda <- exp(linear_predictor)
    
  
    p_i <- dpois(train_data$Sa[i], lambda)
    
     
    loglik <- loglik + log(p_i)
  }
  
  
  return(-loglik)
}

 
theta0 <- c(log(mean(train_data$Sa)), 0, 0, 0, 0, 0, 0, 0)


fit_mle <- nlminb(start     = theta0,
                  objective = nloglik_loglinear,
                  lower     = -Inf,
                  upper     =  Inf)


mle_coef <- fit_mle$par
names(mle_coef) <- c("(Intercept)", "C2", "C3", "C4", "S2", "S3", "W", "Wt")

cat("\n--- ضرایب به دست آمده از روش MLE (nlminb) ---\n")
print(round(mle_coef, 6))


glm_fit <- glm(Sa ~ factor(C) + factor(S) + W + Wt,
               data   = train_data,
               family = poisson(link = "log"))

glm_coef <- coef(glm_fit)

cat("\n--- ضرایب به دست آمده از glm ---\n")
print(round(glm_coef, 6))


comparison <- data.frame(
  MLE_nlminb = round(as.numeric(mle_coef), 6),
  glm        = round(as.numeric(glm_coef), 6),
  row.names  = names(mle_coef)
)
comparison$difference <- round(comparison$MLE_nlminb - comparison$glm, 8)

cat("\n--- مقایسه‌ی ضرایب دو روش ---\n")
print(comparison)
cat("\nهمگرا شد؟ (۰ یعنی موفق):", fit_mle$convergence, "\n")
