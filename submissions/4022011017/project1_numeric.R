library(readxl)

crabs <- read_excel("mathematical-software/Crabs.xlsx")
#create dummy variables for color
crabs$C2 <- ifelse(crabs$C == 2, 1, 0)
crabs$C3 <- ifelse(crabs$C == 3, 1, 0)
crabs$C4 <- ifelse(crabs$C == 4, 1, 0)
#create dummy variables for spine
crabs$S2 <- ifelse(crabs$S == 2, 1, 0)
crabs$S3 <- ifelse(crabs$S == 3, 1, 0)
#compute neg log
nloglik_loglinear <- function(theta) { 
  likelihood <- 1
  #loop thru each
  for (i in 1:nrow(crabs)) {
    #named linear_predictor bcs of pdf
    linear_predictor <- theta[1] +
      theta[2] * crabs$C2[i] +
      theta[3] * crabs$C3[i] +
      theta[4] * crabs$C4[i] +
      theta[5] * crabs$S2[i] +
      theta[6] * crabs$S3[i] +
      theta[7] * crabs$W[i] +
      theta[8] * crabs$Wt[i]
    lambda <- exp(linear_predictor) 
    prob <- dpois(
      x = crabs$Sa[i],
      lambda = lambda,
      log = FALSE
    )
    likelihood <- likelihood * prob
  }
  return(-log(likelihood))
}
#initial val for opt
theta0 <- c(
  log(mean(crabs$Sa)),
  0, 0, 0, 0, 0, 0, 0
)
#estimate using max likelihood
fit<- nlminb(
  start = theta0,
  objective = nloglik_loglinear
)

fit$par
fit$objective

glm_fitted <- glm(
  Sa ~ C2 + C3 + C4 + S2 + S3 + W + Wt,
  data = crabs,
  family = poisson(link = "log")
)

coef(glm_fitted)
summary(glm_fitted)
comparison <- data.frame(
  MLE = fit$par,
  GLM = coef(glm_fitted)
)

comparison
