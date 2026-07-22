install.packages("readxl")

R.version.string

install.packages("dplyr")


getwd()
list.files()

library(readxl)
library(dplyr)

crabs <- read_excel("Crabs.xlsx")

head(crabs)

str(crabs)

summary(crabs)

model <- glm(Sa ~ W + Wt + C + S,
             family = poisson(link = "log"),
             data = crabs)

summary(model)

#-------------------------------------------------
# step 1
train_data <- crabs
train_data$C2 <- ifelse(train_data$C == 2, 1, 0)
train_data$C3 <- ifelse(train_data$C == 3, 1, 0)
train_data$C4 <- ifelse(train_data$C == 4, 1, 0)

train_data$S2 <- ifelse(train_data$S == 2, 1, 0)
train_data$S3 <- ifelse(train_data$S == 3, 1, 0)

head(train_data)

#-------------------------------------------------
# step 2
nloglik_loglinear <- function(theta){
  
  beta0 <- theta[1]
  beta1 <- theta[2]
  beta2 <- theta[3]
  beta3 <- theta[4]
  beta4 <- theta[5]
  beta5 <- theta[6]
  beta6 <- theta[7]
  beta7 <- theta[8]
  
  likelihood <- 1
  
  for(i in 1:nrow(train_data)){
    
    eta <-
      beta0 +
      beta1*train_data$C2[i] +
      beta2*train_data$C3[i] +
      beta3*train_data$C4[i] +
      beta4*train_data$S2[i] +
      beta5*train_data$S3[i] +
      beta6*train_data$W[i] +
      beta7*train_data$Wt[i]
    
    lambda <- exp(eta)
    
    likelihood <-
      likelihood *
      dpois(train_data$Sa[i],
            lambda = lambda)
    
  }
  
  return(-log(likelihood))
  
}
theta0 <- c(log(mean(train_data$Sa)), 0, 0, 0, 0, 0, 0, 0)
nloglik_loglinear(theta0)
#-------------------------------------------------
# step 3
result <- nlminb(  start = theta0,
                   objective = nloglik_loglinear,
                   lower = rep(-Inf, 8),
                   upper = rep(Inf, 8)
)

result

result$par


#-------------------------------------------------
# step 4
glm_model <- glm(  Sa ~ C2 + C3 + C4 + S2 + S3 + W + Wt,
                   family = poisson(link = "log"),
                   data = train_data
)

summary(glm_model)

coef(glm_model)


#-------------------------------------------------
# extra

# English:
# The parameter estimates obtained using the nlminb optimization function 
# are almost identical to those obtained from the glm function. The differences
# between the estimated coefficients are extremely small  
# (approximately (10^{-6})) and can be considered negligible.
# These minor differences are caused by numerical optimization procedures,
# floating-point arithmetic precision, convergence criteria, and 
# the optimization algorithms used by each method. The glm function estimates
# the parameters using the Iteratively Reweighted Least Squares (IRLS) 
# algorithm, while nlminb uses a general-purpose numerical optimization
# algorithm to maximize the likelihood function. Therefore, slight numerical 
# differences are expected, but both methods converge to essentially the same
# maximum likelihood estimates.

# finglish :
# zarayeb be dast amade az function nlimnb & glm taqriban yeksan
# hastan , faqat ekhtelaf na chizi dar hadde 10^(-6) moshahedeh mishe.
# reason in ekhtelaf ham tafavot dar ravesh-haye addie morede estefadeh baraye behineh sazi
# me-eyare algoritm, deqqat mhasebat aashari(Floating Point Precision) va
# nahve hamgarayi algoritmhast. pas in ekhtelat tabiei budeh.



#-------------------------------------------------

