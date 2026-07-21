library(readxl)

crab <- read_xlsx("assignments/mathematical-software/Crabs.xlsx")

crab <- as.data.frame(crab)
crab <- na.omit(crab)

crab$C <- factor(crab$C)
crab$S <- factor(crab$S)


set.seed(42)

n <- nrow(crab)
train_index <- sample(
  1:n,
  floor(0.8*n)
)

train_set <- crab[train_index, ]
test_set <- crab[-train_index, ]


col2 <- ifelse(train_set$C == 2,1,0)
col3 <- ifelse(train_set$C == 3,1,0)
col4 <- ifelse(train_set$C == 4,1,0)

sp2 <- ifelse(train_set$S == 2,1,0)
sp3 <- ifelse(train_set$S == 3,1,0)



log_function <- function(theta){

  sum_value <- 0

  for(i in 1:nrow(train_set)){

    eta <- theta[1] +
      theta[2]*col2[i] +
      theta[3]*col3[i] +
      theta[4]*col4[i] +
      theta[5]*sp2[i] +
      theta[6]*sp3[i] +
      theta[7]*train_set$W[i] +
      theta[8]*train_set$Wt[i]


    lambda <- exp(eta)

    sum_value <- sum_value +
      log(dpois(train_set$Sa[i], lambda))

  }

  return(-sum_value)

}



theta_start <- c(
  log(mean(train_set$Sa)),
  rep(0,7)
)



result <- nlminb(
  theta_start,
  log_function,
  lower = rep(-Inf,8),
  upper = rep(Inf,8)
)



glm_result <- glm(
  Sa ~ C + S + W + Wt,
  data = train_set,
  family = poisson(link="log")
)


result$par
coef(glm_result)