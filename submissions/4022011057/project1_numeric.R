library(readxl)

dat <- read_xlsx("assignments/mathematical-software/Crabs.xlsx")
dat <- as.data.frame(dat)
dat <- na.omit(dat)

dat$C <- factor(dat$C)
dat$S <- factor(dat$S)

set.seed(42)
idx   <- sample(nrow(dat), floor(0.8 * nrow(dat)))
train <- dat[idx, ]
test  <- dat[-idx, ]

c2 <- ifelse(train$C == 2, 1, 0)
c3 <- ifelse(train$C == 3, 1, 0)
c4 <- ifelse(train$C == 4, 1, 0)
s2 <- ifelse(train$S == 2, 1, 0)
s3 <- ifelse(train$S == 3, 1, 0)

nloglik_loglinear <- function(par) {
  llik <- 0
  for (i in 1:nrow(train)) {
    xb   <- par[1] + par[2]*c2[i] + par[3]*c3[i] + par[4]*c4[i] +
                     par[5]*s2[i] + par[6]*s3[i] +
                     par[7]*train$W[i] + par[8]*train$Wt[i]
    mu   <- exp(xb)
    llik <- llik + log(dpois(train$Sa[i], mu))
  }
  return(-llik)
}

par0 <- c(log(mean(train$Sa)), rep(0, 7))

fit <- nlminb(par0, nloglik_loglinear,
              lower = rep(-Inf, 8),
              upper = rep( Inf, 8))

m <- glm(Sa ~ C + S + W + Wt,
         data   = train,
         family = poisson(link = "log"))

fit$par
coef(m)