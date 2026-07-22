crabs <- read.csv("Crabs.csv")

crabs$C2 <- ifelse(crabs$C == 2, 1, 0)
crabs$C3 <- ifelse(crabs$C == 3, 1, 0)
crabs$C4 <- ifelse(crabs$C == 4, 1, 0)
crabs$S2 <- ifelse(crabs$S == 2, 1, 0)
crabs$S3 <- ifelse(crabs$S == 3, 1, 0)

neg_ll <- function(b) {
  lam <- exp(b[1] + b[2]*crabs$C2 + b[3]*crabs$C3 + b[4]*crabs$C4 +
             b[5]*crabs$S2 + b[6]*crabs$S3 + b[7]*crabs$W + b[8]*crabs$Wt)
  -sum(dpois(crabs$Sa, lam, log = TRUE))
}

init_b <- c(log(mean(crabs$Sa)), 0, 0, 0, 0, 0, 0, 0)
opt_result <- nlminb(start = init_b, objective = neg_ll)

names(opt_result$par) <- c("b0","bC2","bC3","bC4","bS2","bS3","bW","bWt")
print(round(opt_result$par, 4))

# مقایسه و چک کردن با glm
fit_glm <- glm(Sa ~ C2 + C3 + C4 + S2 + S3 + W + Wt,
               data = crabs, family = poisson)
print(round(coef(fit_glm), 4))
