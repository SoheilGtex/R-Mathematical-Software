#خواندن داده
library(readxl)
crabs <- read_excel("Crabs.xlsx")

#متغير هاي مجازي (دامي) براي رنگ
crabs$C2 <- ifelse(crabs$C == 2, 1, 0)
crabs$C3 <- ifelse(crabs$C == 3, 1, 0)
crabs$C4 <- ifelse(crabs$C == 4, 1, 0)

#متغير هاي مجازي (دامي) براي ستون فقرات
crabs$S2 <- ifelse(crabs$S == 2, 1, 0)
crabs$S3 <-  ifelse(crabs$S == 3, 1, 0)


#نوشتن تابع درستنمايي
nloglik_loglinear <- function(theta) {
beta0 <- theta[1]
beta1 <- theta[2]
beta2 <- theta[3]
beta3 <- theta[4]
beta4 <- theta[5]
beta5 <- theta[6]
beta6 <- theta[7]
beta7 <- theta[8]


loglik <- 0

  
for(i in 1:nrow(crabs)){

linear_predictor <-
  beta0 +
  beta1*crabs$C2[i] +
  beta2*crabs$C3[i] +
  beta3*crabs$C4[i] +
  beta4*crabs$S2[i] +
  beta5*crabs$S3[i] +
  beta6*crabs$W[i] +
  beta7*crabs$Wt[i]



#ميانگين پواسون و احتمال
lambda <- exp(linear_predictor)
loglik <- loglik + dpois(crabs$Sa[i], lambda = lambda, log = TRUE)
}

#برگرداندن منفي لگاريتم درستنمايي
return(-loglik)
}


#حدس اوليه
theta0 <- c(log(mean(crabs$Sa)), 0, 0, 0, 0, 0, 0, 0)
#نیاز  نیست بازه رو برای پارامتر ها استفاده کنیم به طور پیش فرض همین اتفاق میفته ولی چون ذکر شده نوشتم
fit <- nlminb(start = theta0, objective = nloglik_loglinear, lower = rep(-Inf,8), upper = rep(Inf, 8))
print(fit$par)




#مقايسه 
model_glm <- glm(Sa ~ C2 + C3 + C4 + S2 + S3 + W + Wt,
                 data = crabs,
                 family = poisson(link = "log"))

print(coef(model_glm))

comparison <- cbind(MLE_nlminb = fit$par, GLM = coef(model_glm))

print(comparison)

#جواب پرسش:
#ضرایب برآورده شده توسط این دو تقریبا یکسان هستند و اگر تفاوت ناچیزی در حد اعشاری دارند به دلیل اختلاف ناشی از تفاوت آن ها در الگوریتم های بهینه سازی و تقریب های عددی و معیار توقف است.

