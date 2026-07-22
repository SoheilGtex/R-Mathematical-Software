
if (!require(readxl)) {
  install.packages("readxl")
}
library(readxl)

data <- read_excel("C:/Users/Lenovo/Downloads/crabs.xlsx")
head(data)
colnames(data) <- c("C", "S", "W", "Wt", "Sa")
str(data)
data$C <- as.numeric(data$C)
data$S <- as.numeric(data$S)
data$W <- as.numeric(data$W)
data$Wt <- as.numeric(data$Wt)
data$Sa <- as.numeric(data$Sa)


data <- na.omit(data)

data$C2 <- ifelse(data$C == 2, 1, 0)
data$C3 <- ifelse(data$C == 3, 1, 0)
data$C4 <- ifelse(data$C == 4, 1, 0)
data$S2 <- ifelse(data$S == 2, 1, 0)
data$S3 <- ifelse(data$S == 3, 1, 0)

nloglik_loglinear <- function(theta, Y, C2, C3, C4, S2, S3, W, Wt) {
  beta0 <- theta[1]
  beta1 <- theta[2]
  beta2 <- theta[3]
  beta3 <- theta[4]
  beta4 <- theta[5]
  beta5 <- theta[6]
  beta6 <- theta[7]
  beta7 <- theta[8]
  
  eta <- beta0 + beta1*C2 + beta2*C3 + beta3*C4 + 
    beta4*S2 + beta5*S3 + beta6*W + beta7*Wt
  
  lambda <- exp(eta)
  
  log_lik <- sum(dpois(Y, lambda, log = TRUE))
  return(-log_lik)
}


theta_init <- c(log(mean(data$Sa)), 0, 0, 0, 0, 0, 0, 0)


result_nlm <- nlm(
  f = nloglik_loglinear,
  p = theta_init,
  Y = data$Sa,
  C2 = data$C2,
  C3 = data$C3,
  C4 = data$C4,
  S2 = data$S2,
  S3 = data$S3,
  W = data$W,
  Wt = data$Wt
)


print(result_nlm)


coef_nlm <- result_nlm$estimate
names(coef_nlm) <- c("beta0", "beta1", "beta2", "beta3", 
                     "beta4", "beta5", "beta6", "beta7")
print(coef_nlm)


model_glm <- glm(Sa ~ factor(C) + factor(S) + W + Wt, 
                 data = data, 
                 family = poisson(link = "log"))


summary(model_glm)


coef_glm <- coef(model_glm)

comparison <- data.frame(
  Parameter = c("Intercept", "C2", "C3", "C4", "S2", "S3", "W", "Wt"),
  nlm = coef_nlm,
  glm = coef_glm,
  Difference = coef_nlm - coef_glm
)

print(comparison)

cat("\nبیشترین تفاوت مطلق:", max(abs(comparison$Difference)), "\n")


#آیا ضرایب دقیقاً یکی هستند؟ اگر تفاوت ناچیزی دارند، علت چیست؟

پاسخ#:
#  ضرایب کاملاً یکسان نیستن و تفاوت‌های بسیار جزئی (در حد ۱۰ به توان منفی ۶ یا کمتر) دارند.

دلیل#:
  
#  1. الگوریتم‌های عددی متفاوت: glm از الگوریتم IWLS (Iteratively Weighted Least Squares) استفاده میکنه، در حالی که nlm از روش Newton-Type استفاده میکنه.
#2. معیار توقف (Convergence Criteria) متفاوت: هر الگوریتم با تلورانس‌های متفاوتی متوقف میشه.
#3. مقدار اولیه متفاوت: glm از یک روش ابتکاری برای پیدا کردن مقدار اولیه بهتر استفاده میکنه.

#تیجه: هر دو روش به همان نقطه بهینه سراسری میرن، ولی به خاطر تفاوت‌های عددی، ضرایب تا چند رقم اعشار متفاوت هستن. این تفاوت‌ها قابل چشم‌پوشی هستن و تأثیری در پیش‌بینی ندارن.

