train_data <- read.csv("Crabs_2.csv")

C  <- train_data$C
S  <- train_data$S
W  <- train_data$W
Wt <- train_data$Wt
Sa <- train_data$Sa

C2 <- ifelse(C == 2, 1, 0)
C3 <- ifelse(C == 3, 1, 0)
C4 <- ifelse(C == 4, 1, 0)

S2 <- ifelse(S == 2, 1, 0)
S3 <- ifelse(S == 3, 1, 0)

train_data$C2 <- C2
train_data$C3 <- C3
train_data$C4 <- C4
train_data$S2 <- S2
train_data$S3 <- S3

nloglik_loglinear <- function(theta) {

  beta0 <- theta[1]
  beta1 <- theta[2]
  beta2 <- theta[3]
  beta3 <- theta[4]
  beta4 <- theta[5]
  beta5 <- theta[6]
  beta6 <- theta[7]
  beta7 <- theta[8]

  n <- length(Sa)
  loglik <- 0

  for (i in 1:n) {

    linear_predictor <- beta0 +
      beta1 * C2[i] + beta2 * C3[i] + beta3 * C4[i] +
      beta4 * S2[i] + beta5 * S3[i] +
      beta6 * W[i]  + beta7 * Wt[i]

    lambda <- exp(linear_predictor)

    prob_i <- dpois(Sa[i], lambda)

    loglik <- loglik + log(prob_i)
  }

  return(-loglik)
}

theta0 <- c(log(mean(Sa)), 0, 0, 0, 0, 0, 0, 0)

fit_mle <- nlminb(
  start     = theta0,
  objective = nloglik_loglinear,
  lower     = rep(-Inf, 8),
  upper     = rep(Inf, 8)
)

cat("ط¶ط±ط§غŒط¨ ط¨ط±ط¢ظˆط±ط¯ ط´ط¯ظ‡ ط¨ط§ nlminb (MLE ط¯ط³طھغŒ):\n")
print(fit_mle$par)

fit_glm <- glm(Sa ~ C2 + C3 + C4 + S2 + S3 + W + Wt,
                data = train_data, family = poisson(link = "log"))

cat("\nط¶ط±ط§غŒط¨ ط¨ط±ط¢ظˆط±ط¯ ط´ط¯ظ‡ ط¨ط§ glm:\n")
print(coef(fit_glm))

cat("\nظ…ظ‚ط§غŒط³ظ‡â€ŒغŒ ط¯ظˆ ط±ظˆط´:\n")
comparison <- data.frame(
  parameter = c("beta0","beta1(C2)","beta2(C3)","beta3(C4)",
                "beta4(S2)","beta5(S3)","beta6(W)","beta7(Wt)"),
  nlminb    = fit_mle$par,
  glm       = coef(fit_glm)
)
print(comparison, row.names = FALSE)

## پرسش: آیا ضرایب دقیقاً یکی هستند؟
## پاسخ: تفاوت‌ها در حد اعشارهای بسیار کوچک (مثلاً ۱۰^-۴ به بعد) هستند
## و ناشی از دقت الگوریتم بهینه‌سازی عددی (nlminb) در مقابل الگوریتم
## دقیق‌تر IRLS (Iteratively Reweighted Least Squares) است که تابع glm
## به صورت داخلی و تحلیلی برای مدل‌های خطی تعمیم‌یافته استفاده می‌کند.
## هر دو روش در حال بیشینه‌سازی همان تابع درست‌نمایی هستند، فقط با
## الگوریتم‌های عددی متفاوت که به یک نقطه‌ی بهینه (تقریباً) یکسان همگرا می‌شوند.
