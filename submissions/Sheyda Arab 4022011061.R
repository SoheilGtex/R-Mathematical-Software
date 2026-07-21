library(readxl)

train_data <- read_excel("Crabs.xlsx")

train_data$C2 <- ifelse(train_data$C == 2, 1, 0)
train_data$C3 <- ifelse(train_data$C == 3, 1, 0)
train_data$C4 <- ifelse(train_data$C == 4, 1, 0)

train_data$S2 <- ifelse(train_data$S == 2, 1, 0)
train_data$S3 <- ifelse(train_data$S == 3, 1, 0)

head(train_data[, c("C", "C2", "C3", "C4", "S", "S2", "S3")])

nloglik_loglinear <- function(theta, data) {

  b0 <- theta[1]
  b1 <- theta[2]
  b2 <- theta[3]
  b3 <- theta[4]
  b4 <- theta[5]
  b5 <- theta[6]
  b6 <- theta[7]
  b7 <- theta[8]

  n <- nrow(data)
  loglik_i <- numeric(n)

  for (i in 1:n) {

    linear_predictor <- b0 +
      b1 * data$C2[i] +
      b2 * data$C3[i] +
      b3 * data$C4[i] +
      b4 * data$S2[i] +
      b5 * data$S3[i] +
      b6 * data$W[i]  +
      b7 * data$Wt[i]

    lambda_i <- exp(linear_predictor)

    prob_i <- dpois(data$Sa[i], lambda_i)

    loglik_i[i] <- log(prob_i)
  }

  total_loglik <- sum(loglik_i)
  return(-total_loglik)
}

theta0 <- c(
  b0 = log(mean(train_data$Sa)),
  b1 = 0,
  b2 = 0,
  b3 = 0,
  b4 = 0,
  b5 = 0,
  b6 = 0,
  b7 = 0
)

fit <- nlminb(
  start = theta0,
  objective = nloglik_loglinear,
  data = train_data,
  lower = rep(-Inf, 8),
  upper = rep(Inf, 8)
)

print(fit)

coef_nlminb <- fit$par
names(coef_nlminb) <- c("b0", "b1", "b2", "b3", "b4", "b5", "b6", "b7")
print(coef_nlminb)

fit_glm <- glm(Sa ~ C + S + W + Wt, data = train_data, family = poisson(link = "log"))
summary(fit_glm)

coef_glm <- coef(fit_glm)
print(coef_glm)

comparison <- data.frame(
  parameter = c("Intercept", "C2", "C3", "C4", "S2", "S3", "W", "Wt"),
  nlminb = as.numeric(coef_nlminb),
  glm    = as.numeric(coef_glm)
)
print(comparison)

# پاسخ به سؤال گام ۴:
# ضرایب nlminb و glm تقریباً یکسان‌اند اما دقیقاً برابر نیستند (تفاوت در حد
# رقم چهارم یا پنجم اعشار). هر دو روش همان تابع درست‌نمایی پواسون را
# بیشینه می‌کنند و از نظر تئوری باید به یک نقطه برسند؛ تفاوت جزئی ناشی از
# الگوریتم عددی متفاوت است (glm از IRLS استفاده می‌کند، nlminb از یک
# بهینه‌ساز عمومی quasi-Newton)، معیار همگرایی متفاوت هر الگوریتم، و
# انباشت خطاهای رند در محاسبات عددی (floating point).
