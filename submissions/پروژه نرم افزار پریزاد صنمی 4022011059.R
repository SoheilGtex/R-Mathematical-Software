# =====================================================================
# برآورد پارامترهای مدل رگرسیون پواسون با بیشینه‌سازی تابع درست‌نمایی (MLE)
# داده: Horseshoe Crab | مدل: Sa ~ C + S + W + Wt
# =====================================================================

# ---- گام ۰: خواندن داده ----
train_data <- read.csv("train_data.csv")

# روش جایگزین: خواندن مستقیم از فایل اصلی اکسل (Crabs.xlsx) به‌جای CSV
# library(readxl)
# train_data <- read_excel("Crabs.xlsx")

str(train_data)
head(train_data)


# ---- گام ۱: کدگذاری متغیرهای کیفی (Dummy Coding) ----
# C چهار سطح دارد -> سه متغیر مجازی (سطح ۱ = مرجع)
train_data$C2 <- ifelse(train_data$C == 2, 1, 0)
train_data$C3 <- ifelse(train_data$C == 3, 1, 0)
train_data$C4 <- ifelse(train_data$C == 4, 1, 0)

# S سه سطح دارد -> دو متغیر مجازی (سطح ۱ = مرجع)
train_data$S2 <- ifelse(train_data$S == 2, 1, 0)
train_data$S3 <- ifelse(train_data$S == 3, 1, 0)

head(train_data[, c("C", "C2", "C3", "C4", "S", "S2", "S3", "W", "Wt", "Sa")])


# ---- گام ۲: تابع منفی لگاریتم درست‌نمایی ----
# مدل: log(lambda_i) = b0 + b1*C2 + b2*C3 + b3*C4 + b4*S2 + b5*S3 + b6*W + b7*Wt

nloglik_loglinear <- function(theta, data) {

  # تفکیک بردار theta به ۸ پارامتر مدل
  beta0 <- theta[1]
  beta1 <- theta[2]
  beta2 <- theta[3]
  beta3 <- theta[4]
  beta4 <- theta[5]
  beta5 <- theta[6]
  beta6 <- theta[7]
  beta7 <- theta[8]

  n <- nrow(data)
  loglik_total <- 0

  # برای هر مشاهده: پیش‌بین خطی -> lambda -> احتمال پواسون آن مشاهده
  for (i in 1:n) {

    linear_predictor <- beta0 +
      beta1 * data$C2[i] +
      beta2 * data$C3[i] +
      beta3 * data$C4[i] +
      beta4 * data$S2[i] +
      beta5 * data$S3[i] +
      beta6 * data$W[i]  +
      beta7 * data$Wt[i]

    lambda_i <- exp(linear_predictor)     # معکوس کردن لینک log
    p_i <- dpois(data$Sa[i], lambda_i)    # احتمال پواسون مشاهده i

    # جمع لگاریتم احتمالات به‌جای ضرب مستقیم (جلوگیری از Underflow عددی)
    loglik_total <- loglik_total + log(p_i)
  }

  return(-loglik_total)   # منفیِ درست‌نمایی، برای کمینه‌سازی با nlminb
}


# ---- گام ۳: بهینه‌سازی عددی با nlminb ----

# نقطه شروع: beta0 = log(میانگین Sa)، بقیه ضرایب صفر
theta0 <- c(
  log(mean(train_data$Sa)),
  0, 0, 0, 0, 0, 0, 0
)
cat("نقطه شروع (theta0):\n")
print(theta0)

fit_manual <- nlminb(
  start     = theta0,
  objective = nloglik_loglinear,
  data      = train_data,
  lower     = rep(-Inf, 8),
  upper     = rep(Inf, 8)
)

theta_hat <- fit_manual$par
names(theta_hat) <- c("beta0_Intercept", "beta1_C2", "beta2_C3", "beta3_C4",
                       "beta4_S2", "beta5_S3", "beta6_W", "beta7_Wt")

cat("\n=== ضرایب برآورد شده (nlminb) ===\n")
print(theta_hat)
cat("\nمنفی لگاریتم درست‌نمایی در نقطه بهینه:", fit_manual$objective, "\n")
cat("کد همگرایی (0 = موفق):", fit_manual$convergence, "-", fit_manual$message, "\n")


# ---- گام ۴: مقایسه با glm ----

fit_glm <- glm(
  Sa ~ C2 + C3 + C4 + S2 + S3 + W + Wt,
  data   = train_data,
  family = poisson(link = "log")
)

cat("\n=== خروجی glm ===\n")
print(summary(fit_glm))

comparison <- data.frame(
  Manual_MLE = round(as.numeric(theta_hat), 6),
  GLM        = round(as.numeric(coef(fit_glm)), 6)
)
rownames(comparison) <- names(theta_hat)
comparison$Difference <- round(comparison$Manual_MLE - comparison$GLM, 8)

cat("\n=== جدول مقایسه ضرایب دستی و glm ===\n")
print(comparison)

# ---------------------------------------------------------------------
# پاسخ به پرسش پایانی: آیا ضرایب nlminb و glm دقیقاً یکی هستند؟
# ---------------------------------------------------------------------
# دقیقاً یکی نیستند، ولی تفاوتشان در حد اعشارهای بسیار کوچک (مثلاً
# 1e-4 یا کمتر) است. سه دلیل اصلی این اختلاف جزئی:
#   ۱) الگوریتم متفاوت: glm از روش تخصصی IRLS (کمترین مربعات وزنی
#      تکرارشونده) استفاده می‌کند که برای مدل‌های خطی تعمیم‌یافته طراحی
#      شده، در حالی که nlminb یک بهینه‌ساز عددی عمومی (Quasi-Newton) است.
#   ۲) معیار توقف (Convergence Tolerance) پیش‌فرض این دو تابع یکسان نیست.
#   ۳) خطای گرد کردن اعداد اعشاری (floating-point rounding) در محاسبات
#      تکراری هر دو روش انباشته می‌شود.
# با افزایش دقت عددی بهینه‌سازی دستی، جواب nlminb به جواب glm نزدیک‌تر
# می‌شود؛ یعنی هر دو روش در حال حل همان مسئله MLE هستند، فقط با مسیر
# عددی متفاوت.
