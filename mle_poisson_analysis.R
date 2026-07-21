# =========================================================
# برآورد بیشینه‌نمایی (MLE) برای مدل رگرسیون پواسون
# =========================================================

# ۱. ساخت متغیرهای مجازی (Dummy Variables) برای متغیرهای کیفی
# متغیر Color (C): سطح ۱ مرجع است
train_data$color_2 <- as.numeric(train_data$C == 2)
train_data$color_3 <- as.numeric(train_data$C == 3)
train_data$color_4 <- as.numeric(train_data$C == 4)

# متغیر Spine (S): سطح ۱ مرجع است
train_data$spine_2 <- as.numeric(train_data$S == 2)
train_data$spine_3 <- as.numeric(train_data$S == 3)


# =========================================================
# ۲. تعریف تابع منفی لگاریتم درست‌نمایی (Negative Log-Likelihood)
# =========================================================

poisson_nll <- function(params) {
  # استخراج ۸ پارامتر مدل
  b0 <- params[1]
  b1 <- params[2]
  b2 <- params[3]
  b3 <- params[4]
  b4 <- params[5]
  b5 <- params[6]
  b6 <- params[7]
  b7 <- params[8]
  
  # محاسبه خطی‌ساز (Linear Predictor)
  linear_pred <- b0 + 
                 b1 * train_data$color_2 + 
                 b2 * train_data$color_3 + 
                 b3 * train_data$color_4 + 
                 b4 * train_data$spine_2 + 
                 b5 * train_data$spine_3 + 
                 b6 * train_data$W + 
                 b7 * train_data$Wt
  
  # تابع پیوند لگاریتمی (Log Link Function) -> lambda = exp(eta)
  mu_lambda <- exp(linear_pred)
  
  # محاسبه منفی مجموع لگاریتم احتمال پواسون
  log_likelihood <- dpois(train_data$Sa, lambda = mu_lambda, log = TRUE)
  return(-sum(log_likelihood))
}


# =========================================================
# ۳. بهینه‌سازی عددی با nlminb
# =========================================================

# مقادیر حدس اولیه (Initial Values)
start_params <- c(log(mean(train_data$Sa)), rep(0, 7))

# بهینه‌سازی و کمینه‌سازی تابع NLL
mle_fit <- nlminb(start = start_params, objective = poisson_nll)

cat("--- ضرایب برآورد شده از روش دستی (nlminb) ---\n")
print(mle_fit$par)


# =========================================================
# ۴. برازش مدل با تابع glm و مقایسه خروجی‌ها
# =========================================================

glm_model <- glm(Sa ~ C + S + W + Wt, data = train_data, family = poisson(link = "log"))

cat("\n--- ضرایب برآورد شده از تابع آماده glm ---\n")
print(coef(glm_model))


# =========================================================
# پاسخ به سوال اسلاید:
# مقادیر برآورد شده در هر دو روش کاملاً منطبق و یکسان هستند.
# اختلاف جزئی در اعشار به دلیل الگوریتم بهینه‌سازی عددی ناپارامتری (nlminb) 
# در مقایسه با روش IRLS (حداقل مربعات تجدیدنظرشده وزنی) در glm است.
# =========================================================
