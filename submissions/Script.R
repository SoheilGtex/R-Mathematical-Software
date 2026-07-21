# =========================================================
# گام ۱: آماده‌سازی داده‌ها و کدگذاری متغیرهای کیفیم
# =========================================================

# ساخت متغیرهای مجازی برای رنگ (C) - سطح ۱ مرجع است
train_data$C2 <- ifelse(train_data$C == 2, 1, 0)
train_data$C3 <- ifelse(train_data$C == 3, 1, 0)
train_data$C4 <- ifelse(train_data$C == 4, 1, 0)

# ساخت متغیرهای مجازی برای وضعیت فقرات (S) - سطح ۱ مرجع است
train_data$S2 <- ifelse(train_data$S == 2, 1, 0)
train_data$S3 <- ifelse(train_data$S == 3, 1, 0)


# =========================================================
# گام ۲: تعریف تابع منفی لگاریتم درستنمایی (Negative Log-Likelihood)
# =========================================================

nloglik_loglinear <- function(theta) {
  # ۱. جداسازی ۸ پارامتر مدل (beta0 تا beta7)
  beta0 <- theta[1]
  beta1 <- theta[2]
  beta2 <- theta[3]
  beta3 <- theta[4]
  beta4 <- theta[5]
  beta5 <- theta[6]
  beta6 <- theta[7]
  beta7 <- theta[8]
  
  # ۲. محاسبه خطی‌ساز پیش‌بین (linear predictor) و احتمال برای هر مشاهده
  # محاسبه eta و lambda به صورت برداری برای کارایی بالاتر
  eta <- beta0 + 
         beta1 * train_data$C2 + 
         beta2 * train_data$C3 + 
         beta3 * train_data$C4 + 
         beta4 * train_data$S2 + 
         beta5 * train_data$S3 + 
         beta6 * train_data$W + 
         beta7 * train_data$Wt
  
  lambda <- exp(eta)
  
  # محاسبه لگاریتم احتمال پواسون برای هر مشاهده
  # dpois(..., log = TRUE) لگاریتم احتمال را مستقیماً محاسبه می‌کند
  log_lik <- dpois(train_data$Sa, lambda = lambda, log = TRUE)
  
  # ۶. مجموع لگاریتم درستنمایی ضرب‌در منفی یک (برای کمینه‌سازی)
  return(-sum(log_lik))
}


# =========================================================
# گام ۳: بهینه‌سازی عددی با nlminb
# =========================================================

# تعریف مقادیر اولیه برای پارامترها
# beta0 برابر با log(mean(Sa)) و بقیه صفر در نظر گرفته می‌شوند
theta0 <- c(log(mean(train_data$Sa)), rep(0, 7))

# کمینه‌سازی تابع negative log-likelihood
opt_res <- nlminb(start = theta0, objective = nloglik_loglinear)

# نمایش ضرایب به دست آمده از روش MLE دستی
cat("ضرایب برآورد شده با روش MLE (nlminb):\n")
print(opt_res$par)


# =========================================================
# گام ۴: مقایسه با تابع glm
# =========================================================

# برازش مدل رگرسیون پواسون با استفاده از glm
fit_glm <- glm(Sa ~ C + S + W + Wt, data = train_data, family = poisson(link = 'log'))

cat("\nضرایب برآورد شده با glm:\n")
print(coef(fit_glm))


# =========================================================
# پاسخ به پرسش پایانی اسلاید
# =========================================================
# پرسش: آیا ضرایب nlminb و glm دقیقا یکی هستند؟
# پاسخ: بله، مقادیر ضرایب بسیار به هم نزدیک و عملاً یکسان هستند. 
# تفاوت بسیار ناچیز (در حد چند رقم اعشار) به دلیل استفاده از روش‌های عددی و 
# الگوریتم‌های بهینه‌سازی متفاوت (مثل Newton-Raphson / IRLS در glm در مقابل 
# الگوریتم‌های Quasi-Newton در nlminb) و حد حدس اولیه (Initial values) است.
