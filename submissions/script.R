# ۱. فراخوانی کتابخانه‌ها و خواندن داده‌ها

library(readxl)

# خواندن فایل
df <- as.data.frame(read_xlsx("Crabs.xlsx"))

# ساخت متغیرهای دامی (Dummy Variables)
# مرجع برای رنگ C = 1 و برای ستون فقرات S = 1
df$C2 <- ifelse(df$C == 2, 1, 0)
df$C3 <- ifelse(df$C == 3, 1, 0)
df$C4 <- ifelse(df$C == 4, 1, 0)

df$S2 <- ifelse(df$S == 2, 1, 0)
df$S3 <- ifelse(df$S == 3, 1, 0)


# ۲. تقسیم داده‌ها به مجموعه‌های آموزش (Train 80%) و تست (Test 20%)

set.seed(42) # برای تکرارپذیری دقیق نتایج
n <- nrow(df)
train_idx <- sample(1:n, floor(0.8 * n))

train_data <- df[train_idx, ]
test_data  <- df[-train_idx, ]

# استخراج بردارهای مجموعه آموزش
Sa_tr <- train_data$Sa
W_tr  <- train_data$W
Wt_tr <- train_data$Wt
C2_tr <- train_data$C2
C3_tr <- train_data$C3
C4_tr <- train_data$C4
S2_tr <- train_data$S2
S3_tr <- train_data$S3


# ۳. تعریف تابع منفی لگاریتم درست‌نمایی (Negative Log-Likelihood)

nloglik_poisson <- function(theta) {
  beta0 <- theta[1]
  beta1 <- theta[2] # C2
  beta2 <- theta[3] # C3
  beta3 <- theta[4] # C4
  beta4 <- theta[5] # S2
  beta5 <- theta[6] # S3
  beta6 <- theta[7] # W
  beta7 <- theta[8] # Wt
  
  # پیش‌بینی‌کننده خطی
  eta <- beta0 + beta1 * C2_tr + beta2 * C3_tr + beta3 * C4_tr + 
         beta4 * S2_tr + beta5 * S3_tr + beta6 * W_tr + beta7 * Wt_tr
  
  # تابع پیوند (Link Function): exp(eta)
  lambda <- exp(eta)
  
  # لگاریتم درست‌نمایی پواسون با پایداری عددی بالا
  log_lik <- sum(Sa_tr * log(lambda) - lambda - lfactorial(Sa_tr))
  
  return(-log_lik)
}


# ۴. بهینه‌سازی عددی MLE و محاسبه خطای استاندارد و p-value 

# نقطه شروع
init_theta <- c(log(mean(Sa_tr)), rep(0, 7))

# بهینه‌سازی عددی همراه با محاسبه ماتریس هسیان (Hessian)
fit_mle <- optim(
  par = init_theta,
  fn = nloglik_poisson,
  method = "BFGS",
  hessian = TRUE
)

# استخراج ضرایب
mle_coefs <- fit_mle$par

# محاسبه ماتریس کواریانس ضرایب از معکوس ماتریس هسیان
var_cov_matrix <- solve(fit_mle$hessian)
std_errors <- sqrt(diag(var_cov_matrix))

# محاسبه Z-score و p-value
z_values <- mle_coefs / std_errors
p_values <- 2 * (1 - pnorm(abs(z_values)))

# ساخت جدول خلاصه خروجی MLE
summary_mle <- data.frame(
  Estimate = round(mle_coefs, 4),
  Std_Error = round(std_errors, 4),
  z_value = round(z_values, 4),
  p_value = round(p_values, 4)
)
rownames(summary_mle) <- c("(Intercept)", "C2", "C3", "C4", "S2", "S3", "W", "Wt")

cat("=====================================================\n")
cat(" جدول خلاصه برآورد پارامترها به روش MLE (الگوریتم عددی دستی)\n")
cat("=====================================================\n")
print(summary_mle)


# ۵. صحه‌گذاری با تابع glm آماده R
model_glm <- glm(
  Sa ~ C2 + C3 + C4 + S2 + S3 + W + Wt,
  family = poisson(link = "log"),
  data = train_data
)

cat("\n=====================================================\n")
cat(" خلاصه مدل برازش شده با تابع آماده glm جهت مقایسه\n")
cat("=====================================================\n")
print(summary(model_glm)$coefficients)


# ۶. ارزیابی پیش‌بینی مدل روی داده‌های تست (Test Data)

# پیش‌بینی روی داده‌های تست با ضرایب دست‌نویس MLE
eta_test <- mle_coefs[1] + 
            mle_coefs[2] * test_data$C2 + 
            mle_coefs[3] * test_data$C3 + 
            mle_coefs[4] * test_data$C4 + 
            mle_coefs[5] * test_data$S2 + 
            mle_coefs[6] * test_data$S3 + 
            mle_coefs[7] * test_data$W  + 
            mle_coefs[8] * test_data$Wt

predicted_sa <- exp(eta_test)

# محاسبه معیار RMSE روی مجموعه داده تست
rmse <- sqrt(mean((test_data$Sa - predicted_sa)^2))
cat("\n-----------------------------------------------------\n")
cat("معیار ریشه میانگین مربع خطا (RMSE) روی داده‌های تست:", round(rmse, 4), "\n")
cat("-----------------------------------------------------\n")


# ۷. رسم نمودار مقایسه مقادیر واقعی و پیش‌بینی‌شده

plot(test_data$Sa, predicted_sa, 
     pch = 19, col = "royalblue",
     xlab = "مقادیر واقعی Sa (تست)", 
     ylab = "مقادیر پیش‌بینی‌شده Sa",
     main = "مقایسه مقادیر واقعی و پیش‌بینی‌شده با مدل رگرسیون پواسون")
abline(a = 0, b = 1, col = "red", lty = 2, lwd = 2) # خط ۴۵ درجه جهت ایده‌آل
grid()
