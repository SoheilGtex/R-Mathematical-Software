# پروژه ۱: برازش مدل پواسون با روش بیشینه‌سازی درست‌نمایی
# نام: شهرزاد احدی
# شماره دانشجویی: 4012011002

library(readxl)

# اگر کد از ریشه مخزن اجرا شود، مسیر اول درست است.
# مسیر دوم برای زمانی است که working directory داخل پوشه دانشجو باشد.
data_path <- file.path("assignments", "mathematical-software", "Crabs.xlsx")

if (!file.exists(data_path)) {
  data_path <- file.path("..", "..", "assignments",
                         "mathematical-software", "Crabs.xlsx")
}

if (!file.exists(data_path)) {
  stop("فایل Crabs.xlsx پیدا نشد.")
}

df <- as.data.frame(read_xlsx(data_path))

# متغیرهای C و S کیفی هستند و سطح ۱ به عنوان سطح مرجع در نظر گرفته می‌شود.
df$C <- factor(df$C, levels = c(1, 2, 3, 4))
df$S <- factor(df$S, levels = c(1, 2, 3))

# تقسیم داده‌ها به دو بخش آموزش و آزمون
set.seed(42)
n <- nrow(df)
train_idx <- sample(1:n, floor(0.8 * n))

train_data <- df[train_idx, ]
test_data <- df[-train_idx, ]

# ساخت متغیرهای مجازی
train_data$C2 <- ifelse(train_data$C == "2", 1, 0)
train_data$C3 <- ifelse(train_data$C == "3", 1, 0)
train_data$C4 <- ifelse(train_data$C == "4", 1, 0)

train_data$S2 <- ifelse(train_data$S == "2", 1, 0)
train_data$S3 <- ifelse(train_data$S == "3", 1, 0)

# منفی لگاریتم تابع درست‌نمایی
nloglik_loglinear <- function(theta) {
  beta0 <- theta[1]
  beta1 <- theta[2]
  beta2 <- theta[3]
  beta3 <- theta[4]
  beta4 <- theta[5]
  beta5 <- theta[6]
  beta6 <- theta[7]
  beta7 <- theta[8]

  log_likelihood <- 0

  for (i in 1:nrow(train_data)) {
    linear_predictor <- beta0 +
      beta1 * train_data$C2[i] +
      beta2 * train_data$C3[i] +
      beta3 * train_data$C4[i] +
      beta4 * train_data$S2[i] +
      beta5 * train_data$S3[i] +
      beta6 * train_data$W[i] +
      beta7 * train_data$Wt[i]

    lambda_i <- exp(linear_predictor)

    log_likelihood <- log_likelihood +
      dpois(train_data$Sa[i], lambda = lambda_i, log = TRUE)
  }

  return(-log_likelihood)
}

# حدس اولیه ضرایب
theta0 <- c(log(mean(train_data$Sa)), rep(0, 7))

# برآورد ضرایب با nlminb
fit_nlminb <- nlminb(
  start = theta0,
  objective = nloglik_loglinear,
  lower = rep(-Inf, 8),
  upper = rep(Inf, 8)
)

coef_names <- c(
  "(Intercept)", "C2", "C3", "C4",
  "S2", "S3", "W", "Wt"
)

coef_nlminb <- setNames(fit_nlminb$par, coef_names)

# برازش همان مدل با glm
fit_glm <- glm(
  Sa ~ C + S + W + Wt,
  data = train_data,
  family = poisson(link = "log")
)

coef_glm <- coef(fit_glm)[coef_names]

# مقایسه ضرایب دو روش
comparison <- data.frame(
  parameter = coef_names,
  nlminb = as.numeric(coef_nlminb),
  glm = as.numeric(coef_glm),
  difference = as.numeric(coef_nlminb - coef_glm)
)

print(comparison, digits = 8, row.names = FALSE)

cat("\nتعداد داده‌های آموزش:", nrow(train_data), "\n")
cat("تعداد داده‌های آزمون:", nrow(test_data), "\n")
cat("کد همگرایی nlminb:", fit_nlminb$convergence, "\n")
cat("بیشترین قدر مطلق اختلاف ضرایب:",
    max(abs(comparison$difference)), "\n")

