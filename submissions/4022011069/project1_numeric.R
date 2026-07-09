# ==============================================================================
# Project: Poisson Log-Linear Regression via Maximum Likelihood Estimation (MLE)
# Course: Mathematical Software - Kharazmi University
# Instructor: Dr. Elham Tabrizi
# TA: Soheil Salmani Safarpour
# Student: Kiana Kamrava
# Student ID: 4022011069
# Date: 1405/04/19
# ==============================================================================

# ---------------------------------------------------------
# 0. Load required library
# ---------------------------------------------------------
if (!require("readxl")) {
  install.packages("readxl")
}
library(readxl)

# ---------------------------------------------------------
# 1. Data Loading and Preprocessing
# ---------------------------------------------------------

# Read the dataset
df <- read_xlsx("C:/Users/1/Desktop/Kiana_R_Crab_Project/Crabs.xlsx")

# نام ستون‌ها را بررسی می‌کنیم
cat("--- Column Names ---\n")
print(colnames(df))

# Convert categorical variables to factors
# C: Color (Levels: 1, 2, 3, 4) -> Reference Level: 1
# S: Spine condition (Levels: 1, 2, 3) -> Reference Level: 1
df$C <- factor(df$C, levels = c(1, 2, 3, 4))
df$S <- factor(df$S, levels = c(1, 2, 3))

# Display initial structure
cat("--- Dataset Structure ---\n")
str(df)

cat("\n--- First 6 rows ---\n")
print(head(df))

cat(sprintf("\nTotal observations: %d\n", nrow(df)))

# ---------------------------------------------------------
# 2. Train/Test Split (optional but kept for consistency)
# ---------------------------------------------------------
set.seed(42)

n_total <- nrow(df)
train_indices <- sample(1:n_total, floor(0.8 * n_total))

train_data <- df[train_indices, ]
test_data  <- df[-train_indices, ]

cat(sprintf("\nTraining set size: %d\n", nrow(train_data)))
cat(sprintf("Test set size: %d\n", nrow(test_data)))

# ---------------------------------------------------------
# 3. Feature Engineering (Dummy Variables)
# ---------------------------------------------------------
# متغیر C: 4 سطح -> 3 متغیر مجازی (سطح 1 مرجع است)
# For variable C (4 levels) -> 3 dummies (C2, C3, C4), C1 as reference
# For variable S (3 levels) -> 2 dummies (S2, S3), S1 as reference

C2 <- ifelse(train_data$C == 2, 1, 0)
C3 <- ifelse(train_data$C == 3, 1, 0)
C4 <- ifelse(train_data$C == 4, 1, 0)

# متغیر S: 3 سطح -> 2 متغیر مجازی (سطح 1 مرجع است)
S2 <- ifelse(train_data$S == 2, 1, 0)
S3 <- ifelse(train_data$S == 3, 1, 0)

# متغیرهای عددی و متغیر پاسخ
W  <- train_data$W
Wt <- train_data$Wt
Sa <- train_data$Sa

n_obs <- length(Sa)

cat(sprintf("\nNumber of training observations used in MLE: %d\n", n_obs))

# Quick check
cat("\n--- Dummy Variables Check ---\n")
cat(sprintf("n_obs = %d\n", n_obs))
cat(sprintf("Sum C2=%d, C3=%d, C4=%d\n", sum(C2), sum(C3), sum(C4)))
cat(sprintf("Sum S2=%d, S3=%d\n", sum(S2), sum(S3)))

# ---------------------------------------------------------
# 4. Negative Log-Likelihood Function for Poisson MLE
# ---------------------------------------------------------
# Model:
# log(lambda_i) = beta0 + beta1*C2 + beta2*C3 + beta3*C4
#               + beta4*S2 + beta5*S3 + beta6*W + beta7*Wt

nloglik_loglinear <- function(theta) {

  # استخراج پارامترها از بردار theta
  # Unpack parameters
  beta0 <- theta[1]
  beta1 <- theta[2]
  beta2 <- theta[3]
  beta3 <- theta[4]
  beta4 <- theta[5]
  beta5 <- theta[6]
  beta6 <- theta[7]
  beta7 <- theta[8]

  # مجموع لگاریتم درست‌نمایی
  ll_sum <- 0
  
  for (i in 1:n_obs) {
    # گام 3 از صورت‌مسئله: محاسبه linear predictor
    # Linear predictor
    eta <- beta0 +
           beta1 * C2[i] +
           beta2 * C3[i] +
           beta3 * C4[i] +
           beta4 * S2[i] +
           beta5 * S3[i] +
           beta6 * W[i]  +
           beta7 * Wt[i]

    # گام 4 از صورت‌مسئله: lambda_i = exp(eta)
    # Inverse log-link
    lambda_i <- exp(eta)

    # گام 5 از صورت‌مسئله: محاسبه احتمال پواسون
    # P(Sa_i) = dpois(Sa[i], lambda = lambda_i)
    # Log-probability under Poisson
    ll_sum <- ll_sum + dpois(Sa[i], lambda = lambda_i, log = TRUE)
  }

  # گام 6 از صورت‌مسئله: برگرداندن منفی لگاریتم درست‌نمایی
  # Return negative log-likelihood (for minimization)
  return(-ll_sum)
}

# ---------------------------------------------------------
# 5. Numerical Optimization using nlminb
# ---------------------------------------------------------
# گام 3 از صورت‌مسئله: تعریف نقطه شروع
# beta0 = log(mean(Sa)) چون در مدل پواسون lambda = exp(beta0)
# بقیه ضرایب صفر در نظر گرفته می‌شوند
# Initial values:
# beta0 = log(mean(Sa)), others = 0
theta0 <- c(log(mean(Sa)), 0, 0, 0, 0, 0, 0, 0)

cat("\n--- Initial Parameter Values (theta0) ---\n")
cat(sprintf("beta0 = log(mean(Sa)) = log(%.4f) = %.4f\n",
            mean(Sa), log(mean(Sa))))
cat("beta1 to beta7 = 0\n")

cat("\n--- Starting Optimization (nlminb) ---\n")

fit_mle <- nlminb(
  start     = theta0,
  objective = nloglik_loglinear,
  lower     = rep(-Inf, 8),
  upper     = rep(Inf, 8),
  control   = list(iter.max = 1000, eval.max = 1000, trace = 0)
)

# بررسی وضعیت همگرایی
cat(sprintf("\nConvergence status: %d (0 = success)\n", fit_mle$convergence))
cat(sprintf("Message: %s\n", fit_mle$message))
cat(sprintf("Negative log-likelihood at optimum: %.6f\n", fit_mle$objective))

# استخراج ضرایب بهینه
# Extract coefficients
mle_coefs <- fit_mle$par
names(mle_coefs) <- c("(Intercept)", "C2", "C3", "C4", "S2", "S3", "W", "Wt")

cat("\n--- Manual MLE Results (nlminb) ---\n")
print(round(mle_coefs, 6))

# ---------------------------------------------------------
# 6. Comparison with Standard GLM
# ---------------------------------------------------------
# گام 4 از صورت‌مسئله: مقایسه با glm
model_glm <- glm(
  Sa ~ C + S + W + Wt,
  data   = train_data,
  family = poisson(link = "log")
)

glm_coefs <- coef(model_glm)

cat("\n--- Standard GLM Results ---\n")
print(round(glm_coefs, 6))

# ---------------------------------------------------------
# 7. Detailed Comparison Table
# ---------------------------------------------------------
# Align names carefully
# glm names are typically: (Intercept), C2, C3, C4, S2, S3, W, Wt
# (because C and S are factors with reference level 1)

common_names <- names(glm_coefs)

comparison_df <- data.frame(
  Parameter    = common_names,
  MLE_Manual   = round(as.numeric(mle_coefs[common_names]), 6),
  GLM_Standard = round(as.numeric(glm_coefs), 6),
  Difference   = round(as.numeric(mle_coefs[common_names]) - as.numeric(glm_coefs), 6)
)

cat("\n--- Coefficient Comparison ---\n")
print(comparison_df)

# بررسی میزان تفاوت
max_diff <- max(abs(comparison_df$Difference))
cat(sprintf("\nMaximum absolute difference: %.8f\n", max_diff))

if (max_diff < 1e-4) {
  cat(" ضرایب به خوبی همگرا شده‌اند\n")
} else {
  cat(" تفاوت قابل توجهی وجود دارد. بررسی بیشتر لازم است.\n")
}

# ---------------------------------------------------------
# 8. Theoretical Explanation (پرسش صورت‌مسئله)
# ---------------------------------------------------------

cat("\n")
cat("=================================================================\n")
cat("گام ۴ - پرسش: آیا ضرایب دقیقاً یکی هستند؟\n")
cat("اگر تفاوت ناچیزی در حد اعشار دارند، به نظر شما علت آن چیست؟\n")
cat("=================================================================\n")
cat("\nپاسخ:\n")
cat("ضرایب به دست آمده از nlminb و glm از نظر عددی معادل هستند.\n")
cat("تفاوت‌های جزئی در حد اعشار به دلایل زیر است:\n\n")
cat("1. الگوریتم بهینه‌سازی متفاوت:\n")
cat("   - nlminb از روش شبه-نیوتن (Quasi-Newton) استفاده می‌کند\n")
cat("   - glm() از روش IRLS (Iteratively Reweighted Least Squares)\n")
cat("     استفاده می‌کند که برای GLM بهینه‌تر است\n\n")
cat("2. معیار توقف متفاوت:\n")
cat("   - هر الگوریتم تُلرانس همگرایی متفاوتی دارد\n")
cat("   - دقت محاسبات اعشاری (Floating Point Precision)\n\n")
cat("3. هر دو روش همان تابع لگاریتم درست‌نمایی پواسون را\n")
cat("   بیشینه می‌کنند، بنابراین نتیجه نظری یکسان است.\n")
cat("=================================================================\n")

# ---------------------------------------------------------
# Again explain in English
# ---------------------------------------------------------
cat("\n--- Theoretical Note ---\n")
cat("The coefficients obtained from manual MLE (via nlminb) are numerically\n")
cat("very close (essentially equivalent) to those from glm().\n")
cat("Minor differences in decimal places arise because:\n")
cat("1. Different optimization algorithms: nlminb uses a quasi-Newton method,\n")
cat("   while glm() typically uses Iteratively Reweighted Least Squares (IRLS).\n")
cat("2. Convergence tolerances and stopping criteria may vary slightly.\n")
cat("Both methods maximize the same Poisson log-likelihood function.\n")
