library(readxl)

df <- as.data.frame(read_xlsx("Crabs.xlsx"))
df$C <- factor(df$C)
df$S <- factor(df$S)

set.seed(42)
n <- nrow(df)
idx <- sample(1:n, floor(0.8*n))
train_data <- df[idx,]
test_data <- df[-idx,]

train_data$C2 <- ifelse(train_data$C == 2, 1, 0)
train_data$C3 <- ifelse(train_data$C == 3, 1, 0)
train_data$C4 <- ifelse(train_data$C == 4, 1, 0)
train_data$S2 <- ifelse(train_data$S == 2, 1, 0)
train_data$S3 <- ifelse(train_data$S == 3, 1, 0)

Sa <- train_data$Sa
C2 <- train_data$C2
C3 <- train_data$C3
C4 <- train_data$C4
S2 <- train_data$S2
S3 <- train_data$S3
W  <- train_data$W
Wt <- train_data$Wt

nloglik_loglinear <- function(theta){
  beta0 <- theta[1]; beta1 <- theta[2]; beta2 <- theta[3]; beta3 <- theta[4]
  beta4 <- theta[5]; beta5 <- theta[6]; beta6 <- theta[7]; beta7 <- theta[8]

  n_obs <- length(Sa)
  loglik <- 0

  for(i in 1:n_obs){
    linear_predictor <- beta0 + beta1*C2[i] + beta2*C3[i] + beta3*C4[i] +
      beta4*S2[i] + beta5*S3[i] + beta6*W[i] + beta7*Wt[i]

    lambda_i <- exp(linear_predictor)
    prob_i <- dpois(Sa[i], lambda_i)

    loglik <- loglik + log(prob_i)
  }

  -loglik
}

theta0 <- c(log(mean(Sa)), 0, 0, 0, 0, 0, 0, 0)

fit_manual <- nlminb(theta0, nloglik_loglinear, lower=rep(-Inf,8), upper=rep(Inf,8))
fit_manual$par

fit_glm <- glm(Sa ~ C + S + W + Wt, data = train_data, family = poisson(link = "log"))
coef(fit_glm)

comparison <- data.frame(
  parameter = c("beta0","beta1(C2)","beta2(C3)","beta3(C4)","beta4(S2)","beta5(S3)","beta6(W)","beta7(Wt)"),
  manual_MLE = round(fit_manual$par,6),
  glm = round(unname(coef(fit_glm)),6)
)
comparison

# ضرایب یکی نمیشن، ولی اختلافشون خیلیییی کوچیکه (رقم چهارم پنجم اعشار).
# نکته فکنم اینجاست که glm برای مدلهای نمایی مثل پواسون یه مزیت ساختاری داره:
# چون تابع لینک لگاریتمی، مشتق دوم تابع درستنمایی رو میشه به فرم
# بسته و دقیق نوشت (نه تقریبی ). IRLS دقیقا از همین مشتقات تحلیلی
# برای بهروزرسانی ضرایب در هر تکرار استفاده میکنه، پس مستقیم به سمت
# جواب دقیق حرکت میکنه تقریباا بدون خطای گرد کردن از اون تقریب عددیه.
# nlminb تازه اطلاعاتی از ساختار مدل نداره؛ چون یه بهینهساز عمومیه
# که برای هر تابعی کار میکنه، مجبوره گرادیان رو با تفاضل محدود
# تخمین بزنه. همین تقریبه هر چقدرم دقیق باشه، یه خطای کوچیک وارد
# مسیر همگرایی میکنه که نتیجهش همون اختلاف جزئیه که توی ضرایبه.
