library(readxl)

dat <- as.data.frame(read_xlsx("Crabs.xlsx"))
dat$C <- factor(dat$C)
dat$S <- factor(dat$S)

set.seed(42)
n_total <- nrow(dat)
pick <- sample(1:n_total, floor(0.8 * n_total))
train_data <- dat[pick, ]
holdout_data <- dat[-pick, ]

build_dummy <- function(col, lvl) {
  result <- ifelse(col == lvl, 1, 0)
  return(result)
}

train_data$C2 <- build_dummy(train_data$C, 2)
train_data$C3 <- build_dummy(train_data$C, 3)
train_data$C4 <- build_dummy(train_data$C, 4)
train_data$S2 <- build_dummy(train_data$S, 2)
train_data$S3 <- build_dummy(train_data$S, 3)

resp <- train_data$Sa
covars <- cbind(train_data$C2, train_data$C3, train_data$C4,
                 train_data$S2, train_data$S3, train_data$W, train_data$Wt)

nloglik_loglinear <- function(theta) {
  b0 <- theta[1]
  b_vec <- theta[2:8]

  m <- length(resp)
  ll_vals <- numeric(m)

  for (k in 1:m) {
    eta_k <- b0 + sum(b_vec * covars[k, ])
    mu_k <- exp(eta_k)
    p_k <- dpois(resp[k], mu_k)
    ll_vals[k] <- log(p_k)
  }

  -sum(ll_vals)
}

start_vals <- c(log(mean(resp)), rep(0, 7))

opt_result <- nlminb(start_vals, nloglik_loglinear, lower = rep(-Inf, 8), upper = rep(Inf, 8))
opt_result$par

glm_fit <- glm(Sa ~ C + S + W + Wt, data = train_data, family = poisson(link = "log"))
coef(glm_fit)

result_table <- data.frame(
  coef_name = c("b0", "C2", "C3", "C4", "S2", "S3", "W", "Wt"),
  hand_mle = round(opt_result$par, 6),
  glm_out = round(unname(coef(glm_fit)), 6)
)
result_table

# به نظرم مشکلی نیست، این اختلاف خیلی کوووچیکه چهار پنج رقم اعشاری طبیعیه.
# قبلا هم یه بار با یه دیتاست دیگه دیده بودم که glm و nlminb
# دقیقا رو یه نقطه توقف نمیکنن ولی جفتشون درستن. اصلش برمیگرده
# به معیار توقفشون - یعنی نه دقیقا "معیار توقف"، بیشتر شبیه
# تلورانسیه که هر الگوریتم برای خودش داره: وقتی تغییر تابع هدف
# (یا پارامترا، بسته به پیادسازی) از یه حدی کمتر بشه میگه خب
# دیگه بسه همینجا خوبه. این حدا بین دو تا تابع دیفالت متفاوتن.
# مهمتر از اون اینه که اطراف نقطه‌ی بیشینه‌ی لاگ‌لایکیهود معمولا
# خیلی صافه (مشتق تقریبا صفره)، برای همین حتی یه فرق کوچیک تو
# تلورانس میتونه نقطه توقف رو جابجا کنه. به‌هرحال فکنم جای نگرانی نیست.
