library(readxl)
file_path <- "Crabs.xlsx"
crabs_data <- read_xlsx(file_path)

set.seed(42)
n <- nrow(crabs_data)
idx <- sample(1:n, floor(0.8*n))
train_data <- crabs_data[idx,]
test_data <- crabs_data[-idx,]

train_data$C <- factor(train_data$C)
train_data$C2 <- ifelse(train_data$C == 2,1 ,0)
train_data$C3 <- ifelse(train_data$C == 3,1 ,0)
train_data$C4 <- ifelse(train_data$C == 4,1 ,0)
train_data$S <- factor(train_data$S)
train_data$S2 <- ifelse(train_data$S == 2,1, 0)
train_data$S3 <- ifelse(train_data$S == 3,1, 0)
head(train_data[ ,c("S", "S2", "S3")])

nloglik_loglinear <- function(theta, data) {
  beta0 = theta[1]  # Interception
  beta1 = theta[2]  # C2
  beta2 = theta[3]  # C3
  beta3 = theta[4]  # C4
  beta4 = theta[5]  # S2
  beta5 = theta[6]  # S3
  beta6 = theta[7]  # W
  beta7 = theta[8]  # Wt

  prediction <- beta0 + beta1 * data$C2 + beta2 * data$C3 +
    beta3 * data$C4 + beta4 * data$S2 + beta5 * data$S3 +
    beta6 * data$W + beta7 * data$Wt

  lambda <- exp(prediction)
  log_likelihoods <- dpois(x = data$Sa, lambda = lambda)
  log_likelihoods <- -sum(log_likelihoods)

  -log_likelihoods
}

initial_tehta <- rep(0, 8)
initial_null <- nloglik_loglinear(theta= initial_tehta, data= train_data)
start_beta0 <- log(mean(train_data$Sa))
start_another_beta <- rep(0, 7)
theta0 <- c(start_beta0, other_beta_start)
print(theta0)

optimize_result <- nlminb(start =theta0, objective= nloglik_loglinear,
                          data = train_data,lower = -Inf, upper = Inf)
print(optimize_result)

model_glm <- glm(train_data$Sa ~ train_data$C + train_data$S + train_data$W+
 train_data$Wt ,data = train_data, family = poisson(link = 'log'))

summary(model_glm)

# اعداد به دست آمده در هر دو روش خیلییی نزدیک به یکدیگرند.
# بله ضرایب کاملا یکی هستند. تفاوت ناچیز در حد اعشار به دلیل الگوریتم‌های بهینه‌سازی متفاوت است.
# هر کدام از یک مسیر ریاضی متفاوت برای رسیدن به نقطه بهینه استفاده می‌کنند و این تفاوت‌های جزئی کاملاً طبیعیه.
