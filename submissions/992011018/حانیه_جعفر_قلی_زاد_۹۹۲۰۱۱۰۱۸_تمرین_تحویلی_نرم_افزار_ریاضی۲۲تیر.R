library(readxl)
df <- read_excel("Crabs.xlsx")


head(df)


set.seed(42)
n <- nrow(df)
train_ind <- sample(1:n, size = round(0.7 * n))
train_data <- df[train_ind, ]
test_data <- df[-train_ind, ]

print(paste("tedad kol dade ha", n))
print(paste("train", nrow(train_data)))
print(paste("test", nrow(test_data)))


C_train  <- train_data$C
S_train  <- train_data$S
W_train  <- train_data$W
Wt_train <- train_data$Wt
Sa_train <- train_data$Sa


C2_train <- ifelse(C_train == 2, 1, 0)
C3_train <- ifelse(C_train == 3, 1, 0)
C4_train <- ifelse(C_train == 4, 1, 0)

)
S2_train <- ifelse(S_train == 2, 1, 0)
S3_train <- ifelse(S_train == 3, 1, 0)


X_train <- cbind(1, C2_train, C3_train, C4_train, S2_train, S3_train, W_train, Wt_train)
colnames(X_train) <- c("Intercept", "C2", "C3", "C4", "S2", "S3", "W", "Wt")

y_train <- Sa_train



nloglik_loglinear <- function(theta, y, X) {
  

  beta0 <- theta[1]
  beta1 <- theta[2]
  beta2 <- theta[3]
  beta3 <- theta[4]
  beta4 <- theta[5]
  beta5 <- theta[6]
  beta6 <- theta[7]
  beta7 <- theta[8]
  
  n <- length(y)
  likelihood_product <- 1
  
  
  for (i in 1:n) {
    
    
    eta <- beta0 + 
           beta1 * X[i, 2] +
           beta2 * X[i, 3] +
           beta3 * X[i, 4] +
           beta4 * X[i, 5] +
           beta5 * X[i, 6] +
           beta6 * X[i, 7] +
           beta7 * X[i, 8]
    
    
    lambda <- exp(eta)
    
    
    prob_i <- dpois(y[i], lambda)
    
    
    likelihood_product <- likelihood_product * prob_i
  }
  
  
  log_likelihood <- log(likelihood_product)
  
  
  return(-log_likelihood)
}



mean_Sa_train <- mean(Sa_train)
beta0_0 <- log(mean_Sa_train)
beta1_0 <- 0
beta2_0 <- 0
beta3_0 <- 0
beta4_0 <- 0
beta5_0 <- 0
beta6_0 <- 0
beta7_0 <- 0

theta0 <- c(beta0_0, beta1_0, beta2_0, beta3_0, beta4_0, beta5_0, beta6_0, beta7_0)


result_nlminb <- nlminb(start = theta0,objective = nloglik_loglinear,y = y_train,
X = X_train, lower = -Inf, upper = Inf)


theta_hat <- result_nlminb$par
names(theta_hat) <- c("beta0", "beta1", "beta2", "beta3", "beta4", "beta5", "beta6", "beta7")

print("parametr hai takhmin zde shode ba nlminb:")
print(theta_hat)


model_glm <- glm(Sa ~ ., data = train_data, family = poisson(link = 'log'))


summary(model_glm)

print(" natige takhmin ba glm:")
print(coef(model_glm))



coef_glm <- coef(model_glm)
coef_manual <- theta_hat

com_table <- data.frame(
  Parameter = names(coef_glm),
  glm = coef_glm,
  Manual = coef_manual[1:length(coef_glm)],
  Difference = coef_glm - coef_manual[1:length(coef_glm)]
)

print("jadval moghayse ")
print(comparison_table)



# علت تفاوت در ضرایب:

# این دو الگوریتم مسیرهای متفاوتی برای رسیدن به پاسخ نهایی طی می‌کنند


# هر الگوریتم بر اساس یک آستانه توقف متفاوت کار می‌کند

# ممکن است یکی از الگوریتم‌ها زودتر از دیگری به شرط همگرایی برسد و متوقف شود


# کامپیوترها اعداد را با دقت محدود (مثلاً ۱۶ رقم اعشار) ذخیره می‌کنند

# این خطاها در طی مراحل مختلف محاسبه ممکن است باعث این تفاوت ها شوند

# هر الگوریتم تعداد تکرارهای متفاوتی برای رسیدن به همگرایی نیاز دارد

# تکرارهای بیشتر معمولاً به دقت بالاتر منجر می‌شود

# البته این تفاوت مقدار بسیار کوچکی دارند و میتوان از ان صرف نظر کرد