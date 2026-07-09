# Ex 22 Tir
# Reyhane Amraei  4022011012

library(readxl)
df = as.data.frame(read_xlsx("F:/Uni/نرم افزار ریاضی/Crabs.xlsx"))
head(df)
df$C <- factor(df$C)
df$S <- factor(df$S)
str(df)
set.seed(42)
n <- nrow(df)
train_idx <- sample(1:n, floor(0.8*n))
train_data <- df[train_idx, ]
test_data <- df[-train_idx, ]
train_data$C2 <- ifelse(train_data$C == 2, 1, 0)
train_data$C3 <- ifelse(train_data$C == 3, 1, 0)
train_data$C4 <- ifelse(train_data$C == 4, 1, 0)
train_data$S2 <- ifelse(train_data$S == 2, 1, 0)
train_data$S3 <- ifelse(train_data$S == 3, 1, 0)
nloglik_loglinear <- function(theta) {
  b0 <- theta[1]
  b1 <- theta[2]
  b2 <- theta[3]
  b3 <- theta[4]
  b4 <- theta[5]
  b5 <- theta[6]
  b6 <- theta[7]
  b7 <- theta[8]
  
  e <- b0 + b1*train_data$C2 + b2*train_data$C3 + 
    b3*train_data$C4 + b4*train_data$S2 + 
    b5*train_data$S3 + b6*train_data$W + b7*train_data$Wt
  
  la <- exp(e)
  ehtemal <- dpois(train_data$Sa, la)
  neg_loglik <- -sum(log(ehtemal))
  return(neg_loglik)
}

theta0 <- c(log(mean(train_data$Sa)), rep(0,7))
kamine_nlminb <- nlminb(start = theta0, objective = nloglik_loglinear,
                        lower = c(-Inf, -Inf, -Inf, -Inf, -Inf, -Inf, -Inf, -Inf),
                        upper = c(Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf))
cat("ضرایب بدست اومده از nlminb \n")
print(kamine_nlminb$par)

bishine_glm <- glm(Sa~C+S+W+Wt, data = train_data , family = poisson(link = 'log'))
cat("ضرایب بدست اومده از glm \n")
print(bishine_glm$coefficients)

#پرسش گام 4
#خیر. دقیقا یکی نیستن.
#هر دو روش عملا به یک جواب رسیدن، اما تفاوت کمی توی اعشار های پایانی دارن
#این تفاوت به خاطر تفاوت روش حل این دو تابع هست
#این دو تابع با هم متفاوت هستن(هر دو برای بیشینه کردن درستنمایی استفاده میشن)، یکی بیشینه و یکی کمینه رو مبنا قرار میدن
#از طرفی چون الگوریتم یا راه رسیدن به جواب اونها با هم متفاوته، دقت توی نقطه توقف اونها هم متفاوته. 
