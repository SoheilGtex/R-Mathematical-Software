# پروژه ۱ - نرم افزارهای ریاضی
# ابوالفضل رحمی | 4022011036
# برآورد MLE مدل پواسون با nlminb و مقایسه با glm

# این تابع فایل Crabs.xlsx (که در کنار این اسکریپت قرار دارد) را بدون پکیج
# اضافی می خواند. فایل داده فقط شامل یک شیت و پنج ستون عددی است.
read_crabs_xlsx <- function(path) {
  con <- unz(path, "xl/worksheets/sheet1.xml")
  on.exit(close(con), add = TRUE)
  xml <- paste(readLines(con, warn = FALSE), collapse = "")
  values <- regmatches(xml, gregexpr("(?<=<v>)[^<]+(?=</v>)", xml, perl = TRUE))[[1]]
  values <- as.numeric(values)
  data <- as.data.frame(matrix(values, ncol = 5, byrow = TRUE))
  names(data) <- c("C", "S", "W", "Wt", "Sa")
  data
}

# مسیر فایل داده، مستقل از پوشه ای که Rscript از آن اجرا می شود.
args <- commandArgs(trailingOnly = FALSE)
file_arg <- args[grepl("^--file=", args)]
if (length(file_arg) == 0L) {
  stop("این اسکریپت را با Rscript اجرا کنید تا مسیر فایل داده تشخیص داده شود.")
}
script_path <- normalizePath(sub("^--file=", "", file_arg[1]))
data_path <- file.path(dirname(script_path), "Crabs.xlsx")
if (!file.exists(data_path)) stop("فایل Crabs.xlsx در کنار اسکریپت پیدا نشد.")

train_data <- read_crabs_xlsx(data_path)

# گام ۱: کدگذاری مجازی؛ C=1 و S=1 سطح مرجع هستند.
train_data$C2 <- ifelse(train_data$C == 2, 1, 0)
train_data$C3 <- ifelse(train_data$C == 3, 1, 0)
train_data$C4 <- ifelse(train_data$C == 4, 1, 0)
train_data$S2 <- ifelse(train_data$S == 2, 1, 0)
train_data$S3 <- ifelse(train_data$S == 3, 1, 0)

X <- as.matrix(train_data[, c("C2", "C3", "C4", "S2", "S3", "W", "Wt")])
y <- train_data$Sa

# گام ۲: منفی لگاریتم تابع درست نمایی پواسون.
# theta شامل beta0 تا beta7 است. dpois(..., log = TRUE) از underflow جلوگیری می کند.
nloglik_loglinear <- function(theta) {
  linear_predictor <- theta[1] + X %*% theta[2:8]
  lambda <- exp(pmin(linear_predictor, log(.Machine$double.xmax)))
  -sum(dpois(y, lambda = lambda, log = TRUE))
}

# گام ۳: بهینه سازی عددی با مقدار شروع پیشنهادی.
theta0 <- c(log(mean(y)), rep(0, 7))
fit_nlminb <- nlminb(
  start = theta0,
  objective = nloglik_loglinear,
  lower = rep(-Inf, 8),
  upper = rep(Inf, 8)
)

coef_names <- c("(Intercept)", "C2", "C3", "C4", "S2", "S3", "W", "Wt")
coef_nlminb <- setNames(fit_nlminb$par, coef_names)

# گام ۴: برازش معادل با glm؛ factor سطح اول را به عنوان مرجع نگه می دارد.
fit_glm <- glm(
  Sa ~ factor(C) + factor(S) + W + Wt,
  data = train_data,
  family = poisson(link = "log")
)
coef_glm <- coef(fit_glm)
names(coef_glm) <- coef_names

comparison <- data.frame(
  coefficient = coef_names,
  nlminb = unname(coef_nlminb),
  glm = unname(coef_glm),
  absolute_difference = abs(unname(coef_nlminb) - unname(coef_glm)),
  row.names = NULL
)

cat("تعداد مشاهدات:", nrow(train_data), "\n")
cat("همگرایی nlminb:", fit_nlminb$convergence, "(صفر یعنی موفق)\n")
cat("منفی لگاریتم درست نمایی:", format(fit_nlminb$objective, digits = 12), "\n\n")
print(comparison, row.names = FALSE)
cat("\nبیشترین اختلاف مطلق:", format(max(comparison$absolute_difference), digits = 8), "\n")

if (max(comparison$absolute_difference) < 1e-5) {
  cat("نتیجه: ضرایب nlminb و glm تا خطای عددی یکسان هستند.\n")
} else {
  cat("نتیجه: اختلاف کوچک احتمالا به دلیل تلورانس و معیار توقف الگوریتم است.\n")
}
