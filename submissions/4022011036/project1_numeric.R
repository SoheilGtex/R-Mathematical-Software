# Project 1 - Mathematical Software
# Abolfazl Rahimi | 4022011036
# Poisson-model MLE using nlminb, with a comparison to glm

# Read Crabs.xlsx (stored next to this script) without external packages.
# The supplied workbook has one sheet and five numeric columns.
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

# Resolve the data-file path independently of the working directory.
args <- commandArgs(trailingOnly = FALSE)
file_arg <- args[grepl("^--file=", args)]
if (length(file_arg) == 0L) {
  stop("Run this script with Rscript so that the data-file path can be resolved.")
}
script_path <- normalizePath(sub("^--file=", "", file_arg[1]))
data_path <- file.path(dirname(script_path), "Crabs.xlsx")
if (!file.exists(data_path)) stop("Crabs.xlsx was not found next to the script.")

train_data <- read_crabs_xlsx(data_path)

# Step 1: Dummy coding; C = 1 and S = 1 are the reference levels.
train_data$C2 <- ifelse(train_data$C == 2, 1, 0)
train_data$C3 <- ifelse(train_data$C == 3, 1, 0)
train_data$C4 <- ifelse(train_data$C == 4, 1, 0)
train_data$S2 <- ifelse(train_data$S == 2, 1, 0)
train_data$S3 <- ifelse(train_data$S == 3, 1, 0)

X <- as.matrix(train_data[, c("C2", "C3", "C4", "S2", "S3", "W", "Wt")])
y <- train_data$Sa

# Step 2: Negative Poisson log-likelihood.
# theta contains beta0 through beta7. log = TRUE prevents numerical underflow.
nloglik_loglinear <- function(theta) {
  linear_predictor <- theta[1] + X %*% theta[2:8]
  lambda <- exp(pmin(linear_predictor, log(.Machine$double.xmax)))
  -sum(dpois(y, lambda = lambda, log = TRUE))
}

# Step 3: Numerical optimization with the requested starting values.
theta0 <- c(log(mean(y)), rep(0, 7))
fit_nlminb <- nlminb(
  start = theta0,
  objective = nloglik_loglinear,
  lower = rep(-Inf, 8),
  upper = rep(Inf, 8)
)

coef_names <- c("(Intercept)", "C2", "C3", "C4", "S2", "S3", "W", "Wt")
coef_nlminb <- setNames(fit_nlminb$par, coef_names)

# Step 4: Equivalent glm fit; factor() keeps the first level as the reference.
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

cat("Number of observations:", nrow(train_data), "\n")
cat("nlminb convergence:", fit_nlminb$convergence, "(0 means success)\n")
cat("Negative log-likelihood:", format(fit_nlminb$objective, digits = 12), "\n\n")
print(comparison, row.names = FALSE)
cat("\nMaximum absolute difference:", format(max(comparison$absolute_difference), digits = 8), "\n")

if (max(comparison$absolute_difference) < 1e-5) {
  cat("Conclusion: nlminb and glm coefficients agree up to numerical precision.\n")
} else {
  cat("Conclusion: Small differences are likely due to tolerance and stopping criteria.\n")
}
