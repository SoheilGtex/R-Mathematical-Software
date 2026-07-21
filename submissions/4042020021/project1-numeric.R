# تابع محاسبه فاصله اقلیدسی میان یک نقطه و یک مرکز
# x1 : مختصات اول نقطه
# y1 : مختصات دوم نقطه
# x2 : مختصات اول مرکز
# y2 : مختصات دوم مرکز


dis <- function(x1, y1, x2, y2) {
  
  distance <- sqrt((x1 - x2)^2 + (y1 - y2)^2)
  
  return(distance)
}



# تابع  اصلی خوشه بندی
# x : ماتریس نقاط جغرافیایی
#     ستون اول = Latitude
#     ستون دوم = Longitude
# k : تعداد مراکز یا خوشه‌ها



km <- function(x, k) {
  
  # تعداد نقاط جغرافیایی
  n <- nrow(x)
  
  

  # مرحله اول:
  # انتخاب تصادفی k نقطه از میان نقاط به عنوان مراکز اولیه

  
  selected_points <- sample(n, k)
  
  centers <- matrix(
    x[selected_points, ],
    nrow = k,
    ncol = 2
  )
  
  
  
  # ایجاد بردار عضویت نقاط
  # clust[i] مشخص می‌کند نقطه i به کدام مرکز تعلق دارد.
  # مقدار اولیه همه عضویت‌ها صفر در نظر گرفته شده است.
  
  
  
  clust <- rep(0, n)
  
  
  # عضویت نقاط در تکرار قبلی
  oldclust <- rep(-1, n)
  
  
  # شمارنده تعداد تکرارهای الگوریتم
  iteration <- 0
  
  
  
  while (sum(clust != oldclust) != 0) {
    
    # ذخیره عضویت فعلی به عنوان عضویت قبلی
    oldclust <- clust
    
    
    # افزایش شمارنده تکرار
    iteration <- iteration + 1
    
    
    
    # ساخت ماتریس فاصله ها
    # سطر i مربوط به نقطه i است.
    # ستون j مربوط به مرکز j است.

    
    
    distance_matrix <- matrix(
      0,
      nrow = n,
      ncol = k
    )
    
    
    
    # محاسبه فاصله تمام نقاط از تمام مراکز
    
    
    
    for (j in 1:k) {
      
      distance_matrix[, j] <- dis(
        x[, 1],
        x[, 2],
        centers[j, 1],
        centers[j, 2]
      )
    }
    
    
    
    # تخصیص هر نقطه به نزدیک ترین نقطه به مرکز
  
    
    for (i in 1:n) {
      
      # پیدا کردن شماره مرکزی که کمترین فاصله را دارد
      nearest_center <- which(
        distance_matrix[i, ] == min(distance_matrix[i, ])
      )
      
      # اگر دو مرکز دقیقاً فاصله برابر داشته باشند،
      # اولین مرکز انتخاب می شود
      clust[i] <- nearest_center[1]
    }
    
    
    
    # محاسبه مراکز جدید
    # مرکز جدید هر خوشه برابر میانگین مختصات نقاط آن است.
    
    for (j in 1:k) {
      
      # تعداد نقاط متعلق به مرکز j
      number_of_points <- sum(clust == j)
      
      
      # در صورتی که خوشه خالی نباشد، مرکز آن به‌روز می‌شود.
      if (number_of_points != 0) {
        
        # میانگین ستون اول نقاط خوشه j
        centers[j, 1] <- mean(
          x[which(clust == j), 1]
          
        )
        
        # میانگین ستون دوم نقاط خوشه j
        centers[j, 2] <- mean(
          x[which(clust == j), 2]
        )
      }
    }
  }
  
  
  
  # جداکردن داده های متعلق به هر خوشه
  # clustdata[[1]] = نقاط خوشه اول
  # clustdata[[2]] = نقاط خوشه دوم
  # و...
  
  
  
  clustdata <- list()
  
  
  # تعداد نقاط موجود در هر خوشه
  size <- rep(0, k)
  
  
  for (j in 1:k) {
    
    clustdata[[j]] <- x[which(clust == j), , drop = FALSE]
    
    size[j] <- sum(clust == j)
  }
  
  
  # قرار دادن نام برای ستون‌های ماتریس مراکز
  colnames(centers) <- c("Latitude", "Longitude")
  
  

  # رسم نمودار نقاط و مراکز

  
  
  plot(
    x[, 1],
    x[, 2],
    type = "n",
    main = "Office Location",
    sub = paste("N =", n, ", k =", k),
    xlab = "Latitude",
    ylab = "Longitude"
  )
  
  
  # رسم پاره‌خط میان هر نقطه و مرکز خوشه آن
  for (i in 1:n) {
    
    segments(
      x0 = x[i, 1],
      y0 = x[i, 2],
      x1 = centers[clust[i], 1],
      y1 = centers[clust[i], 2],
      col = clust[i]
    )
  }
  
  
  
  # رسم خود نقاط جغرافیایی
  points(
    x[, 1],
    x[, 2],
    col = clust,
    pch = 8,
    cex = 0.8
  )
  
  
  # رسم مراکز نهایی 
  points(
    centers[, 1],
    centers[, 2],
    col = 1:k,
    pch = 19,
    cex = 1.5
  )
  
  
  
  result <- list(
    clustdata = clustdata,
    clust = clust,
    centers = centers,
    size = size,
    iteration = iteration
  )
  
  
  return(result)
}




# تولید داده های نمونه 


# برای آنکه در هر بار اجرا نتیجه یکسان باشد.
set.seed(123)


# تولید 50 نقطه جغرافیایی
x <- cbind(
  
  matrix(
    rnorm(
      50,
      mean = 33.8000,
      sd = 0.05
    ),
    ncol = 1
  ),
  
  matrix(
    rnorm(
      50,
      mean = 46.4333,
      sd = 0.05
    ),
    ncol = 1
  )
)



# انتخاب نام ستون ها
colnames(x) <- c("Latitude", "Longitude")



# اجرای تابع برای سه مرکز



answer <- km(x, 3)


# نمایش تمام خروجی
answer



# در صورت نیاز می توان اجزای خروجی را جداگانه مشاهده کرد.


# نقاط متعلق به هر خوشه
answer$clustdata

# شماره خوشه هر نقطه
answer$clust

# مختصات مراکز نهایی
answer$centers

# تعداد نقاط هر خوشه
answer$size

# تعداد تکرارهای الگوریتم
answer$iteration
