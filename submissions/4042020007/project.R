# تابع محاسبه فاصله بین نقاط و مراکز
x <- cbind(matrix(rnorm(50, mean = 33.8000, sd =.05), ncol =1),matrix(rnorm(50, mean = 46.4333,sd = .05), ncol = 1))
dis <- function(x, centers){
  
  n <- nrow(x)
  k <- nrow(centers)
  
  d <- matrix(0,n,k)
  
  for(i in 1:n){
    for(j in 1:k){
      
      d[i,j] <- sqrt((x[i,1]-centers[j,1])^2 +
                       (x[i,2]-centers[j,2])^2)
    }
  }
  
  return(d)
}



# تابع اصلی k-means
km <- function(x,k){
  
  n <- nrow(x)
  
  
  # انتخاب k نقطه به صورت تصادفی به عنوان مرکز اولیه
  centers <- x[sample(1:n,k),]
  
  
  # برای نگهداری دسته قبلی
  clust0 <- rep(0,n)
  
  itr <- 0
  
  
  repeat{
    
    itr <- itr + 1
    
    
    # محاسبه فاصله نقاط از مراکز
    d <- dis(x,centers)
    
    
    # پیدا کردن نزدیک ترین مرکز برای هر نقطه
    clust <- rep(0,n)
    
    for(i in 1:n){
      clust[i] <- which.min(d[i,])
    }
    
    
    # اگر دسته ها تغییر نکردند الگوریتم متوقف شود
    if(sum(clust != clust0)==0){
      break
    }
    
    
    clust0 <- clust
    
    
    # به روز کردن مراکز
    for(i in 1:k){
      
      if(sum(clust==i)>0){
        
        centers[i,1] <- mean(x[clust==i,1])
        centers[i,2] <- mean(x[clust==i,2])
        
      }
      
    }
    
  }
  
  
  
  # تعداد اعضای هر دسته
  size <- rep(0,k)
  
  for(i in 1:k){
    size[i] <- sum(clust==i)
  }
  
  
  
  # ذخیره نقاط هر دسته
  clustdata <- list()
  
  for(i in 1:k){
    
    clustdata[[i]] <- x[clust==i,]
    
  }
  
  
  
  # رسم نمودار
  
  col <- rainbow(k)
  
  
  plot(x[,1],x[,2],
       col=col[clust],
       pch=19,
       xlab="latitude",
       ylab="longitude",
       main=paste("office location",
                  "\nn =",n,"   k =",k))
  
  
  # رسم مراکز
  
  points(centers[,1],
         centers[,2],
         col=col,
         pch=8,
         cex=2,
         lwd=3)
  
  
  # رسم خط بین نقاط و مرکز مربوطه
  
  for(i in 1:k){
    
    for(j in 1:nrow(clustdata[[i]])){
      
      lines(c(clustdata[[i]][j,1],
              centers[i,1]),
            c(clustdata[[i]][j,2],
              centers[i,2]),
            col=col[i])
      
    }
  }
  
  
  
  # خروجی تابع
  
  result <- list(
    centers=centers,
    size=size,
    clust=clust,
    clustdata=clustdata,
    itr=itr
  )
  
  
  return(result)
}

km(x,3)