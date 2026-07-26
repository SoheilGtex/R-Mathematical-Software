# تابع محاسبه فاصله

distance <- function(x, center){
  
  n <- nrow(x)
  k <- nrow(center)
  
  d <- matrix(0,n,k)
  
  for(i in 1:n){
    
    for(j in 1:k){
      
      dx <- x[i,1]-center[j,1]
      dy <- x[i,2]-center[j,2]
      
      d[i,j] <- sqrt(dx*dx+dy*dy)
      
    }
    
  }
  
  return(d)
  
}



# تابع اصلی K-Means

km <- function(x,k){
  
  n <- nrow(x)
  
  center <- x[sample(1:n,k),]
  
  group.old <- rep(0,n)
  
  iteration <- 0
  
  while(TRUE){
    
    iteration <- iteration+1
    
    d <- distance(x,center)
    
    group <- rep(0,n)
    
    for(i in 1:n){
      
      group[i] <- which(d[i,]==min(d[i,]))[1]
      
    }
    
    if(sum(group!=group.old)==0){
      
      break
      
    }
    
    group.old <- group
    
    for(i in 1:k){
      
      if(sum(group==i)>0){
        
        center[i,1] <- mean(x[group==i,1])
        
        center[i,2] <- mean(x[group==i,2])
        
      }
      
    }
    
  }
  
  
  
  size <- rep(0,k)
  
  for(i in 1:k){
    
    size[i] <- sum(group==i)
    
  }
  
  
  
  cluster.data <- list()
  
  for(i in 1:k){
    
    cluster.data[[i]] <- x[group==i,]
    
  }
  
  
  
  color <- c("red","blue","green","orange","purple","brown")
  
  
  
  plot(x[,1],x[,2],
       col=color[group],
       pch=19,
       xlab="Latitude",
       ylab="Longitude",
       main=paste("Office Location",
                  "\nN =",n,"  k =",k))
  
  
  
  points(center[,1],
         center[,2],
         pch=8,
         cex=2,
         col=color[1:k])
  
  
  
  for(i in 1:k){
    
    for(j in 1:nrow(cluster.data[[i]])){
      
      segments(cluster.data[[i]][j,1],
               cluster.data[[i]][j,2],
               center[i,1],
               center[i,2],
               col=color[i])
      
    }
    
  }
  
  
  
  answer <- list(
    
    centers=center,
    
    clust=group,
    
    size=size,
    
    clustdata=cluster.data,
    
    itr=iteration
    
  )
  
  return(answer)
  
}



# اجرای برنامه

x <- cbind(
  matrix(rnorm(50,33.8,0.05),ncol=1),
  matrix(rnorm(50,46.4333,0.05),ncol=1)
)

result <- km(x,3)

result