km <- function(x,k){
  
  n <- nrow(x)
  
  dis <- function(a,b){
    sqrt((a[1]-b[1])^2 + (a[2]-b[2])^2)
  }
  
  centers <- x[sample(1:n,k),]
  
  clust <- rep(0,n)
  
  itr <- 0
  
  repeat{
    itr <- itr + 1
    
    oldclust <- clust
    
    for(i in 1:n){
      
      distance <- rep(0,k)
      
      for(j in 1:k){
        
        distance[j] <- dis(x[i,],centers[j,])}
      
      clust[i] <- which.min(distance)}
    
    newcenters <- matrix(0,k,2)
    
    for(j in 1:k){
      
      cluster_points <- x[clust==j,]
      
      if(nrow(cluster_points)>0){
        
        newcenters[j,1] <- mean(cluster_points[,1])
        
        newcenters[j,2] <- mean(cluster_points[,2])
        
      }
      else{
        
        newcenters[j,] <- centers[j,]
        
      }
      
    }
    
    
    centers <- newcenters
    
    if(all(oldclust==clust)){
      break
    }
    
  }
  
  size <- numeric(k)
  
  for(i in 1:k){
    
    size[i] <- sum(clust==i)
    
  }
  
  clustdata <- list()
  
  
  for(i in 1:k){
    
    clustdata[[i]] <- x[clust==i,]
    
  }
  
  plot(x,
       col=clust,
       pch=16,
       ylab="longitude",
       xlab="latitude",
       main=paste("office location\nn =",n," k =",k))
  
  
  points(centers,
         pch=8,
         cex=2)
  
  
  for(i in 1:n){
    
    segments(x[i,1],
             x[i,2],
             centers[clust[i],1],
             centers[clust[i],2])
    
  }
  
  return(list(
    clustdata=clustdata,
    clust=clust,
    centers=centers,
    size=size,
    itr=itr
  ))
  
}


x <- cbind(matrix(rnorm(50, mean = 33.8000, sd =
.05), ncol =1),matrix(rnorm(50, mean = 46.4333,
sd = .05), ncol = 1))
km(x,3)