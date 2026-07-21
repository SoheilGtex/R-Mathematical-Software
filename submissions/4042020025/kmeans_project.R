dis <- function(a, b) {
  sqrt((a[1] - b[1])^2 + (a[2] - b[2])^2)
}

km <- function(x, k) {
  n <- nrow(x)
  idx <- sample(n, k)
  centers <- matrix(0, k, 2)
  for (c in 1:k) {
    centers[c, 1] <- x[idx[c], 1]
    centers[c, 2] <- x[idx[c], 2]
  }
  
  clust <- rep(0, n)
  clust0 <- rep(-1, n)
  itr <- 0
  
  while (sum(clust != clust0) != 0) {
    clust0 <- clust
    itr <- itr + 1
    
    for (i in 1:n) {
      d <- rep(0, k)
      for (c in 1:k) {
        d[c] <- dis(x[i, ], centers[c, ])
      }
      clust[i] <- which.min(d)
    }
    
    for (c in 1:k) {
      members <- which(clust == c)
      if (length(members) > 0) {
        centers[c, 1] <- mean(x[members, 1])
        centers[c, 2] <- mean(x[members, 2])
      }
    }
  }
  
  size <- rep(0, k)
  clustdata <- list()
  for (c in 1:k) {
    members <- which(clust == c)
    size[c] <- length(members)
    clustdata[[c]] <- x[members, , drop = FALSE]
  }
  
  palette <- c("red", "blue", "green", "orange", "purple", "brown", "cyan", "magenta")
  
  ptColors <- rep("black", n)
  for (i in 1:n) {
    ptColors[i] <- palette[((clust[i] - 1) %% length(palette)) + 1]
  }
  
  plot(x[, 1], x[, 2], col = ptColors, pch = 16,
       xlab = "latitude", ylab = "longitude",
       main = "office location")
  mtext(paste("n =", n, ", k =", k))
  
  for (c in 1:k) {
    col_c <- palette[((c - 1) %% length(palette)) + 1]
    points(centers[c, 1], centers[c, 2], col = col_c, pch = 8, cex = 2)
    members <- which(clust == c)
    for (i in members) {
      segments(x[i, 1], x[i, 2], centers[c, 1], centers[c, 2], col = col_c)
    }
  }
  
  return(list(centers = centers, size = size, clust = clust, clustdata = clustdata, itr = itr))
}
