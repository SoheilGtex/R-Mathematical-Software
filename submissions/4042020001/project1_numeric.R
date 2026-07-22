# Project 1 - K-means algorithm
# x is a matrix with two columns: latitude and longitude
# k is the number of offices

km <- function(x, k) {

  n <- nrow(x)

  if (ncol(x) != 2) {
    stop("x must have exactly two columns")
  }

  if (k < 1 || k > n) {
    stop("k must be between 1 and the number of points")
  }

  # choose k points randomly as the first centers
  selected.points <- sample(1:n, k)
  centers <- x[selected.points, , drop = FALSE]

  clust <- rep(0, n)
  old.clust <- rep(-1, n)
  itr <- 0

  # continue until the cluster numbers do not change
  while (sum(clust != old.clust) != 0) {

    old.clust <- clust
    itr <- itr + 1

    # distance of every point from every center
    dis <- matrix(0, nrow = n, ncol = k)

    for (i in 1:n) {
      for (j in 1:k) {
        dis[i, j] <- sqrt(
          (x[i, 1] - centers[j, 1])^2 +
          (x[i, 2] - centers[j, 2])^2
        )
      }
    }

    # assign every point to its nearest center
    for (i in 1:n) {
      clust[i] <- which(dis[i, ] == min(dis[i, ]))[1]
    }

    # calculate the new centers
    new.centers <- matrix(0, nrow = k, ncol = 2)

    for (j in 1:k) {
      members <- which(clust == j)

      # if a cluster is empty, choose a random point for it
      if (length(members) == 0) {
        new.centers[j, ] <- x[sample(1:n, 1), ]
      } else {
        new.centers[j, 1] <- mean(x[members, 1])
        new.centers[j, 2] <- mean(x[members, 2])
      }
    }

    centers <- new.centers
  }

  # save the points of each cluster
  clustdata <- list()

  for (j in 1:k) {
    clustdata[[j]] <- x[which(clust == j), , drop = FALSE]
  }

  # number of points in each cluster
  size <- rep(0, k)

  for (j in 1:k) {
    size[j] <- sum(clust == j)
  }

  # colors used in the plot
  colors <- c("black", "red", "green", "blue",
              "orange", "purple", "brown", "pink")
  cluster.colors <- rep(colors, length.out = k)

  # make an empty plot first
  plot(
    x[, 1], x[, 2],
    type = "n",
    xlab = "Latitude",
    ylab = "Longitude",
    main = paste("Office Location\nN =", n, ", k =", k)
  )

  # draw points, centers and connecting lines
  for (j in 1:k) {
    members <- which(clust == j)

    points(
      x[members, 1],
      x[members, 2],
      pch = 8,
      col = cluster.colors[j]
    )

    for (i in members) {
      segments(
        x[i, 1], x[i, 2],
        centers[j, 1], centers[j, 2]
      )
    }

    points(
      centers[j, 1],
      centers[j, 2],
      pch = 19,
      cex = 1.4,
      col = cluster.colors[j]
    )
  }

  return(
    list(
      clustdata = clustdata,
      clust = clust,
      centers = centers,
      size = size,
      Iteration = itr
    )
  )
}


# Example of using the function:
#
# set.seed(10)
#
# x <- cbind(
#   matrix(rnorm(50, mean = 33.8000, sd = 0.05), ncol = 1),
#   matrix(rnorm(50, mean = 46.4333, sd = 0.05), ncol = 1)
# )
#
# result <- km(x, 3)
# result
