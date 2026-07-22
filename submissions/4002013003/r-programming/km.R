# ==============================================================================
#  Programming with R  |  K-means clustering written from scratch
#  Locating k branch offices so the total distance to customers is minimised
#  Student ID: 4002013003
# ------------------------------------------------------------------------------
#  km(x, k)
#    x : an n x 2 matrix, column 1 = latitude, column 2 = longitude
#    k : the number of offices (clusters) to open
#
#  Returns a list:
#    centers   : k x 2 matrix with the optimal office coordinates
#    size      : how many customer points each office covers
#    clust     : for every point, the index (1..k) of its nearest office
#    clustdata : a list of the point coordinates grouped per office
#    itr       : number of iterations the algorithm needed to converge
#  and draws the "office location" plot.
#  Uses base R only -- no third-party packages.
# ==============================================================================

km <- function(x, k) {

  n <- nrow(x)

  # ---- helper: Euclidean distance from one point to every current centre ----
  dis <- function(point, centers) {
    sqrt((point[1] - centers[, 1])^2 + (point[2] - centers[, 2])^2)
  }

  # ---- Step 1 : pick k random points as the starting centres ----------------
  start <- sample(1:n, k)
  centers <- matrix(x[start, ], ncol = 2)

  clust  <- rep(0, n)   # current assignment
  clust0 <- rep(-1, n)  # previous assignment (forces at least one pass)
  itr <- 0

  # ---- Steps 2-5 : repeat until the assignment stops changing ---------------
  while (sum(clust != clust0) != 0) {

    itr <- itr + 1
    clust0 <- clust          # remember the previous assignment

    # Steps 2 & 3 : assign every point to its nearest centre
    for (i in 1:n) {
      d <- dis(x[i, ], centers)
      clust[i] <- which(d == min(d))[1]
    }

    # Step 4 : recompute each centre as the mean of the points it now covers
    for (c in 1:k) {
      if (sum(clust == c) != 0) {
        members <- which(clust == c)
        centers[c, 1] <- mean(x[members, 1])
        centers[c, 2] <- mean(x[members, 2])
      }
    }
  }

  # ---- Build the remaining output pieces ------------------------------------
  size      <- rep(0, k)
  clustdata <- list()
  for (c in 1:k) {
    members        <- which(clust == c)
    size[c]        <- sum(clust == c)
    clustdata[[c]] <- matrix(x[members, ], ncol = 2)
  }

  # ---- Plot : one colour per office, points linked to their office ----------
  plot(x[, 1], x[, 2], col = clust, pch = 8,
       xlab = "latitude", ylab = "longitude",
       main = "office location",
       sub = paste("N=", n, ",k=", k))

  # a line segment from each point to the centre of its cluster
  for (i in 1:n) {
    segments(x[i, 1], x[i, 2],
             centers[clust[i], 1], centers[clust[i], 2],
             col = clust[i])
  }

  # the offices themselves: same colour, drawn larger
  points(centers[, 1], centers[, 2], col = 1:k, pch = 16, cex = 2)

  # ---- Return the result list -----------------------------------------------
  return(list(centers   = centers,
              size      = size,
              clust     = clust,
              clustdata = clustdata,
              itr       = itr))
}

# ==============================================================================
#  Demo (the example from the assignment sheet)
# ==============================================================================
# 50 customer points scattered around a single town centre:
x <- cbind(matrix(rnorm(50, mean = 33.8000, sd = 0.05), ncol = 1),
           matrix(rnorm(50, mean = 46.4333, sd = 0.05), ncol = 1))

result <- km(x, 3)
print(result)
