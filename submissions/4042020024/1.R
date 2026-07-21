km <- function(x, k){

n <- nrow(x)

centers <- x[sample(1:n, k), ]

clust <- rep(0, n)
clust0 <- rep(-1, n)

itr <- 0

while(any(clust != clust0)){

itr <- itr + 1
clust0 <- clust

for(i in 1:n){

d <- rep(0, k)

for(j in 1:k){

d[j] <- sqrt(
(x[i,1]-centers[j,1])^2 +
(x[i,2]-centers[j,2])^2
)

}

clust[i] <- which.min(d)

}

for(j in 1:k){

if(sum(clust==j)>0){

centers[j,] <- c(
mean(x[clust==j,1]),
mean(x[clust==j,2])
)

}

}

}

clustdata <- vector("list",k)
size <- rep(0,k)

for(j in 1:k){

clustdata[[j]] <- x[clust==j,,drop=FALSE]
size[j] <- sum(clust==j)

}

cols <- c("red","blue","darkgreen","orange",
"purple","brown","black","pink")

plot(x,
type="n",
main="office location",
xlab="latitude",
ylab="longitude")

for(j in 1:k){

points(clustdata[[j]], pch=19, col=cols[j])

points(centers[j,1], centers[j,2], pch=8, cex=2, col=cols[j])

for(i in 1:n){

if(clust[i]==j){

segments(x[i,1],x[i,2],centers[j,1],centers[j,2],col=cols[j])

}

}

}

mtext(paste("n =",n," k =",k))

return(list(
clustdata=clustdata,
clust=clust,
centers=centers,
size=size,
Iteration=itr
))

}

set.seed(1)

x <- cbind(
matrix(rnorm(50,mean=33.8000,sd=.05),ncol=1),
matrix(rnorm(50,mean=46.4333,sd=.05),ncol=1)
)

km(x,3)
