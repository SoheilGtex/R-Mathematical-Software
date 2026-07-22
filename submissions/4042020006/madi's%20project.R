km <- function(x, k){

n <- nrow(x)

centers <- matrix(0, k, 2)

a <- sample(1:n, k)

for(i in 1:k){
centers[i,] <- x[a[i],]
}

dis <- function(p, centers){

d <- numeric(k)

for(i in 1:k){

d[i] <- sqrt((p[1]-centers[i,1])^2 +
             (p[2]-centers[i,2])^2)

}

return(d)

}

clust0 <- rep(0,n)

itr <- 0

while(TRUE){

itr <- itr+1

clust <- numeric(n)

for(i in 1:n){

d <- dis(x[i,], centers)

m <- d[1]
id <- 1

for(j in 2:k){

if(d[j] < m){

m <- d[j]
id <- j

}

}

clust[i] <- id

}

if(all(clust==clust0)){

break

}

clust0 <- clust

for(i in 1:k){

sx <- 0
sy <- 0
cnt <- 0

for(j in 1:n){

if(clust[j]==i){

sx <- sx + x[j,1]
sy <- sy + x[j,2]
cnt <- cnt + 1

}

}

if(cnt>0){

centers[i,1] <- sx/cnt
centers[i,2] <- sy/cnt

}

}

}

size <- numeric(k)

for(i in 1:k){

c <- 0

for(j in 1:n){

if(clust[j]==i){

c <- c+1

}

}

size[i] <- c

}

clustdata <- vector("list",k)

for(i in 1:k){

temp <- matrix(ncol=2,nrow=0)

for(j in 1:n){

if(clust[j]==i){

temp <- rbind(temp,x[j,])

}

}

clustdata[[i]] <- temp

}

color <- c("red","blue","green","orange","purple","brown","pink","cyan","black","yellow")

plot(x[,1],x[,2],
col=color[clust],
pch=19,
xlab="latitude",
ylab="longitude",
main="office location")

mtext(paste("n =",n,"   k =",k))

for(i in 1:n){

segments(x[i,1],
x[i,2],
centers[clust[i],1],
centers[clust[i],2],
col=color[clust[i]])

}

for(i in 1:k){

points(centers[i,1],
centers[i,2],
pch=8,
cex=2,
col=color[i])

}

return(list(
centers=centers,
size=size,
clust=clust,
clustdata=clustdata,
itr=itr
))

}

set.seed(123)

x <- matrix(runif(200,0,100),ncol=2)

ans <- km(x,3)

ans$centers
ans$size
ans$clust
ans$clustdata
ans$itr
