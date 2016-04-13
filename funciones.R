###############################
# Funciones para la tarea #03 #
# - K-means                   #
# - Elbow Method              #
# - Class_help (help, h, s)   #
# - Class_help2               #
# - My Own K-Means            #
# - Fix Matrix                #
###############################

#______ Ordenar Matriz ______

fix_matrix <- function(x){
  x1<-x
  for (i in 1:nrow(x)) {
    pos <- which.max(x[i,])
    x1[,i] <- x[,pos]
  }
  return(x1)
}

#______ Ordenar Matriz ______

#_______ K-means ________

k_means <- function(x, n){
  k_means<-kmeans(x, centers=n)
  if (ncol(x)==10) {
    x$V11 <- k_means$cluster
    plot(x, col=x$V11, pch=19, cex=0.5, main="K-Medias")
    return(x$V11)
  }
  if (ncol(x)==3) {
    x$V4 <- k_means$cluster
    scatterplot3d(x$V1, x$V2, x$V3, color=x$V4, main = "K-MEdias")
    points(k_means$centers, pch=19)
    return(x$V4)
  }
  if(ncol(x)==2){
    print("AQUI!")
    x$V3 <- k_means$cluster
    plot(x$V1, x$V2, col=x$V3, pch=19, cex=0.5, main="K-Medias")
    points(k_means$centers, pch=19)
    return(x$V3)
  }
}

#_______ K-means ________

#_______ My Own K-means ________

my_own_km <- function(x, n, centers){
  #Inicializando los centroides:
  if (is.null(centers)) {
    centers = c(1:3)
    dim(centers) <- c(3,1)
    for (i in 1:n) {
      centers[i] = runif(1, min = min(x$V1), max = max(x$V2))
    }
  }
  x<-as.matrix(x)  
  clusterh <- vector(1, mode="list")
  centerh <- vector(1, mode="list")
  
  for(i in 1:5) {
    dists <- euclid(x, centers)
    clusters <- apply(dists, 1, which.min)
    centers <- apply(x, 2, tapply, clusters, mean)
    clusterh <- clusters
    centerh <- centers
  }
  
  res <- list(clusters=clusterh, centers=centerh)
  return(res)
}

euclid <- function(points1, points2) {
  distanceMatrix <- matrix(NA, nrow=dim(points1)[1], ncol=dim(points2)[1])
  for(i in 1:nrow(points2)) {
    distanceMatrix[,i] <- sqrt(rowSums(t(t(points1)-points2[i,])^2))
  }
  distanceMatrix
}

#_______ My Own K-means ________

#_______ Elbow Method ________

elbow <- function(x){
  wss = list()
  
  for (i in 1:20) {
    wss[i]=kmeans(x, centers=i)$tot.withinss
  }
  plot(c(1:20), wss, type = "b",
       col=c(2,3), xlab = "Number of Clusters", 
       ylab="Withinss", main = "Elbow Method",
       pch=19)
}

#_______ Elbow Method ________


#_______ Class_help __________

class_help <- function(x, histo){
  for (i in 1:nrow(as.matrix(histo$counts))) {
    if (x < histo$breaks[i+1]) {
      return(i)
    }
  }
}

#_______ Class_help End __________


#_______ Class_help2 __________

class_h <- function(x, histo){
  l=1
  for (i in seq(from=3, to=nrow(as.matrix(histo$counts)), by=2)) {
    if (x < histo$breaks[i+1]) {
      return(l)
    }
    l=l+1
  }
}

#_______ Class_help2 End __________

#_______ Class_help3 __________

class_s <- function(x, histo){
  if (x < -4){return(1)}
  else if(x < -2){return(2)}
  else if(x < 0){return(3)}
  else if(x < 2){return(4)}
  else if(x < 4){return(5)}
  else {return(6)}
}

#_______ Class_help3 End __________