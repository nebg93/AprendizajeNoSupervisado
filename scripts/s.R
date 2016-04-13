#####################
#         S         #
#####################

s<-read.csv("s.csv", header = F)

dim(s)
summary(s)

plot(s)
plot3d(s$V1, s$V2, s$V3, col = mycol)

#___ Acomodando la Clase _______

histo <- hist(s$V4)

res <- sapply(as.matrix(s$V4), class_help3, histo=histo)
s1<-s
s1$V4<-res
plot3d(s$V1, s$V2, s$V3, col=s1$V4)
plot(s, col=s1$V4)


#____ KMEans _______

s1 <- s
s1$V4 <- NULL
k_means<-kmeans(s1, centers=11)
s1$V4 <- k_means$cluster
plot3d(s1$V1, s1$V2, s1$V3, col=s1$V4)

#___ HClust _______

help_hclust <- s
help_hclust$V4 <- NULL

#Calculamos la matriz de Distancia

help_hclust <- as.matrix(help_hclust)
help_hclust <- dist(help_hclust)

#Single

h_clust <- hclust(help_hclust, method="single")
plot(h_clust)
pruning <- cutree(h_clust, k=11)
plot3d(h$V1, h$V2, h$V3, col=pruning)

dendrogram <- as.dendrogram(h_clust)
dendrogram <- cut(dendrogram, h=2.2)
plot(dendrogram$upper)


#Complete

h_clust <- hclust(help_hclust, method="complete")
plot(h_clust)

pruning <- cutree(h_clust, k=11)
plot3d(h$V1, h$V2, h$V3, col=pruning)

dendrogram <- as.dendrogram(h_clust)
dendrogram <- cut(dendrogram, h=18.5)
plot(dendrogram$upper)

#Average

h_clust <- hclust(help_hclust, method="average")
plot(h_clust)

pruning <- cutree(h_clust, k=11)
plot3d(h$V1, h$V2, h$V3, col=pruning)

dendrogram <- as.dendrogram(h_clust)
dendrogram <- cut(dendrogram, h=10.8)
plot(dendrogram$upper)

###########################
#     S CODIGO LISTO      #
###########################