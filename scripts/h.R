#####################
#         H         #
#####################

h<-read.csv("h.csv", header = F)

dim(h)
summary(h)

plot(h)
plot3d(h$V1, h$V2, h$V3, col = mycol)

#___ Acomodando la Clase _______

histo <- hist(h$V4)

res <- sapply(as.matrix(h$V4), class_h, histo=histo)
h$V4<-res
plot3d(h$V1, h$V2, h$V3, col=h$V4)
plot(h, col=h$V4)


#____ KMEans _______

h1<-h
h1$V4 <- NULL
h1$V4 <- k_means(h1, 5)

plot3d(h1$V1, h1$V2, h1$V3, col=h1$V4)

#___ HClust _______

help_hclust <- h

#Calculamos la matriz de Distancia

help_hclust <- as.matrix(h)
help_hclust <- dist(help_hclust)

#Single

h_clust <- hclust(help_hclust, method="single")
plot(h_clust)
pruning <- cutree(h_clust, k=5)
plot3d(h$V1, h$V2, h$V3, col=pruning)

dendrogram <- as.dendrogram(h_clust)
dendrogram <- cut(dendrogram, h=2.2)
plot(dendrogram$upper)


#Complete

h_clust <- hclust(help_hclust, method="complete")
plot(h_clust)

pruning <- cutree(h_clust, k=5)
plot3d(h$V1, h$V2, h$V3, col=pruning)

dendrogram <- as.dendrogram(h_clust)
dendrogram <- cut(dendrogram, h=18.5)
plot(dendrogram$upper)

#Average

h_clust <- hclust(help_hclust, method="average")
plot(h_clust)

pruning <- cutree(h_clust, k=5)
plot3d(h$V1, h$V2, h$V3, col=pruning)

dendrogram <- as.dendrogram(h_clust)
dendrogram <- cut(dendrogram, h=10.8)
plot(dendrogram$upper)

###########################
#     H CODIGO LISTO      #
###########################