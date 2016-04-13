################
#     MOON     #
################

moon<-read.csv("moon.csv", header = F)

dim(moon)
summary(moon)
moon$V3 = moon$V3 + 1
plot(moon$V1, moon$V2, col=moon$V3, pch=19, cex=0.5)

#___ KMEans _______

m<-moon
m$V3 <- NULL
m$V3 <- k_means(m, 2)

#___ HClust _______

moon_hclust <- moon

#Calculamos la matriz de Distancia

moon_hclust <- as.matrix(moon)
moon_hclust <- dist(moon_hclust)

#Single

h_clust <- hclust(moon_hclust, method="single")
plot(h_clust)

pruning <- cutree(h_clust, k=2)
plot(moon1$V1, moon1$V2, col=pruning, pch=19, cex=0.5)

dendrogram <- as.dendrogram(h_clust)
dendrogram <- cut(dendrogram, h=5)
plot(dendrogram$upper)


#Complete

h_clust <- hclust(moon_hclust, method="complete")
plot(h_clust)

pruning <- cutree(h_clust, k=2)
plot(moon1$V1, moon1$V2, col=pruning, pch=19, cex=0.5)

dendrogram <- as.dendrogram(h_clust)
dendrogram <- cut(dendrogram, h=3)
plot(dendrogram$upper)

#Average

h_clust <- hclust(moon_hclust, method="average")
plot(h_clust)

pruning <- cutree(h_clust, k=2)
plot(moon1$V1, moon1$V2, col=pruning, pch=19, cex=0.5)

dendrogram <- as.dendrogram(h_clust)
dendrogram <- cut(dendrogram, h=1.4)
plot(dendrogram$upper)

############################
#    MOON CODIGO LISTO     #
############################