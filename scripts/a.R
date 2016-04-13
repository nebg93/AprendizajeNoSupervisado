#####################
#         A         #
#####################

a<-read.csv("a.csv", header = F)

dim(a)
summary(a)
table(a$V3)

a$V3 = a$V3 + 1
plot(a$V1, a$V2, col=a$V3, main = "a.csv")

#_______ KMEans _______

a1 <- a
a1$V3 <- NULL
a1$V3 <- k_means(a1, 3)


#Realizamos la matriz de confusion

tab<-table(as.matrix(a1$V3), a$V3)
tab<-fix_matrix(tab)
confusionMatrix(tab)

#______ H-Clust _______

a_hclust <- a
a_hclust$V3 <- NULL

#Calculamos la matriz de Distancia

a_hclust <- dist(as.matrix(a))

#Single

h_clust <- hclust(a_hclust, method="single")
plot(h_clust)

pruning <- cutree(h_clust, k=3)
plot(a$V1, a$V2, col=pruning, pch=19, cex=0.5)

dendrogram <- as.dendrogram(h_clust)
dendrogram <- cut(dendrogram, h=3.5)
plot(dendrogram$upper)

a1 <- a
a1$V3<-pruning


#Realizamos la matriz de confusion

confusionMatrix(a1$V3, a$V3)


#Complete

h_clust <- hclust(a_hclust, method="complete")
plot(h_clust)

pruning <- cutree(h_clust, k=3)
plot(a$V1, a$V2, col=pruning, pch=19, cex=0.5)

dendrogram <- as.dendrogram(h_clust)
dendrogram <- cut(dendrogram, h=40)
plot(dendrogram$upper)

a1 <- a
a1$V3<-pruning


#Realizamos la matriz de confusion

confusionMatrix(a1$V3, a$V3)

#Average

h_clust <- hclust(a_hclust, method="average")
plot(h_clust)

pruning <- cutree(h_clust, k=3)
plot(a$V1, a$V2, col=pruning, pch=19, cex=0.5)

dendrogram <- as.dendrogram(h_clust)
dendrogram <- cut(dendrogram, h=15)
plot(dendrogram$upper)

a1 <- a
a1$V3<-pruning


#Realizamos la matriz de confusion

confusionMatrix(a1$V3, a$V3)

###########################
#     A CODIGO LISTO      #
###########################