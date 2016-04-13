################
#     GOOD     #
################

good<-read.csv("good_luck.csv", header = F)

dim(good)
summary(good)
good$V11 = good$V11 + 1
plot(good, col=good$V11)


#K-Medias
g <- good
g$V11 <- NULL
g$V11 <- k_means(g, 2)

#______ H-Clust _______

g_hclust <- good
g_hclust$V11 <- NULL

#Calculamos la matriz de Distancia

g_hclust <- dist(as.matrix(g_hclust))

#Single

h_clust <- hclust(g_hclust, method="single")
plot(h_clust)

pruning <- cutree(h_clust, k=2)
plot(g, col=pruning, pch=19, cex=0.5)

dendrogram <- as.dendrogram(h_clust)
dendrogram <- cut(dendrogram, h=3.5)
plot(dendrogram$upper)

g <- good
g$V11<-pruning


#Realizamos la matriz de confusion

tab <- table(g$V11, good$V11)
tab <- fix_matrix(tab)
confusionMatrix(tab)


#Complete

h_clust <- hclust(g_hclust, method="complete")
plot(h_clust)

pruning <- cutree(h_clust, k=2)
plot(g, col=pruning, pch=19, cex=0.5)

dendrogram <- as.dendrogram(h_clust)
dendrogram <- cut(dendrogram, h=3.5)
plot(dendrogram$upper)

g <- good
g$V11<-pruning


#Realizamos la matriz de confusion

tab <- table(g$V11, good$V11)
tab <- fix_matrix(tab)
confusionMatrix(tab)

#Average

h_clust <- hclust(g_hclust, method="average")
plot(h_clust)

pruning <- cutree(h_clust, k=2)
plot(g, col=pruning, pch=19, cex=0.5)

dendrogram <- as.dendrogram(h_clust)
dendrogram <- cut(dendrogram, h=3.5)
plot(dendrogram$upper)

g <- good
g$V11<-pruning


#Realizamos la matriz de confusion

tab <- table(g$V11, good$V11)
tab <- fix_matrix(tab)
confusionMatrix(tab)

############################
#    GOOD CODIGO LISTO     #
############################