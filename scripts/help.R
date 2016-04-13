################
#     HELP     #
################

help<-read.csv("help.csv", header = F)

dim(help)
summary(help)

plot(help)
plot3d(help$V1, help$V2, help$V3, col=mycol)

#___ Acomodando la Clase _______

histo <- hist(help$V4)
res <- sapply(as.matrix(help$V4), class_help, histo=histo)
help1<-help
help1$V4<-res
plot3d(help$V1, help$V2, help$V3, col=help1$V4)
plot(help, col=help1$V4)


#____ KMEans K=2 _______

help1 <- help
help1$V4 <- NULL
help1$V4 <- k_means(help1, 2)
plot3d(help1$V1, help1$V2, help1$V3, col=help1$V4)

#____ KMEans K=11 _______

help1 <- help
help1$V4 <- NULL
help1$V4 <- k_means(help1, 11)
plot3d(help1$V1, help1$V2, help1$V3, col=help1$V4)

#___ HClust _______

help_hclust <- help

#Calculamos la matriz de Distancia

help_hclust <- as.matrix(help)
help_hclust <- dist(help_hclust)

#Single

h_clust <- hclust(help_hclust, method="single")
plot(h_clust)

pruning <- cutree(h_clust, k=11)
plot3d(help$V1, help$V2, help$V3, col=pruning)

dendrogram <- as.dendrogram(h_clust)
dendrogram <- cut(dendrogram, h=2.2)
plot(dendrogram$upper)


#Complete

h_clust <- hclust(help_hclust, method="complete")
#plot(h_clust)

pruning <- cutree(h_clust, k=11)
plot3d(help$V1, help$V2, help$V3, col=pruning)

dendrogram <- as.dendrogram(h_clust)
dendrogram <- cut(dendrogram, h=28)
plot(dendrogram$upper)

#Average

h_clust <- hclust(help_hclust, method="average")
plot(h_clust)

pruning <- cutree(h_clust, k=11)
plot3d(help$V1, help$V2, help$V3, col=pruning)

dendrogram <- as.dendrogram(h_clust)
dendrogram <- cut(dendrogram, h=17)
plot(dendrogram$upper)


############################
#    HELP CODIGO LISTO     #
############################