#####################
#       GUESS       #
#####################

guess<-read.csv("guess.csv", header = F)

dim(guess)
summary(guess)
plot(guess$V1, guess$V2, main="guess.csv")

#____ Codo de Jambu ____

elbow(guess)

#_______ KMEans _______

guess1<-guess
guess1$V3 <- k_means(guess, 5)

#_______ HClust _______

guess_hclust <- guess

#Calculamos la matriz de Distancia

guess_hclust <- dist(as.matrix(guess))

#Single

h_clust <- hclust(guess_hclust, method="single")
plot(h_clust)

pruning <- cutree(h_clust, k=5)
plot(guess1$V1, guess1$V2, col=pruning, pch=19, cex=0.5)

dendrogram <- as.dendrogram(h_clust)
dendrogram <- cut(dendrogram, h=5)
plot(dendrogram$upper)


#Complete

h_clust <- hclust(guess_hclust, method="complete")
plot(h_clust)

pruning <- cutree(h_clust, k=5)
plot(guess1$V1, guess1$V2, col=pruning, pch=19, cex=0.5)

dendrogram <- as.dendrogram(h_clust)
dendrogram <- cut(dendrogram, h=65)
plot(dendrogram$upper)

#Average

h_clust <- hclust(guess_hclust, method="average")
plot(h_clust)

pruning <- cutree(h_clust, k=5)
plot(guess1$V1, guess1$V2, col=pruning, pch=19, cex=0.5)

dendrogram <- as.dendrogram(h_clust)
dendrogram <- cut(dendrogram, h=26)
plot(dendrogram$upper)


###########################
#   GUESS CODIGO LISTO    #
###########################