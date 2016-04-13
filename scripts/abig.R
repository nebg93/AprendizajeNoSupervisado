#####################
#       A_BIG       #
#####################

a<-read.csv("a_big.csv", header = F)

dim(a)
summary(a)
table(a$V3)

a$V3 = a$V3 + 1
plot(a$V1, a$V2, col=a$V3, main = "a_big.csv")

#_______ KMEans _______

#K-Medias
a1 <- a
a1$V3 <- NULL
a1 <- my_own_km(a1, 3, NULL)
plot(a$V1, a$V2, col=a1$clusters, main = "a_big.csv")
points(a1$centers[,2], pch=19, col="black", cex=3)

#Observamos los valores de los centros

a1$centers[,2]

#________ Subset _______

p = vector(length = nrow(a))
p[a[,"V3"]==1] = 1/100000
p[a[,"V3"]==2] = 1/100000
p[a[,"V3"]==3] = 1/100000

a_sub <- sample(nrow(a), 1000, prob=p)
a_sub<-a[a_sub,]
plot(a_sub$V1, a_sub$V2, col=a_sub$V3)

###########################
#   A_BIG CODIGO LISTO    #
###########################