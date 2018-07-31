# http://www.dcc.fc.up.pt/~ltorgo/DM1_1718/Rclustering.html
#############
##  Code of Slides:  Clustering in R
#############
#



####### Section:  Distance Functions

##
data(iris)
dm <- dist(iris[,-5])  # excluding the nominal target
as.matrix(dm)[1,4] # because dm is of class "dist"

##
library(cluster)
data(iris)
dm <- daisy(iris)
as.matrix(dm)[1,4] # because dm is of class "dist"



####### Section:  Partitional Methods

##
data(iris)

set.seed(50)
k3 <- kmeans(iris[,-5],centers=3,iter.max=200)


####### Section:  Clustering Validation

##
table(k3$cluster,iris$Species)
library(cluster)
s <- silhouette(k3$cluster,
                dist(iris[,-5]))

##
plot(s)


##
set.seed(50)
d <- dist(iris[,-5])
avgS <- c()
for(k in 2:6) {
  cl <- kmeans(iris[,-5],centers=k,iter.max=200)
  s <- silhouette(cl$cluster,d)
  avgS <- c(avgS,mean(s[,3]))
}

##
library(ggplot2)
ggplot(data.frame(nClus=2:6,Silh=avgS),
       aes(x=nClus,y=Silh)) + 
  geom_point(size=3,color="red") + geom_line() +
  xlab("Nr.Clusters") + ylab("Silh.Coef.")


####### Section:  Partitional Methods

##
library(cluster)

pc <- pam(iris[,-5],k=3)
table(pc$clustering,iris$Species)
s <- silhouette(pc$clustering,
                dist(iris[,-5]))

##
plot(s)
clusplot(pc)


##
library(cluster)
cl <- clara(iris[,-5],3)
table(cl$clustering,iris$Species)

##
clusplot(cl)
#install.packages("fpc")
##
library(fpc)
d <- scale(iris[,-5])
db <- dbscan(d,0.9)
db
table(db$cluster,iris$Species)

##
plot(db,d)

##
library(cluster)
f <- fanny(iris[,-5],3,metric='euclidean',stand=T)
head(f$membership)
table(f$clustering,iris$Species)

##
clusplot(f)


####### Section:  Hierarchical Methods

##
data(iris)
d <- dist(scale(iris[,-5]))
h <- hclust(d)

##
cls <- cutree(h,3)
table(cls,iris$Species)

##
plot(h)

##
library(cluster)
d <- dist(scale(iris[,-5]))
methds <- c('complete','single','average')
avgS <- matrix(NA,ncol=3,nrow=5,
               dimnames=list(2:6,methds))
for(k in 2:6) 
  for(m in seq_along(methds)) {
    h <- hclust(d,meth=methds[m])
    c <- cutree(h,k)
    s <- silhouette(c,d)
    avgS[k-1,m] <- mean(s[,3])
  }
library(reshape2)
dt <- melt(avgS)
colnames(dt) <- c("NClusts","Meth","AvgS")
library(ggplot2)
ggplot(dt,aes(x=NClusts,y=AvgS,color=Meth)) + 
  geom_line()

##
library(cluster)
di <- diana(iris[,-5],
            metric='euclidean',
            stand=TRUE)
table(cutree(di,3),iris$Species)

##
pltree(di)
