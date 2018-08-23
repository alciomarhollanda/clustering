library(cluster)
data(iris)

############ Kmeans

set.seed(50)
k3 <- kmeans(iris[,-5],centers=3,iter.max=200)
table(k3$cluster,iris$Species)

sk3 <- silhouette(k3$cluster,
                dist(iris[,-5]))
plot(sk3)

########### PAM

pc <- pam(iris[,-5],k=3)
table(pc$clustering,iris$Species)
sPam <- silhouette(pc$clustering,
                dist(iris[,-5]))


plot(sPam)

########## CLARA

cl <- clara(iris[,-5],3)
table(cl$clustering,iris$Species)

sCl <- silhouette(cl$clustering,
                   dist(iris[,-5]))


plot(sCl)

##
clusplot(cl)

######### dbscan
library(fpc)
d <- scale(iris[,-5])
db <- dbscan(d,0.9)
db
table(db$cluster,iris$Species)


sDb <- silhouette(db$cluster,
                  dist(iris[,-5]))


plot(sDb)

plot(db,d)

############ fanny

f <- fanny(iris[,-5],3,metric='euclidean',stand=T)
head(f$membership)
table(f$clustering,iris$Species)

sF <- silhouette(f$cluster,
                  dist(iris[,-5]))
mean(sF[,3])

clusplot(f)

plot(sF)

################################

library(cluster)
d <- dist(scale(iris[,-5]))
methds <- c('kmeans','fanny','pam')
avgS <- matrix(NA,ncol=3,nrow=5,
               dimnames=list(2:6,methds))
for(k in 2:6) 
  for(m in seq_along(methds)) {
    if(m == 1){
      c <- kmeans(d,centers=k,iter.max=200)
    }else if( m == 2){
      c <- fanny(d,k,metric='euclidean',stand=T)
    }else if(m == 3){
      c <- pam(d,k=k)
    }
    s <- silhouette(c$cluster,d)
    avgS[k-1,m] <- mean(s[,3])
  }

library(reshape2)
dt <- melt(avgS)
colnames(dt) <- c("NClusts","Meth","AvgS")
library(ggplot2)
ggplot(dt,aes(x=NClusts,y=AvgS,color=Meth)) + 
  geom_line()


