
##Testes Iris @alciomar

library(diceR)
library(ggplot2)
dat <- iris[, -5]
head(iris)
head(dat)



x <- consensus_cluster(dat, nk = 3, reps = 10, algorithms = c("km"),
                       progress = FALSE)



cspaData <- CSPA(x, k = 3)

mvData <- majority_voting(x,is.relabelled = FALSE)


ggplot(iris, aes(Petal.Length, Petal.Width, color = iris$Species)) + geom_point()


table(mvData, iris$Species)

table(cspaData, iris$Species)




irisClusterCSPA <- as.factor(cspaData)
irisClusterMV <- as.factor(mvData)

ggplot(iris, aes(Petal.Length, Petal.Width, color = irisClusterCSPA)) + geom_point()
ggplot(iris, aes(Petal.Length, Petal.Width, color = irisClusterMV)) + geom_point()

