library(diceR)
library(datasets)
library(ggplot2)
head(iris)

irisAlc <- iris[1:150, 1:4]

# Consensus clustering for multiple algorithms

CC1 <- consensus_cluster(irisAlc, nk = 3, reps = 10, algorithms = "ap",
                         progress = FALSE)
CC2 <- consensus_cluster(irisAlc, nk = 3, reps = 10, algorithms = "km",
                         progress = FALSE)

# Combine and return either matrices or classes
y1 <- consensus_combine(CC1, CC2, element = "matrix")
str(y1)
y2 <- consensus_combine(CC1, CC2, element = "class")
str(y2)

cmAlc <- consensus_matrix(y2)

set.seed(20)
irisCluster <- kmeans(cmAlc, 3)
irisCluster

table(irisCluster$cluster, iris$Species)

irisCluster$cluster <- as.factor(irisCluster$cluster)
ggplot(iris, aes(Petal.Length, Petal.Width, color = irisCluster$cluster))+ geom_point()
